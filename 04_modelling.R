library(tidyverse)
library(ggplot2)
library(themis)
library(randomForest)
library(caret)
library(doParallel)
library(foreach)


#####Brisbane Model#######

#pre-processing

modelling_b <- readRDS("Modelling_input/pre-processed-b.RDS")
modelling_RF_b <- modelling_b %>%
  ungroup() %>%
  select(commuters, distance, mode, Street_length, 
         EDU, REC, MED, RET, Bld_area, 
         noise_cat_0, noise_cat_1, noise_cat_2, 
         noise_cat_3, noise_cat_4) %>%
  mutate(
    mode = as.factor(mode)
  ) %>%
  uncount(commuters)

#data splitting

set.seed(222)

#create stratified train and test data
train.index <- createDataPartition(modelling_RF_b$mode, p = .7, list = FALSE)
train <- modelling_RF_b[ train.index,]
test  <- modelling_RF_b[-train.index,]

table(train$mode) 
table(test$mode)

###very unbalanced
# 92% car | 1.6 % bicycle | 5% walk

#re-balancing training set with undersampling
#scale both the training and test set with the same scaling parameters

train_mean <- colMeans(train[,-2])
train_sd <- apply(train[,-2], 2, sd)

train_scaled <- scale(train[,-2], center = train_mean, scale = train_sd)
train_scaled <- cbind(train[,2],as.data.frame(train_scaled))

test_scaled <- scale(test[,-2], center = train_mean, scale = train_sd)
test_scaled <- cbind(test[,2],as.data.frame(test_scaled))

#Nearmiss algorithm to undersample with themis

train_balanced <- nearmiss(train_scaled, var = "mode", under_ratio = 1)
saveRDS(train_balanced, "Model_output/train_balanced_b.rds")

#Tune and train the model

# hyperparameters
mtry <- expand.grid(.mtry=c(2:10))
ntrees <- expand.grid(.tree=c(100, 200, 500, 1000))

num_cores <- 34  # Set the number of CPU cores to use

# Register parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl) # Ensure that 'doParallel' is registered and parallel processing is allowed

## Tune training model

trainControl <- trainControl(
  method = "oob" # good for RF
)

tuned_models <- NULL
i=0

for(ntree in ntrees$.tree) {
  i = i+1
  tuned_model <- caret::train(
      mode ~ .,
      data = train_balanced,
      method = "rf",
      tuneGrid = mtry,
      trControl = trainControl,
      allowParallel = TRUE,
      ntree = ntree)
  
  tuned_model_results <- tuned_model$results
  tuned_model_results$ntree <- ntree
  tuned_models <- rbind(tuned_models, tuned_model_results)
  print(ntree)
}  

# visualise performance metrics

training_performance <- tuned_models %>%
  pivot_longer(c("Accuracy", "Kappa"), names_to = "Metric", values_to = "Value")

saveRDS(training_performance, "Model_output/training_performance_b.rds")
#training_performance <- readRDS("Model_output/training_performance_b.rds")

p1 <- ggplot(training_performance[training_performance$Metric == "Accuracy",],aes(x = mtry, y = Value, color = as.factor(ntree), group = as.factor(ntree)))+
  geom_line() +
  labs(color = "Number of Trees", y = "Accuracy")+
  theme_minimal()

### Test prediction metrics

rf_test_eval <- NULL
rf_test_eval_class <- NULL
mtry_values <- seq(4, 8)
i=0
# Loop through different mtry values
for (mtry_value in mtry_values) {
  i=i+1
  # Build the Random Forest model with the specified mtry value
  model <- randomForest(mode ~ ., data = train_balanced, mtry = mtry_value, ntree = 500)  # Adjust ntree as needed
  predictions <- predict(model, newdata = test_scaled)
  test_metrics <- confusionMatrix(predictions, reference = test_scaled$mode)
  class_metrics <- data.frame(test_metrics$byClass) %>%
                      mutate(
                        class = rownames(.),
                        mtry_value
                        ) %>%
                    remove_rownames 
  
  overall_metrics <- data.frame(as.list(test_metrics$overall))
  overall_metrics$mtry <- mtry_value
  # Store the model in the list along with its mtry value
  rf_test_eval <- rbind(rf_test_eval, overall_metrics)
  rf_test_eval_class <- rbind(rf_test_eval_class, class_metrics)
  print(mtry_value)
}

saveRDS(rf_test_eval_class, "Model_output/rf_test_eval_class_b.rds")
saveRDS(rf_test_eval, "Model_output/rf_test_eval_b.rds")

###visualise results 

rf_test_eval_b <- readRDS("Model_output/rf_test_eval_b.rds")
rf_test_eval_class_b <- readRDS("Model_output/rf_test_eval_class_b.rds")

p2 <- ggplot(rf_test_eval_b, aes(x = mtry, y = Accuracy))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = AccuracyLower, ymax = AccuracyUpper), width=0.1)+
  theme_minimal()

p3 <- ggplot(rf_test_eval_class_b, aes(x = mtry_value, y = Balanced.Accuracy, color = class))+
  geom_point()+
  geom_line() +
  labs(x = "mtry", y = "Accuracy\n(balanced)", color = "Mode of travel")+
  theme_minimal() +
  theme(legend.position = "right")

arranged <- ggarrange(p1,
                      ggarrange(p3,p2, ncol = 2, labels = c("B", "C"), widths = c(6, 3)),
                      nrow = 2,
                      labels = "A")

png("img/Brisbane_tuning.png",units="cm", width=15, height=10, res=300)
arranged
dev.off()


###best model

final_model <- randomForest(mode ~ ., data = train_balanced, mtry = 4, ntree = 500)  # Adjust ntree as needed
final_prediction <- predict(final_model, newdata = test_scaled)

saveRDS(final_model, "Model_output/final_model_b.rds")
saveRDS(final_prediction, "Model_output/final_prediction_b.rds")

final_prediction <- readRDS("Model_output/final_prediction_b.rds")

cm_b <- confusionMatrix(table(final_prediction,test_scaled$mode))

table <- as.data.frame(cm_b$table)
cm_p1_b <- ggplot(table, aes(x = final_prediction, y = Var2, fill = Freq/1000)) +
  geom_tile() + 
  scale_fill_viridis_c()+
  labs(x = "Predicted class", y = "True class", fill = "Trip Count\n(thousands)")+
  theme_minimal()

saveRDS(cm_p1_b, "Model_output/cm_p1_b.rds")

cm_byclass_b <- as.data.frame(cm_b$byClass) %>%
  mutate(
    mode = rownames(as.data.frame(cm_b$byClass))
  ) %>%
  pivot_longer(-mode, names_to = "Metrics", values_to =  "Value")

write.csv(cm_byclass_b, "Model_output/cm_byclass_b.csv", row.names = F)


####London Model#####

#pre-processing

modelling_l <- readRDS("Modelling_input/pre-processed-l.RDS")
modelling_RF_l <- modelling_l %>%
  ungroup() %>%
  select(commuters, distance, mode, Street_length, 
         EDU, REC, MED, RET, Bld_area, 
         noise_cat_0, noise_cat_1, noise_cat_2, 
         noise_cat_3, noise_cat_4, noise_cat_5) %>%
  mutate(
    mode = as.factor(mode)
  ) %>%
  uncount(commuters)

#data splitting

set.seed(222)

#create stratified train and test data
train.index <- createDataPartition(modelling_RF_l$mode, p = .7, list = FALSE)
train <- modelling_RF_l[ train.index,]
test  <- modelling_RF_l[-train.index,]

table(train$mode) 
table(test$mode)

###very unbalanced
# 92% car | 1.6 % bicycle | 5% walk

#re-balancing training set with undersampling
#scale both the training and test set with the same scaling parameters

train_mean <- colMeans(train[,-2])
train_sd <- apply(train[,-2], 2, sd)

train_scaled <- scale(train[,-2], center = train_mean, scale = train_sd)
train_scaled <- cbind(train[,2],as.data.frame(train_scaled))

test_scaled <- scale(test[,-2], center = train_mean, scale = train_sd)
test_scaled <- cbind(test[,2],as.data.frame(test_scaled))

#Nearmiss algorithm to undersample with themis

train_balanced <- nearmiss(train_scaled, var = "mode", under_ratio = 1)
saveRDS(train_balanced, "Model_output/train_balanced_l.rds")

#Tune and train the model

# hyperparameters
mtry <- expand.grid(.mtry=c(2:10))
ntrees <- expand.grid(.tree=c(100, 200, 500, 1000))

num_cores <- 34  # Set the number of CPU cores to use

# Register parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl) # Ensure that 'doParallel' is registered and parallel processing is allowed

#set.seed(222)  # for reproducibility

# 
# trainControl <- trainControl(
#   method = "cv",
#   number = 10,
#   returnResamp="all"
# )

## Tune training model

trainControl <- trainControl(
  method = "oob" # good for RF
)

tuned_models <- NULL
i=0

for(ntree in ntrees$.tree) {
  i = i+1
  tuned_model <- caret::train(
    mode ~ .,
    data = train_balanced,
    method = "rf",
    tuneGrid = mtry,
    trControl = trainControl,
    allowParallel = TRUE,
    ntree = ntree)
  
  tuned_model_results <- tuned_model$results
  tuned_model_results$ntree <- ntree
  tuned_models <- rbind(tuned_models, tuned_model_results)
  print(ntree)
}  

# visualise performance metrics

training_performance <- tuned_models %>%
  pivot_longer(c("Accuracy", "Kappa"), names_to = "Metric", values_to = "Value")

saveRDS(training_performance, "Model_output/training_performance_l.rds")
training_performance <- readRDS("Model_output/training_performance_l.rds")

p1 <- ggplot(training_performance[training_performance$Metric == "Accuracy",],aes(x = mtry, y = Value, color = as.factor(ntree), group = as.factor(ntree)))+
  geom_line() +
  labs(color = "Number of Trees", y = "Accuracy")+
  theme_minimal()

### Test prediction metrics

rf_test_eval <- NULL
rf_test_eval_class <- NULL
mtry_values <- c(4, 5, 6)
i=0
# Loop through different mtry values
for (mtry_value in mtry_values) {
  i=i+1
  # Build the Random Forest model with the specified mtry value
  model <- randomForest(mode ~ ., data = train_balanced, mtry = mtry_value, ntree = 500)  # Adjust ntree as needed
  predictions <- predict(model, newdata = test_scaled)
  test_metrics <- confusionMatrix(predictions, reference = test_scaled$mode)
  class_metrics <- data.frame(test_metrics$byClass) %>%
    mutate(
      class = rownames(.),
      mtry_value
    ) %>%
    remove_rownames 
  
  overall_metrics <- data.frame(as.list(test_metrics$overall))
  overall_metrics$mtry <- mtry_value
  # Store the model in the list along with its mtry value
  rf_test_eval <- rbind(rf_test_eval, overall_metrics)
  rf_test_eval_class <- rbind(rf_test_eval_class, class_metrics)
  remove(model, predictions)
  print(mtry_value)
}

saveRDS(rf_test_eval_class, "Model_output/rf_test_eval_class_l.rds")
saveRDS(rf_test_eval, "Model_output/rf_test_eval_l.rds")
rf_test_eval_l <- readRDS("Model_output/rf_test_eval_l.rds")
rf_test_eval_class_l <- readRDS("Model_output/rf_test_eval_class_l.rds")

p2 <- ggplot(rf_test_eval_l, aes(x = mtry, y = Accuracy))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = AccuracyLower, ymax = AccuracyUpper), width=0.1)+
  theme_minimal()

p3 <- ggplot(rf_test_eval_class_l, aes(x = mtry_value, y = Balanced.Accuracy, color = class))+
  geom_point()+
  geom_line() +
  labs(x = "mtry", y = "Accuracy\n(balanced)", color = "Mode of travel")+
  theme_minimal() +
  theme(legend.position = "right")

arranged <- ggarrange(p1,
          ggarrange(p3,p2, ncol = 2, labels = c("B", "C"), widths = c(6, 3)),
          nrow = 2,
          labels = "A")

png("img/London_tuning.png",units="cm", width=15, height=10, res=300)
arranged
dev.off()


###best model

final_model <- randomForest(mode ~ ., data = train_balanced, mtry = 4, ntree = 500)  # Adjust ntree as needed
final_prediction <- predict(final_model, newdata = test_scaled)

saveRDS(final_model, "Model_output/final_model_l.rds")
saveRDS(final_prediction, "Model_output/final_prediction_l.rds")

#final_prediction <- readRDS("Model_output/final_prediction_l.rds")

cm_l <- confusionMatrix(table(final_prediction,test_scaled$mode))

table <- as.data.frame(cm_l$table)
cm_p1_l <- ggplot(table, aes(x = final_prediction, y = Var2, fill = Freq/1000)) +
  geom_tile() + 
  scale_fill_viridis_c()+
  labs(x = "Predicted class", y = "True class", fill = "Trip Count\n(thousands)")+
  theme_minimal()

saveRDS(cm_p1_l, "Model_output/cm_p1_l.rds")

cm_byclass_l <- as.data.frame(cm_l$byClass) %>%
  mutate(
    mode = rownames(as.data.frame(cm_l$byClass))
  ) %>%
  pivot_longer(-mode, names_to = "Metrics", values_to =  "Value")

write.csv(cm_byclass_l, "Model_output/cm_byclass_l.csv", row.names = F)


####Testing performances for GL and B

cm_p1_b <- readRDS("Model_output/cm_p1_b.rds")
cm_p1_l <- readRDS("Model_output/cm_p1_l.rds")

png("img/confusion_L_B.png",units="cm", width=30, height=10, res=300)
ggarrange(cm_p1_l,cm_p1_b, ncol = 2, labels = c("A", "B"))
dev.off()


