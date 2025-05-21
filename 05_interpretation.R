library(pdp)
library(randomForest)
library(cowplot)
library(gridExtra)
library(grid)
library(ggpubr)
library(tidyverse)
library(caret)



#### London Model ####


final_model <- readRDS("Model_output/final_model_l.rds")
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

train_balanced <- readRDS("Model_output/train_balanced_l.rds")
set.seed(222)

#create stratified train and test data
train.index <- createDataPartition(modelling_RF_l$mode, p = .7, list = FALSE)
train <- modelling_RF_l[ train.index,]

train_mean <- colMeans(train[,-2])
train_sd <- apply(train[,-2], 2, sd)

#visualise variable importance

imp <- as.data.frame(varImpPlot(final_model,
                       sort = T,
                       n.var = 13,
                       main = "Variable Importance"))

imp$var <- rownames(imp)
rownames(imp) <- seq(1:nrow(imp))
imp <- imp %>%
  arrange(MeanDecreaseGini) 

l <- rev(imp$var)
imp <- imp %>%
  mutate(
    var = factor(var,
                 levels = l,
                 labels = c("Distance", "55dB <= noise < 60dB", "Bld area", 
                            "noise < 55dB", "70dB <= noise < 75dB", "Retail", "Recreation",
                            "60dB <= noise < 65dB", "Street length", "Education", "Health", 
                            "65dB <= noise < 70dB", "noise >= 75dB"))
  )

imp_l <- ggplot(imp, aes(x = MeanDecreaseGini, y = var))+
  geom_point() +
  scale_y_discrete(limits = rev) +
  theme_minimal()+
  theme(axis.title.y = element_blank())

saveRDS(imp_l, "Model_output/imp_l.rds")


#generate partial dependence measures

pd_WALK <- NULL

for (v in names(modelling_RF_l[,-2])) {
  p <- pdp::partial(object = final_model, v, which.class = "WALK", grid.resolution = 10,
               plot = F, train = train_balanced)
  p <- p %>% 
    rename(
      var_value = !!v
    ) %>%
    mutate(
      var = v
    )
  pd_WALK <- rbind(pd_WALK, p)
  remove(p)
  gc()
  print(v)
}

saveRDS(pd_WALK, "Model_output/pd_WALK_l.rds")


pd_CAR <- NULL

for (v in names(modelling_RF_l[,-2])) {
  p <- pdp::partial(object = final_model, v, which.class = "CAR", grid.resolution = 10,
                    plot = F, train = train_balanced)
  p <- p %>% 
    rename(
      var_value = !!v
    ) %>%
    mutate(
      var = v
    )
  pd_CAR <- rbind(pd_CAR, p)
  remove(p)
  gc()
  print(v)
}

saveRDS(pd_CAR, "Model_output/pd_CAR_l.rds")

pd_BIKE <- NULL

for (v in names(modelling_RF_l[,-2])) {
  p <- pdp::partial(object = final_model, v, which.class = "BICYCLE", grid.resolution = 10,
                    plot = F, train = train_balanced)
  p <- p %>% 
    rename(
      var_value = !!v
    ) %>%
    mutate(
      var = v
    )
  pd_BIKE <- rbind(pd_BIKE, p)
  remove(p)
  gc()
  print(v)
}

saveRDS(pd_BIKE, "Model_output/pd_BIKE_l.rds")

#visualise partial plots
pd_WALK <- readRDS("Model_output/pd_WALK_l.rds")
pd_BIKE <- readRDS("Model_output/pd_BIKE_l.rds")
pd_CAR  <- readRDS("Model_output/pd_CAR_l.rds")
pd_WALK$mode <- "WALK"
pd_BIKE$mode <- "BICYCLE"
pd_CAR$mode <- "CAR"

pd_l <- rbind(pd_WALK, pd_BIKE, pd_CAR)
remove(pd_WALK, pd_BIKE, pd_CAR)
saveRDS(pd_l, "Model_output/pd_l.rds")

####reverse scaled x axis to original values

pd_reverse_scale_l <- pd_l %>%
  mutate(
    mean = rep(train_mean, each = 10, times = 3),
    sd = rep(train_sd, each = 10, times = 3),
    var_reversed = var_value *  sd + mean
  )

pd_reverse_scale_l <- pd_reverse_scale_l %>%
  rename(
    Response = yhat
  ) %>%
  mutate(
    var_reversed = ifelse(var == "Bld_area", var_reversed/1000, var_reversed),
    var_name = case_when(
      var == "distance" ~ "Distance (m)",
      var == "Street_length" ~ "Street Length (avg m)",
      var == "EDU" ~ "Education (avg count)",
      var == "REC" ~ "Recreation (avg count)",
      var == "MED" ~ "Health (avg count)",
      var == "RET" ~ "Retail (avg count)",
      var == "Bld_area" ~ "Building Area (hectars)",
      var == "noise_cat_0" ~ "Trip share with\nnoise < 55dB",
      var == "noise_cat_1" ~ "Trip share with\n55dB <= noise < 60dB",
      var == "noise_cat_2" ~ "Trip share with\n60dB <= noise < 65dB",
      var == "noise_cat_3" ~ "Trip share with\n65dB <= noise < 70dB",
      var == "noise_cat_4" ~ "Trip share with\n70dB <= noise < 75dB",
      var == "noise_cat_5" ~ "Trip share with\nnoise >= 75dB"
    )
  )


ggplot(data = pd_reverse_scale_l)+
  geom_line(aes(x = var_reversed, y = Response, colour = mode, group = mode))+
  facet_wrap(~var_name, scales = "free")+
  labs(colour = "Travel Mode")+
  theme_minimal()+
  theme(axis.title.x = element_blank())

###Reverse scaled value of the train data to create histograms and show the variable 
#distribution in the training set


means <- data.frame(as.list(train_mean)) %>%
  pivot_longer(cols = everything(), names_to = "var", values_to = "mean")

sd <- data.frame(as.list(train_sd)) %>%
  pivot_longer(cols = everything(), names_to = "var", values_to = "sd")

train_reversed_l <- train_balanced %>% 
  pivot_longer(- mode, names_to = "var", values_to = "var_value") %>%
  inner_join(means, by = c("var" = "var")) %>%
  inner_join(sd, by = c("var" = "var")) %>%
  mutate(
    var_reversed = var_value *  sd + mean,
    var_reversed = ifelse(var == "Bld_area", var_reversed/1000, var_reversed)
  ) 

###count the frequency using the same intervals as in the partial dependence plot

reversed_l <- NULL
var_names <- unique(train_reversed_l$var)

for (v in var_names) {
  breaks <- unique(pd_reverse_scale_l[pd_reverse_scale_l$var == v,]$var_reversed)
  x = hist(train_reversed_l[train_reversed_l$var == v,]$var_reversed, breaks = breaks)
  b = x$breaks
  c = x$counts
  x = data.frame(var_value = b[-length(b)], Response = c, var = v)
  reversed_l = rbind(reversed_l, x)
}

reversed_l <- reversed_l %>%
  mutate(
    var_name = case_when(
      var == "distance" ~ "Distance (m)",
      var == "Street_length" ~ "Street Length (avg m)",
      var == "EDU" ~ "Education (avg count)",
      var == "REC" ~ "Recreation (avg count)",
      var == "MED" ~ "Health (avg count)",
      var == "RET" ~ "Retail (avg count)",
      var == "Bld_area" ~ "Building Area (hectars)",
      var == "noise_cat_0" ~ "Trip share with\nnoise < 55dB",
      var == "noise_cat_1" ~ "Trip share with\n55dB <= noise < 60dB",
      var == "noise_cat_2" ~ "Trip share with\n60dB <= noise < 65dB",
      var == "noise_cat_3" ~ "Trip share with\n65dB <= noise < 70dB",
      var == "noise_cat_4" ~ "Trip share with\n70dB <= noise < 75dB",
      var == "noise_cat_5" ~ "Trip share with\nnoise >= 75dB"
    )
  )


###generate plots combining the pdp and hist for each variable

plots <- list()
var_names <- unique(reversed_l$var_name)
i=0

for (v in var_names){
  
  i=i+1
  
  pmain <- ggplot(data = pd_reverse_scale_l[pd_reverse_scale_l$var_name == v,], aes(x = var_reversed, y = Response, colour = mode, group = mode)) + 
    geom_line() +
    theme_minimal() +
    labs(x = v) +
    theme(axis.title.y = element_blank(), legend.position = "none", text = element_text(size = 10))
  
  xbox <- axis_canvas(pmain, axis = "x", coord_flip = F) +
    geom_bar(data = reversed_l[reversed_l$var_name == v,], aes(x = var_value, y = Response), stat = "identity", alpha = 0.6, just = 0) 
  
  p1 <- insert_xaxis_grob(pmain, xbox, grid::unit(0.5, "in"), position = "top")
  
  plots[[i]] <- p1
  
}


ggdraw(plots[[7]])

plot1_legend <- ggplot(data = pd_reverse_scale_l[pd_reverse_scale_l$var_name == v,], aes(x = var_reversed, y = Response, colour = mode, group = mode)) + 
  geom_line() +
  theme_minimal() +
  labs(x = v, color = "Travel Mode") +
  theme(axis.title.y = element_blank(), legend.position = "right")

# function to extract legend from plot 
get_only_legend <- function(plot) { 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend) 
} 

# extract legend from plot1 using above function 
legend <- get_only_legend(plot1_legend)    

prow <- grid.arrange(plots[[1]],
                     plots[[2]],
                     plots[[3]],
                     plots[[4]],
                     plots[[5]],
                     plots[[6]],
                     plots[[7]],
                     plots[[8]],
                     plots[[9]],
                     plots[[10]],
                     plots[[11]],
                     plots[[12]],
                     plots[[13]],
                     ncol = 3, legend
)

png("img/London_m_h.png",units="cm", width=23, height=25, res=300)
annotate_figure(prow, 
                left = text_grob("Partial Dependence", 
                                 color = "black", 
                                 rot = 90, 
                                 size = 12,
                                 family = "arial"))
dev.off()


#### Brisbane Model ####

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

final_model <- readRDS("Model_output/final_model_b.rds")
train_balanced <- readRDS("Model_output/train_balanced_b.rds")

#visualise variable importance

imp <- as.data.frame(varImpPlot(final_model,
                                sort = T,
                                n.var = 13,
                                main = "Variable Importance"))

imp$var <- rownames(imp)
rownames(imp) <- seq(1:nrow(imp))
imp <- imp %>%
  arrange(MeanDecreaseGini) 

l <- rev(imp$var)
imp <- imp %>%
  mutate(
    var = factor(var,
                 levels = l,
                 labels = c("Distance", "Retail", "Street length", "Bld area",
                            "Recreation", "noise < 58dB", "noise >= 73dB",
                            "Health", "68dB <= noise < 73dB", "Education",
                            "63dB <= noise < 68dB", "58dB <= noise < 63dB"))
  )

imp_b <- ggplot(imp, aes(x = MeanDecreaseGini, y = var))+
  geom_point() +
  scale_y_discrete(limits = rev) +
  theme_minimal()+
  theme(axis.title.y = element_blank())

saveRDS(imp_b, "Model_output/imp_b.rds")


#generate partial dependence measures

pd_b <- NULL

for (v in names(modelling_RF_b[,-2])) {
  p <- pdp::partial(object = final_model, v, which.class = "WALK", grid.resolution = 10,
                    plot = F, train = train_balanced)
  p <- p %>% 
    rename(
      var_value = !!v
    ) %>%
    mutate(
      var = v
    )
  pd_b <- rbind(pd_b, p)
  remove(p)
  gc()
  print(v)
}


pd_CAR_b <- NULL

for (v in names(modelling_RF_b[,-2])) {
  p <- pdp::partial(object = final_model, v, which.class = "CAR", grid.resolution = 10,
                    plot = F, train = train_balanced)
  p <- p %>% 
    rename(
      var_value = !!v
    ) %>%
    mutate(
      var = v
    )
  pd_CAR_b <- rbind(pd_CAR_b, p)
  remove(p)
  gc()
  print(v)
}

saveRDS(pd_CAR_b, "Model_output/pd_CAR_b.rds")

pd_BIKE_b <- NULL

for (v in names(modelling_RF_b[,-2])) {
  p <- pdp::partial(object = final_model, v, which.class = "BICYCLE", grid.resolution = 10,
                    plot = F, train = train_balanced)
  p <- p %>% 
    rename(
      var_value = !!v
    ) %>%
    mutate(
      var = v
    )
  pd_BIKE_b <- rbind(pd_BIKE_b, p)
  remove(p)
  gc()
  print(v)
}

saveRDS(pd_BIKE_b, "Model_output/pd_BIKE_b.rds")

#visualise partial plots

pd_b$mode <- "WALK"
pd_BIKE_b$mode <- "BICYCLE"
pd_CAR_b$mode <- "CAR"

pd_b <- rbind(pd_b, pd_BIKE_b, pd_CAR_b)

saveRDS(pd_b, "Model_output/pd_b.rds")

####reverse scaled x axis to original values

pd_b <- readRDS("Model_output/pd_b.rds")

set.seed(222)
train.index <- createDataPartition(modelling_RF_b$mode, p = .7, list = FALSE)
train <- modelling_RF_b[ train.index,]
test  <- modelling_RF_b[-train.index,]

train_mean <- colMeans(train[,-2])
train_sd <- apply(train[,-2], 2, sd)

pd_reverse_scale_b <- pd_b %>%
  mutate(
    mean = rep(train_mean, each = 10, times = 3),
    sd = rep(train_sd, each = 10, times = 3),
    var_reversed = var_value *  sd + mean
  )

pd_reverse_scale_b <- pd_reverse_scale_b %>%
   rename(
     Response = yhat
   ) %>%
  mutate(
    var_reversed = ifelse(var == "Bld_area", var_reversed/1000, var_reversed),
    var_name = case_when(
      var == "distance" ~ "Distance (m)",
      var == "Street_length" ~ "Street Length (avg m)",
      var == "EDU" ~ "Education (avg count)",
      var == "REC" ~ "Recreation (avg count)",
      var == "MED" ~ "Health (avg count)",
      var == "RET" ~ "Retail (avg count)",
      var == "Bld_area" ~ "Building Area (hectars)",
      var == "noise_cat_0" ~ "Trip share with\nnoise < 58dB",
      var == "noise_cat_1" ~ "Trip share with\n58dB <= noise < 63dB",
      var == "noise_cat_2" ~ "Trip share with\n63dB <= noise < 68dB",
      var == "noise_cat_3" ~ "Trip share with\n68dB <= noise < 73dB",
      var == "noise_cat_4" ~ "Trip share with\nnoise >= 73dB"
    )
  )


ggplot(data = pd_reverse_scale_b)+
  geom_line(aes(x = var_reversed, y = Response, colour = mode, group = mode))+
  facet_wrap(~var_name, scales = "free")+
  labs(colour = "Travel Mode")+
  theme_minimal()+
  theme(axis.title.x = element_blank())

###Reverse scaled value of the train data to create histograms and show the variable 
#distribution in the training set

#train_balanced <- readRDS("Model_output/train_balanced_b.rds")
means <- data.frame(as.list(train_mean)) %>%
  pivot_longer(cols = everything(), names_to = "var", values_to = "mean")

sd <- data.frame(as.list(train_sd)) %>%
  pivot_longer(cols = everything(), names_to = "var", values_to = "sd")

train_reversed_b <- train_balanced %>% 
  pivot_longer(- mode, names_to = "var", values_to = "var_value") %>%
  inner_join(means, by = c("var" = "var")) %>%
  inner_join(sd, by = c("var" = "var")) %>%
  mutate(
    var_reversed = var_value *  sd + mean,
    var_reversed = ifelse(var == "Bld_area", var_reversed/1000, var_reversed)
  ) 

###count the frequency using the same intervals as in the partial dependence plot

reversed_b <- NULL
var_names <- unique(train_reversed_b$var)

for (v in var_names) {
  breaks <- unique(pd_reverse_scale_b[pd_reverse_scale_b$var == v,]$var_reversed)
  x = hist(train_reversed_b[train_reversed_b$var == v,]$var_reversed, breaks = breaks)
  b = x$breaks
  c = x$counts
  x = data.frame(var_value = b[-length(b)], Response = c, var = v)
  reversed_b = rbind(reversed_b, x)
}

reversed_b <- reversed_b %>%
  mutate(
      var_name = case_when(
      var == "distance" ~ "Distance (m)",
      var == "Street_length" ~ "Street Length (avg m)",
      var == "EDU" ~ "Education (avg count)",
      var == "REC" ~ "Recreation (avg count)",
      var == "MED" ~ "Health (avg count)",
      var == "RET" ~ "Retail (avg count)",
      var == "Bld_area" ~ "Building Area (hectars)",
      var == "noise_cat_0" ~ "Trip share with\nnoise < 58dB",
      var == "noise_cat_1" ~ "Trip share with\n58dB <= noise < 63dB",
      var == "noise_cat_2" ~ "Trip share with\n63dB <= noise < 68dB",
      var == "noise_cat_3" ~ "Trip share with\n68dB <= noise < 73dB",
      var == "noise_cat_4" ~ "Trip share with\nnoise >= 73dB"
    )
  )


###generate plots combining the pdp and hist for each variable

plots <- list()
var_names <- unique(reversed_b$var_name)
i=0

for (v in var_names){
  
  i=i+1
  
  pmain <- ggplot(data = pd_reverse_scale_b[pd_reverse_scale_b$var_name == v,], aes(x = var_reversed, y = Response, colour = mode, group = mode)) + 
    geom_line() +
    theme_minimal() +
    labs(x = v) +
    theme(axis.title.y = element_blank(), legend.position = "none", text = element_text(size = 10))
  
  xbox <- axis_canvas(pmain, axis = "x", coord_flip = F) +
    geom_bar(data = reversed_b[reversed_b$var_name == v,], aes(x = var_value, y = Response), stat = "identity", alpha = 0.6, just = 0) 
  
  p1 <- insert_xaxis_grob(pmain, xbox, grid::unit(0.5, "in"), position = "top")
  
  plots[[i]] <- p1
  
}


ggdraw(plots[[7]])

plot1_legend <- ggplot(data = pd_reverse_scale_b[pd_reverse_scale_b$var_name == v,], aes(x = var_reversed, y = Response, colour = mode, group = mode)) + 
  geom_line() +
  theme_minimal() +
  labs(x = v, color = "Travel Mode") +
  theme(axis.title.y = element_blank(), legend.position = "top")

# function to extract legend from plot 
get_only_legend <- function(plot) { 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend) 
} 

# extract legend from plot1 using above function 
legend <- get_only_legend(plot1_legend)    

prow <- grid.arrange(plots[[1]],
                     plots[[2]],
                     plots[[3]],
                     plots[[4]],
                     plots[[5]],
                     plots[[6]],
                     plots[[7]],
                     plots[[8]],
                     plots[[9]],
                     plots[[10]],
                     plots[[11]],
                     plots[[12]],
                     ncol = 3, legend
                     )

png("img/Brisbane_m_h.png",units="cm", width=23, height=25, res=300)
annotate_figure(prow, 
                left = text_grob("Partial Dependence", 
                                 color = "black", 
                                 rot = 90, 
                                 size = 12,
                                 family = "arial"))
dev.off()


####combine importance plots

imp_b <- readRDS("Model_output/imp_b.rds")
imp_l <- readRDS("Model_output/imp_l.rds")

png("img/Importance_L_B.png",units="cm", width=20, height=10, res=300)
ggarrange(imp_l,imp_b, ncol = 2, labels = c("A", "B"))
dev.off()

