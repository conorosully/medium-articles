# PDPs and ICE Plots
# Conor O'Sullivan
# 16 June 2022

# ======================
# Importing dataset
# ======================

# You can find this dataset here: https://www.kaggle.com/datasets/conorsully1/pdp-and-ice-plots/settings
setwd("~/Documents/git/medium-articles/data")

dataset = read.csv('PDP_ICE.csv',sep = "\t")
dataset$car_type = factor(dataset$car_type, levels = c(0, 1))

setwd("~/Google Drive/My Drive/Medium/PDP and ICE Plots/R")

# ======================
# Training models
# ======================
library(randomForest)

# Continuous target variable 
rf = randomForest(x = dataset[0:5],
                  y = dataset$price,
                  ntree = 100)

#Binary target variable 
#Create binary target variable 
mean_price = mean(dataset$price)
price_binary = as.integer(dataset$price > mean_price)
price_binary = factor(price_binary, levels = c(0, 1))

# Train model
rf_binary = randomForest(x = dataset[0:5],
                         y = price_binary,
                         ntree = 100)


# ======================
# Package - ICEbox
# ======================

require(ICEbox)

#PDP
iceplot = ice(object = rf, 
              X = dataset[0:5], 
              y = dataset$price, 
              predictor = "car_age")
jpeg('R_code_1.jpg',width = 800, height = 600, res=150)
plot(iceplot, 
     plot_orig_pts_preds = F,
     colorvec = '000000')
dev.off()

#ICE Plot
jpeg('R_code_2.jpg',width = 800, height = 600, res=150)
plot(iceplot, 
     frac_to_plot = 0.1, 
     centered = T, 
     plot_orig_pts_preds = F, 
     color_by = "car_type")
dev.off()

#Derivative PDP
iceplot = ice(object = rf, 
              X = dataset[0:5], 
              y = dataset$price, 
              predictor = "repairs")
dice = dice(iceplot)

jpeg('R_code_3.jpg',width = 800, height = 600, res=150)
plot(dice,
     plot_orig_pts_deriv = F,
     colorvec = '000000',
     plot_sd=F)
dev.off()

#Binary target variable 
iceplot = ice(object = rf_binary, 
              X = dataset[0:5], 
              predictor = "car_age",
              predictfcn = function(object, newdata){
                predict(object, newdata, type = "prob")[, 2]
              })

jpeg('R_code_4.jpg',width = 800, height = 600, res=150)
plot(iceplot, 
     plot_orig_pts_preds = F,
     colorvec = '000000')
dev.off()

# ======================
# Package - iml
# ======================
require(iml)

pred <- Predictor$new(rf, data = dataset)

jpeg('R_code_5.jpg',width = 800, height = 600, res=150)
eff <- FeatureEffect$new(pred, 
                         feature = "car_age", 
                         method = "pdp")
plot(eff)
dev.off()

jpeg('R_code_6.jpg',width = 800, height = 600, res=150)
eff <- FeatureEffect$new(pred, 
                         feature = "car_age", 
                         method = "pdp+ice",
                         center.at = 0)
plot(eff)
dev.off()

jpeg('R_code_7.jpg',width = 800, height = 600, res=150)
eff <- FeatureEffect$new(pred, 
                         feature = "car_type", 
                         method = "pdp")
plot(eff)
dev.off()

jpeg('R_code_8.jpg',width = 800, height = 600, res=150)
eff <- FeatureEffect$new(pred, 
                         feature = "car_type", 
                         method = "ice")
plot(eff)
dev.off()

jpeg('R_code_9.jpg',width = 800, height = 600, res=150)
eff <- FeatureEffect$new(pred, 
                         feature = c("car_age","km_driven"), 
                         method = "pdp")
plot(eff)
dev.off()

jpeg('R_code_10.jpg',width = 1200, height = 600, res=150)
pred <- Predictor$new(rf_binary, data = dataset)
eff <- FeatureEffect$new(pred, 
                         feature = "car_age", 
                         method = "pdp")
plot(eff)
dev.off()

# ======================
# Package - vip
# ======================

library(vip)
packageVersion(vip) 
sessionInfo()

jpeg('R_code_11.jpg',width = 800, height = 600, res=150)
vip(rf, method = "firm",ice = TRUE)
dev.off()

jpeg('R_code_12.jpg',width = 800, height = 600, res=150)
vip(rf, method = "firm",ice = TRUE)
dev.off()


