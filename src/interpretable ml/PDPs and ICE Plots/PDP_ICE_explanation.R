# Charts for PDPs and ICE Plots and Explanations
# Conor O'Sullivan
# 06 June 2022

# ======================
# Importing Dataset
# ======================

# NOTE: To create dataset, see "PDP_ICE_data.R" 
setwd("~/Documents/git/medium-articles/data")
dataset = read.csv('PDP_ICE.csv',sep = "\t")

dataset$car_type = factor(dataset$car_type, levels = c(0, 1))

owner_age  = dataset$owner_age  
km_driven = dataset$km_driven
car_age = dataset$car_age
repairs = dataset$repairs
car_type = dataset$car_type
price = dataset$price

setwd("~/Google Drive/My Drive/Medium/PDP and ICE Plots/R")

# ======================
# Random Forest
# ======================
library(randomForest)
set.seed(123)

# Train model on entire set
rf = randomForest(x = dataset[0:5],
                  y = dataset$price,
                  ntree = 100,
                  importance=TRUE)

# ======================
# Scatter Plots
# ======================

library(ggplot2)
library("gridExtra")

# Linear relationships
p1 = ggplot(dataset, aes(x=owner_age, price)) + geom_point(color='#00BFC4')
p2 = ggplot(dataset, aes(x=km_driven/1000, price)) + geom_point(color='#00BFC4') + xlab("km_driven (thousand)")

jpeg('scatterplot1.jpg',width = 1500, height = 600, res=150)
grid.arrange(p1, p2, nrow = 1)
dev.off()

# Non-linear relationships
p1 = ggplot(dataset, aes(x=car_age, price)) + geom_point(color='#00BFC4')
p2 = ggplot(dataset, aes(x=repairs, price)) + geom_point(color='#00BFC4')

jpeg('scatterplot2.jpg',width = 1500, height = 600, res=150)
grid.arrange(p1, p2, nrow = 1)
dev.off()

# car_age:car_type
jpeg('car_age_car_type.jpg',width = 1000, height = 600, res=200)
ggplot(dataset, aes(x=car_age, y=price,color=car_type)) + geom_point()
dev.off()

#car_age:km_driven correlation
cor(car_age, km_driven) 

jpeg('correlation.jpg',width = 1000, height = 600, res=150)
ggplot(dataset, aes(x=car_age, km_driven)) + geom_point(color='#00BFC4')
dev.off()


# ======================
# Step by step breakdown
# ======================
require(ICEbox)

# Choose points
dataset[0:100,][order(dataset[0:100,]$car_age),] #Choice = 59 (8) 17 (81)

#Display data points
dataset[c(59,17),][0:6] 
predict(rf, newdata = dataset[c(59,17),][0:5])

# Chart used to explain PDP plot 
jpeg('pdp_exp_1.jpg',width = 1000, height = 600, res=150)
set.seed(12)
iceplot = ice(object = rf, X = dataset[1:100,][0:5], y = dataset$price, predictor = "car_age")
plot(iceplot, frac_to_plot = 1, plot_points_indices = c(8,81),centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = T,plot_pdp=F)
dev.off()

jpeg('pdp_exp_2.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[1:100,][0:5], y = dataset$price, predictor = "car_age")
plot(iceplot, frac_to_plot = 1,centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F,plot_pdp=F)
dev.off()

jpeg('pdp_exp_3.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[1:100,][0:5], y = dataset$price, predictor = "car_age")
plot(iceplot, frac_to_plot = 1,centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F,plot_pdp=T)
dev.off()

jpeg('pdp_exp_4.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[1:100,][0:5], y = dataset$price, predictor = "car_age")
plot(iceplot, frac_to_plot = 1,centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F,plot_pdp=T,
     colorvec = '000000')
dev.off()


# ======================
# PDP Plots
# ======================

# car_age
jpeg('car_age_dpd.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, 
              X = dataset[0:5], 
              y = dataset$price, 
              predictor = "car_age")
plot(iceplot, 
     frac_to_plot = 1, 
     centered = F, 
     prop_range_y = TRUE, 
     plot_orig_pts_preds = F,
     colorvec = '000000')
dev.off()

jpeg('repairs_dpd.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:5], y = dataset$price, predictor = "repairs")
plot(iceplot, frac_to_plot = 1, centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F,colorvec = '000000')
dev.off()

jpeg('owner_age_dpd.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:5], y = dataset$price, predictor = "owner_age")
plot(iceplot, frac_to_plot = 1, centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F,colorvec = '000000')
dev.off()

jpeg('km_driven_dpd.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:5], y = dataset$price, predictor = "km_driven")
plot(iceplot, frac_to_plot = 1, centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F,colorvec = '000000')
dev.off()


jpeg('car_type_dpd.jpg',width = 1000, height = 600, res=150)
mod <- Predictor$new(rf, data = dataset)
eff <- FeatureEffect$new(mod, feature = "car_type", method = "pdp")
plot(eff)
dev.off()

# ======================
# rug plot 
# ======================

jpeg('km_driven_dpd_2.jpg',width = 1000, height = 600, res=150)
mod <- Predictor$new(rf, data = dataset)
eff <- FeatureEffect$new(mod, feature = "km_driven", method = "pdp")
plot(eff)
dev.off()

# ======================
# Binary target variable 
# ======================

mean_price = mean(dataset$price)
price_binary = as.integer(dataset$price > mean_price)
price_binary = factor(price_binary, levels = c(0, 1))

# Train model on entire set
rf_binary = randomForest(x = dataset[0:5],
                  y = price_binary,
                  ntree = 100,
                  importance=TRUE)

# car_age
jpeg('binary_dpd.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf_binary, 
              X = dataset[0:5], 
              predictor = "car_age",
              predictfcn = function(object, newdata){
                      predict(object, newdata, type = "prob")[, 2]
              })
plot(iceplot, 
     frac_to_plot = 1, 
     centered = F, 
     prop_range_y = TRUE, 
     plot_orig_pts_preds = F,
     colorvec = '000000')
dev.off()


# ======================
# 2 Features
# ======================
jpeg('2_factor_dpd.jpg',width = 1000, height = 600, res=150)
mod <- Predictor$new(rf, data = dataset)
eff <- FeatureEffect$new(mod, feature = c("car_age","km_driven"), method = "pdp")
plot(eff)
dev.off()

# ======================
# Feature importance
# ======================
library(vip)
jpeg('vi_pdp.jpg',width = 1000, height = 600, res=150)
vip(rf, method = "firm")
dev.off()

jpeg('vi_ice.jpg',width = 1000, height = 600, res=150)
vip(rf, method = "firm",ice = TRUE)
dev.off()


# ======================
# Derivative PDP
# ======================

jpeg('derivative_dpd.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:5], y = dataset$price, predictor = "repairs")
dice = dice(iceplot)
plot(dice,x_quantile = F,plot_orig_pts_deriv = F,colorvec = '000000',plot_sd=F)
dev.off()


# ======================
# ICE Plots
# ======================

# car_age
jpeg('car_age_ice_1.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:5], y = dataset$price, predictor = "car_age")
plot(iceplot, frac_to_plot = 0.1, centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F,plot_pdp=F)
dev.off()

# car_age
jpeg('car_age_ice_2.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:5], y = dataset$price, predictor = "car_age")
plot(iceplot, frac_to_plot = 0.1, centered = T, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F,plot_pdp=F)
dev.off()


# car_age:car_type
jpeg('car_age_ice_3.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:5], y = dataset$price, predictor = "car_age")
plot(iceplot, frac_to_plot = 0.1, centered = T, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F, color_by = "car_type",plot_pdp=F)
dev.off()

# car_age:car_type
jpeg('car_age_ice_4.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:5], y = dataset$price, predictor = "car_age")
plot(iceplot, frac_to_plot = 0.1, centered = T, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F, color_by = "car_type")
dev.off()

# car_age:car_type
jpeg('car_age_ice_cover.jpg',width = 1500, height = 1300, res=300)
iceplot = ice(object = rf, X = dataset[0:5], y = dataset$price, predictor = "car_age")
plot(iceplot, frac_to_plot = 0.1, centered = T, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F, color_by = "car_type")
dev.off()


# car_age:car_type
jpeg('derivative_ice.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:5], y = dataset$price, predictor = "car_age")
dice = dice(iceplot)
plot(dice,frac_to_plot = 0.1,x_quantile = F,plot_orig_pts_deriv = F,color_by = "car_type", plot_sd=F)
dev.off()



