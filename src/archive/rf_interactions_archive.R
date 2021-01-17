# Obtianing Knowledge from a Random Forest
# Conor O'Sullivan
# 30 October 2020

# Importing the dataset
setwd("~/Documents/git/medium-articles/data")
dataset = read.csv('interaction_dataset.csv')

# Create dummy variables 
dataset$degree = factor(dataset$degree, levels = c(0, 1))

# Splitting the dataset 
library(caTools)
split = sample.split(dataset$bonus, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# ======================
# Random Forest
# ======================

# training model 
library(randomForest)
set.seed(123)

rf = randomForest(x = training_set[0:5],
                  y = training_set$bonus,
                  ntree = 500,
                  importance=TRUE)

# Predictions
y_true = test_set$bonus
y_pred = predict(rf, newdata = test_set[0:5])

#accuracy
plot(y_true,y_pred)

#Variable importance
importance(rf,type=1)
importance(rf,type=2)

varImpPlot(rf,type=1)
varImpPlot(rf,type=2)

#Ice Plots
require(ICEbox)

iceplot = ice(object = rf, X = training_set[0:5], y = training_set$bonus, predictor = "experience")
## plot
plot(iceplot, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 0.1)
## centered plot
plot(iceplot, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 1, centered = TRUE)
## coloured by degree
plot(iceplot, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,
     x_quantile = T, plot_orig_pts_preds = T, color_by = "degree")


iceplot = ice(object = rf, X = training_set[0:5], y = training_set$bonus, predictor = "sales")
## plot
plot(iceplot, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 0.1)
## centered plot
plot(iceplot, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 1, centered = TRUE)
## coloured by performance
iceplot$Xice$perf_flag = floor(iceplot$Xice$performance/5)
plot(iceplot, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,
     x_quantile = T, plot_orig_pts_preds = T, color_by = "perf_flag")


iceplot = ice(object = rf, X = training_set[0:5], y = training_set$bonus, predictor = "days_late")
## plot
plot(iceplot, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 0.1)
## centered plot
plot(iceplot, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 0.1, centered = TRUE)
## coloured by performance
iceplot$Xice$perf_flag = floor(iceplot$Xice$performance/5)
plot(iceplot, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,
     x_quantile = T, plot_orig_pts_preds = T, color_by = "perf_flag")


#Variable importance with interactions
data2 <- t(apply(dataset, 1, combn, 2, prod))

df_int = model.matrix(~(experience+degree+sales+performance+days_late)^2-1,dataset)

rf = randomForest(x = df_int,
                  y = dataset$bonus,
                  ntree = 100,
                  importance=TRUE)

varImpPlot(rf,type=1)

#Friedman's H-statistic
mod <- Predictor$new(rf, data = training_set[0:5])

overall <- Interaction$new(mod)
plot(overall)

exp_int <- Interaction$new(mod,feature = 'experience')
plot(exp_int)

perf_int <- Interaction$new(mod,feature = 'performance')
plot(perf_int)



# ======================
# Archive
# ======================


# ======================
# Logistic Regression 
# ======================

#training model
lf_classifier = glm(formula = default ~ .,
                    family = binomial,
                    data = training_set)
summary(lf_classifier)

# Predictions
y_true = test_set$bonus 
prob_pred = predict(lf_classifier, type = 'response', newdata = test_set[0:5])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#accuracy
accuracy = round(sum(y_pred == y_true)/length(y_pred),2)
accuracy 


# ======================
# Logistic Regression 
# ======================

## Not run:
require(ICEbox)
require(randomForest)
require(MASS) #has Boston Housing data, Pima
data(Boston) #Boston Housing data
X = Boston
y = X$medv
X$medv = NULL
## build a RF:
bhd_rf_mod = randomForest(X, y)
## Create an 'ice' object for the predictor "age":
bhd.ice = ice(object = bhd_rf_mod, X = X, y = y, predictor = "age",
              frac_to_build = .1)
## plot
plot(bhd.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 1)
## centered plot
plot(bhd.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 1,
     centered = TRUE)
## color the curves by high and low values of 'rm'.
# First create an indicator variable which is 1 if the number of
# rooms is greater than the median:
median_rm = median(X$rm)
bhd.ice$Xice$I_rm = ifelse(bhd.ice$Xice$rm > median_rm, 1, 0)
plot(bhd.ice, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,
     x_quantile = T, plot_orig_pts_preds = T, color_by = "I_rm")

bhd.ice = ice(object = bhd_rf_mod, X = X, y = y, predictor = "age",
              frac_to_build = 1)
plot(bhd.ice, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,
     x_quantile = T, plot_orig_pts_preds = T, color_by = y)
## End(Not run)

#ice plots
bhd.ice = ice(object = rf, X = training_set[0:5], predictor = "experience",logodds = TRUE,
              predictfcn = function(object, newdata){
                predict(object, newdata, type = "prob")[,2]
              })

## plot
plot(bhd.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 0.1)
## centered plot
plot(bhd.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 0.1,
     centered = TRUE)

plot(bhd.ice, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,
     x_quantile = T, plot_orig_pts_preds = T, color_by = "degree")


#promotion
bhd.ice = ice(object = rf, X = training_set[0:5], predictor = "sales",logodds = TRUE,
              predictfcn = function(object, newdata){
                predict(object, newdata, type = "prob")[,2]
              })

## plot
plot(bhd.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 0.1)
## centered plot
plot(bhd.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 0.1,
     centered = TRUE)

bhd.ice$Xice$perf_c = floor(bhd.ice$Xice$performance/5)
plot(bhd.ice, frac_to_plot = 0.1, centered = TRUE, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F, color_by = "perf_c")

accuracy = round(sum(y_pred == y_true)/length(y_pred),2)
accuracy 







