# Finding and Visualising Interactions
# Conor O'Sullivan
# 30 October 2020

# ======================
# Importing Dataset
# ======================

# NOTE: To create dataset, run "Generating Dataset" code at the end of the script
setwd("~/Documents/git/medium-articles/data")
dataset = read.csv('interaction_dataset.csv',sep = "\t")

# Create dummy variables 
dataset$degree = factor(dataset$degree, levels = c(0, 1))

# ======================
# Data Exploration
# ======================
setwd("~/Documents/git/medium-articles/figures")
library(ggplot2)

# experience:degree
jpeg('exp_degree_int.jpg',width = 1000, height = 600, res=200)
ggplot(dataset, aes(x=experience, y=bonus, color=degree)) + geom_point()
dev.off()

# sales:performance
jpeg('sales_perf_int.jpg',width = 1000, height = 600, res=200)
ggplot(dataset, aes(x=sales, y=bonus,color=performance)) + geom_point()
dev.off()

# ======================
# Random Forest
# ======================

# Splitting the dataset 
library(caTools)
split = sample.split(dataset$bonus, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Training model on training set
library(randomForest)
set.seed(123)

rf = randomForest(x = training_set[0:5],
                          y = training_set$bonus,
                          ntree = 100,
                          importance=TRUE)

# Accuracy on test set
actual = test_set$bonus
predicted = predict(rf, newdata = test_set[0:5])
results =data.frame( cbind(actual,predicted))

mean((actual - predicted)^2)

jpeg('accuracy_plot.jpg',width = 600, height = 600, res=150)
ggplot(results, aes(x=actual, y=predicted)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.5) +
 coord_cartesian(ylim=c(0,350),xlim=c(0,350))
dev.off()

# Retrain model on entire set
rf = randomForest(x = dataset[0:5],
                  y = dataset$bonus,
                  ntree = 100,
                  importance=TRUE)

# ======================
# ICE plots
# ======================

require(ICEbox)

# days_late
dataset[c(1,2),] #Predictions = 201.2, 239.2

jpeg('days_ice_example.jpg',width = 1000, height = 600, res=150)
set.seed(12)
iceplot = ice(object = rf, X = dataset[1:10,][0:5], y = dataset$bonus, predictor = "days_late")
plot(iceplot, frac_to_plot = 0.2, centered = F, prop_range_y = TRUE,
x_quantile = F, plot_orig_pts_preds = T,plot_pdp=F)
dev.off()

jpeg('days_ice.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:5], y = dataset$bonus, predictor = "days_late")
plot(iceplot, frac_to_plot = 0.5, centered = T, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F,plot_pdp=T)
dev.off()


# experience:degree
jpeg('exp_degree_ice.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:5], y = dataset$bonus, predictor = "experience")
plot(iceplot, frac_to_plot = 1, centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F, color_by = "degree")
dev.off()

# sales:performance
jpeg('sales_perf_ice.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:5], y = dataset$bonus, predictor = "sales")
iceplot$Xice$perf_flag = floor(iceplot$Xice$performance/5)
plot(iceplot, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F, color_by = "perf_flag")
dev.off()

# ======================
# Variable importance
# ======================

dataset$degree = as.integer(dataset$degree)
df_int = model.matrix(~(experience+degree+sales+performance+days_late)^2-1,dataset)
df_int = data.frame(df_int)
df_int$bonus = dataset$bonus

rf_int = randomForest(x = df_int[,-16],
                  y = df_int$bonus,
                  ntree = 100,
                  importance=TRUE)

jpeg('var_importance.jpg',width = 1000, height = 600, res=150)
varImpPlot(rf_int,type=1,main = "")
dev.off()

#Explain why other interactions are important
library("gridExtra")

p1 = ggplot(df_int, aes(x=experience, y=bonus)) + geom_point(color="#11a3f7")
p2 = ggplot(df_int, aes(x=sales, y=bonus)) + geom_point(color="#11a3f7")
p3 = ggplot(df_int, aes(x=experience.sales, y=bonus)) + geom_point(color="#11a3f7")

jpeg('exp_sales_int.jpg',width = 2000, height = 600, res=150)
grid.arrange(p1, p2, p3, nrow = 1)
dev.off()


# ======================
# Friedman's H-statistic
# ======================
library("iml")

dataset$degree = factor(dataset$degree, levels = c(0, 1))
mod <- Predictor$new(rf, data = dataset[0:5])

#overall
jpeg('hstat_overall.jpg',width = 1000, height = 600, res=150)
overall <- Interaction$new(mod)
plot(overall)
dev.off()

#experience and sales 
exp_int <- Interaction$new(mod,feature = 'experience')
p1 = plot(exp_int)

perf_int <- Interaction$new(mod,feature = 'sales')
p2 = plot(perf_int)

jpeg('hstats_exp_sales.jpg',width = 1800, height = 600, res=150)
grid.arrange(p1, p2, nrow = 1)
dev.off()

# ======================
# Interaction Example
# ======================
age = sample(0:30, 1000,replace = TRUE)
classic = as.integer(runif(1000, 0, 1) < 0.5)
noise = rnorm(1000, mean = 0, sd = 5)
price = (100 -50*classic -age +3*age*classic + noise)/10

car_price = data.frame(age,classic,price)
car_price$classic = factor(car_price$classic, levels = c(0, 1))

jpeg('int_example.jpg',width = 1000, height = 600, res=150)
ggplot(car_price, aes(x=age, y=price, color=classic)) + geom_point()
dev.off()


# ======================
# Generate Dataset 
# ======================

new_row <- function() {
  "Generates one random row (i.e. employee) of the feature matrix"
  
  experience = sample(0:40, 1)
  degree = as.integer(runif(1, 0, 1) < 0.75)
  performance = round(runif(1, 0, 10),2)
  sales = sample(0:100, 1)
  days_late = sample(0:20, 1)
  
  #Target Variable 
  bonus = 20*degree + 
    5*experience*degree + 
    2*performance + 
    (1/5)*sales+ 
    (1/5)*sales*performance +
    (-1)*days_late +
    runif(1, -20, 20) #random variation
  bonus = round(max(0,bonus))
  
  row = c(experience,degree,performance,sales,days_late, bonus)
  return(row)
}

#Create dataset of 1000 employees
datalist = list()
for (i in 1:1000) {
  datalist[[i]] <- new_row()
}
data = do.call(rbind, datalist)
colnames(data) =  c('experience', 'degree', 'performance', 'sales', 'days_late','bonus') 

#Save table
setwd("~/Documents/git/medium-articles/data")
write.table(data, file = "interaction_dataset.csv",
sep = "\t", row.names = F)

