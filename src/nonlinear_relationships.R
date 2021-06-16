# Finding and Visualising Nonlinear Relationships
# Conor O'Sullivan
# 01 June 2021

# ======================
# Importing Dataset
# ======================

# NOTE: To create dataset, run "Generating Dataset" code at the end of the script
setwd("~/Documents/git/medium-articles/data")
dataset = read.csv('nonlinear_dataset.csv',sep = "\t")

owner_age  = dataset$owner_age  
km_driven = dataset$km_driven
car_age = dataset$car_age
repairs = dataset$repairs
price = dataset$price


# ======================
# Scatter Plots
# ======================
setwd("~/Documents/git/medium-articles/figures")
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


# ======================
# Random Forest
# ======================

# Splitting the dataset 
library(caTools)
split = sample.split(dataset$price, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Training model on training set
library(randomForest)
set.seed(123)

rf = randomForest(x = training_set[0:4],
                          y = training_set$price,
                          ntree = 100,
                          importance=TRUE)

# Accuracy on test set
actual = test_set$price
predicted = predict(rf, newdata = test_set[0:4])
results =data.frame( cbind(actual,predicted))

mean((actual - predicted)^2)

jpeg('accuracy_plot.jpg',width = 600, height = 600, res=150)
ggplot(results, aes(x=actual, y=predicted)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.5) +
 coord_cartesian(ylim=c(1250,10200),xlim=c(1250,10200))
dev.off()

# Retrain model on entire set
rf = randomForest(x = dataset[0:4],
                  y = dataset$price,
                  ntree = 100,
                  importance=TRUE)

# ======================
# PDP Plots
# ======================
require(ICEbox)

# Chart used to explain PDP plot 
dataset[c(1,2),][0:4] #Actual = 4491.33, 5882.16
predict(rf, newdata = dataset[c(1,2),][0:4]) #Predictions = 4570.14, 5993.46

jpeg('car_age_example.jpg',width = 1000, height = 600, res=150)
set.seed(12)
iceplot = ice(object = rf, X = dataset[1:100,][0:5], y = dataset$price, predictor = "car_age")
plot(iceplot, frac_to_plot = 1, plot_points_indices = c(50,53),centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = T,plot_pdp=F)
dev.off()

# car_age
jpeg('car_age_dpd.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:4], y = dataset$price, predictor = "car_age")
plot(iceplot, frac_to_plot = 1, centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F)
dev.off()

jpeg('repairs_dpd.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:4], y = dataset$price, predictor = "repairs")
plot(iceplot, frac_to_plot = 1, centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F,colorvec = '000000')
dev.off()

jpeg('owner_age_dpd.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:4], y = dataset$price, predictor = "owner_age")
plot(iceplot, frac_to_plot = 1, centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F,colorvec = '000000')
dev.off()

jpeg('km_driven_dpd.jpg',width = 1000, height = 600, res=150)
iceplot = ice(object = rf, X = dataset[0:4], y = dataset$price, predictor = "km_driven")
plot(iceplot, frac_to_plot = 1, centered = F, prop_range_y = TRUE,
     x_quantile = F, plot_orig_pts_preds = F,colorvec = '000000')
dev.off()


# ======================
# Correlation
# ======================
cor(price, owner_age)  
cor(price, km_driven)  
cor(price, car_age)  
cor(price, repairs)  

features = c("owner_age","km_driven","car_age","repairs")
correlation = c(-0.0355,-0.457876,0.0781,-0.3140)
df = data.frame(features, correlation)
df$features <- factor(df$features, levels = df$features)

jpeg('correlation.jpg',width = 1000, height = 600, res=150)
ggplot(df, aes(x=features, y=correlation)) + 
  geom_bar(stat = "identity",color='#00BFC4',fill='#00BFC4') + 
  xlab("") + ylab("Correlation")
dev.off()

# ======================
# Variable Importance
# ======================
importance(rf,type=1)
df$importance = c(2.8432,43.6313,32.0853,56.9427)

jpeg('importance.jpg',width = 1000, height = 600, res=150)
ggplot(df, aes(x=features, y=importance)) + 
  geom_bar(stat = "identity",color='#00BFC4',fill='#00BFC4') + 
  xlab("") + ylab("Feature Importance")
dev.off()

# ======================
# Mutual Information
# ======================
library(infotheo)

dat <- discretize(dataset)
mutinformation(dat)
df$mutinformation = c(0.0338,0.1389,0.1551,0.3604)

jpeg('mutinformation.jpg',width = 1000, height = 600, res=150)
ggplot(df, aes(x=features, y=mutinformation)) + 
  geom_bar(stat = "identity",color='#00BFC4',fill='#00BFC4') + 
  xlab("") + ylab("Mutual Information")
dev.off()

# ======================
# Relationship Examples
# ======================
library(comprehenr)

theme =   theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                axis.title=element_text(size=14,face="bold"),
                plot.title = element_text(hjust = 0.5,size = 18))

step <- function(x) {
  #Creates step relationship example
  if (x < -3) {result = 0} 
  else if (x < -1) {result = 2} 
  else if (x < 1) {result = 4} 
  else if (x < 3) {result = 6} 
  else {result = 8}
  
  return(result)
}

x = seq(-5,5, length=100)
y1 = x + runif(100, 0, 1) #linear
y2 = x**2 + runif(100, 0, 5) #quadratic
y3 = 3**x + runif(100, 0, 20) #exponential 
y4 = to_vec(for(i in x) step(i)) + runif(100, 0, 1) #step
examples = data.frame(x, y1,y2,y3,y4)

#Linear
jpeg('linear_example.jpg',width = 300, height = 300, res=75)
ggplot(examples, aes(x, y1)) + geom_point(color='#00BFC4') + theme +labs(title="Linear", x ="X", y = "Y")
dev.off()

#Non-linear
p1 = ggplot(examples, aes(x, y2)) + geom_point(color='#00BFC4') + theme +labs(title="Quadratic", x ="", y = "")
p2 = ggplot(examples, aes(x, y3)) + geom_point(color='#00BFC4') + theme +labs(title="Exponential", x ="", y = "")
p3 = ggplot(examples, aes(x, y4)) + geom_point(color='#00BFC4') + theme +labs(title="Step", x ="", y = "")

jpeg('nonlinear_example.jpg',width = 1800, height = 600, res=150)
grid.arrange(p1, p2,p3, nrow = 1)
dev.off()



# ======================
# Generate Dataset 
# ======================

new_row <- function() {
  "Generates one random row (i.e. car sale) of the feature matrix"
  
  owner_age = sample(18:75, 1)
  car_age = round(runif(1, 1, 40),2)
  km_driven = sample(1000:300000, 1)
  repairs = sample(0:25, 1)
  
  #Target Variable 
  price = (0*owner_age +
    (2.493)*car_age^2-99.723*car_age + 1097.23 +
    (-1)*km_driven/325 + 1000 +
    (0.4566)*repairs^3 - (23.9249)*repairs^2 + (313.3928)*repairs+ 100 +
    runif(1, 0, 400) )*3
  
  row = c(owner_age,car_age,km_driven,repairs,price)
  return(row)
}

new_row()
#Create dataset of 1000 employees
datalist = list()
for (i in 1:1000) {
  datalist[[i]] <- new_row()
}
data = do.call(rbind, datalist)
colnames(data) =  c('owner_age', 'car_age', 'km_driven', 'repairs', 'price') 

#Save table
setwd("~/Documents/git/medium-articles/data")
write.table(data, file = "nonlinear_dataset.csv",
sep = "\t", row.names = F)

