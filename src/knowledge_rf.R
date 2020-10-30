# Obtianing Knowledge from a Random Forest
# Conor O'Sullivan
# 30 October 2020



# Importing the dataset
setwd("~/Documents/git/medium-articles/data")
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

#Create dummy variables 
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset 
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# ======================
# Random Forest
# ======================

# training model 
library(randomForest)
set.seed(123)

rf_classifier = randomForest(x = training_set[-3],
                          y = training_set$Purchased,
                          ntree = 500)

# Predictions
y_true = test_set$Purchased
y_pred = predict(rf_classifier, newdata = test_set[-3])

#accuracy
accuracy = round(sum(y_pred == y_true)/length(y_pred),2)
accuracy 

# ======================
# Logistic Regression 
# ======================

#training model
lf_classifier = glm(formula = Purchased ~ .,
            family = binomial,
            data = training_set)
summary(lf_classifier)

# Predictions
y_true = test_set$Purchased
prob_pred = predict(model, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#accuracy
accuracy = round(sum(y_pred == y_true)/length(y_pred),2)
accuracy 













