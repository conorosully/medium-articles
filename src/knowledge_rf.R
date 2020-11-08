# Obtianing Knowledge from a Random Forest
# Conor O'Sullivan
# 30 October 2020


# Importing the dataset
setwd("~/Documents/git/medium-articles/data")
dataset = read.csv('default of credit card clients.csv',row.names = 'id')

# Create dummy variables 
dataset$sex = factor(dataset$sex, levels = c(1, 2))
dataset$education = factor(dataset$education, levels = c(0,1,2,3,4,5,6))
dataset$marriage = factor(dataset$marriage, levels = c(0,1,2,3))
dataset$default = factor(dataset$default, levels = c(0,1))

# Splitting the dataset 
library(caTools)
split = sample.split(dataset$default, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# ======================
# Random Forest
# ======================

# training model 
library(randomForest)
set.seed(123)

rf_classifier = randomForest(x = training_set[-24],
                          y = training_set$default,
                          ntree = 500,
                          importance=TRUE)

# Predictions
y_true = test_set$default
y_pred = predict(rf_classifier, newdata = test_set[-24])


#accuracy
accuracy = round(sum(y_pred == y_true)/length(y_pred),2)
accuracy 

#Variable importance
importance(rf_classifier,type=1)
importance(rf_classifier,type=2)

varImpPlot(rf_classifier,type=1)
varImpPlot(rf_classifier,type=2)



# ======================
# Logistic Regression 
# ======================

#training model
lf_classifier = glm(formula = default ~ .,
            family = binomial,
            data = training_set)
summary(lf_classifier)

# Predictions
y_true = test_set$default 
prob_pred = predict(lf_classifier, type = 'response', newdata = test_set[-24])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#accuracy
accuracy = round(sum(y_pred == y_true)/length(y_pred),2)
accuracy 













