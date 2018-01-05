#2_Chapter 8 Lab

#Environment Setup
###Remove all stuffs
rm(list = ls())

###Clean memery
gc(reset = TRUE)

###Set working directory
setwd("/Users/eve/Dropbox/Activities_2017/Kaggle1_ForestCoverTypePredictions")

#Boston data
library(MASS) #Use to import "Boston" data set
library(randomForest) #Use to build "Bagging" and "RandomForest" models
library(gbm)  #Use to build "Boosting" model
View(Boston)  #Totally 506 obs

#Split data
sample <- sample(nrow(Boston), size = floor( 0.8 * nrow(Boston)))
train <- Boston[sample, ]  #404 obs
test <- Boston[-sample,]   #102 obs
View(test)

#Bagging model
###Model
set.seed(1)
# Failed because "subset should be a vector"
# From: https://stackoverflow.com/questions/35533219/error-in-xji-invalid-subscript-type-list
# model_bag <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13,
#                           importance = TRUE)

model_bag <- randomForest(medv ~ ., data = train, mtry = 13, importance = TRUE)
model_bag

###Prediction
pred_bag <- predict(model_bag, newdata = test)
plot(pred_bag, test$medv)
abline(0, 1)
mean((pred_bag - test$medv)^2)  #7.944803

###Importance of Each feature
importance(model_bag)
varImpPlot(model_bag)

#Random Forest
model_rf <- randomForest(medv ~ ., data = train, mtry = 6, importance = TRUE)
model_rf

###Prediction
pred_rf <- predict(model_rf, newdata = test)
plot(pred_rf, test$medv)
abline(0, 1)
mean((pred_rf - test$medv)^2)  #7.710077

###Importance of each features
importance(model_rf)
varImpPlot(model_rf)

#Boosting - Use gbm
###Model
model_boosting <- gbm(medv ~., data = train, 
                      n.trees = 5000, distribution = "gaussian", 
                      interaction.depth = 4)
model_boosting

###Importance
summary(model_boosting)

###Partial Dependence Plots 排除其他features後該變數對於output variable的影響
par(mfrow = c(1, 2))
plot(model_boosting, i = "rm") #The larger the house size is, the median house prices are increasing
plot(model_boosting, i = 'lstat') #If the wealth level of the community increase, the median house prices will decrease

###Prediction
pred_boosting <- predict(model_boosting, newdata = test, n.trees = 5000) #Need too add "n.trees" inside
plot(pred_boosting, test$medv)
abline(0, 1)
mean((pred_boosting - test$medv)^2) #8.312257

###Model-Boosting, add shrinkage = 0.01
model_boosting2 <- gbm(medv ~., data = train, distribution = "gaussian",
                       shrinkage = 0.01, n.trees = 5000, 
                       interaction.depth = 4)
model_boosting2

###Prediction
pred_boosting2 <- predict(model_boosting2, newdata = test, n.trees = 5000)
plot(pred_boosting2, test$medv)
abline(0, 1)
mean((pred_boosting2 - test$medv) ^ 2) #6.878615


