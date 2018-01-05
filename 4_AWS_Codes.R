#3. Apply Bagging, Random Forest, Boosting Models to Forest Data

#Environment Setup
###Remove all stuffs
rm(list = ls())

###Clean memory
gc(reset = TRUE)

#Set Working directory
setwd("/Users/eve/Dropbox/Activities_2017/Kaggle1_ForestCoverTypePredictions")

###library
# install.packages("randomForest")
# install.packages("gbm")
# install.packages("MASS")
library(randomForest)
library(MASS)
library(gbm)

#Load data & Split into train/test data sets
dat <- read.csv("train.csv")
for (i in 12 : 56){
  dat[, i] <- as.factor(dat[, i])
}

sample <- sample(nrow(dat), size = floor(0.8 * nrow(dat)))
train <- dat[sample, ]  #12096
test <- dat[-sample, ]  #3024

View(train)
#Bagging
model_bag <- randomForest(Cover_Type ~ ., data = train, mtry = 55, 
                          importance = TRUE)
model_bag

###Importance
importance(model_bag) #Elevation is the most important feature
varImpPlot(model_bag)

###Prediction
pred_bag <- predict(model_bag, newdata = test)

#Random Forest Model
mtry = floor(sqrt(55))
model_rf <- randomForest(Cover_Type ~ ., mtry = 7, data = train,
                         importance = TRUE)

###Importance
varImpPlot(model_rf) #Evaluation plays the most important role

###Prediction
pred_rf <- predict(model_rf, newdata = test)
table(pred_rf, test$Cover_Type)

table(pred_rf) #How to interpret?


#Boosting
###Model (n.trees = 5000, depth = 1, shrinkage = 0.01, distribution = multinomial)
model_boost <- gbm(Cover_Type ~ ., data = train, n.trees = 500,
                   shrinkage = 0.01, distribution = "multinomial",
                   interaction.depth = 4)
summary(model_boost)

###Prediction
pred_boost <- predict(model_boost, n.trees = 500,
                      newdata = test)
plot(pred_boost, test$Cover_Type)
nrow(pred_boost)
ncol(pred_boost)

##########Load in real train & test data
real_train <- read.csv("train.csv")
real_test <- read.csv("test.csv")
real_test$Cover_Type <- c(1)
for(i in 12 : 56) {
  real_train[, i] <- as.factor(real_train[, i])
  real_test[, i] <- as.factor(real_test[, i])
}

real_test$Cover_Type <- factor(real_test$Cover_Type, levels = 1:7)
real_train$Soil_Type15 <- factor(real_train$Soil_Type15, 
                                 levels = levels(real_train$Soil_Type16))

colnames(real_test)
summary(real_test)

real_train <- real_train[, -1]
real_test <- real_test[, -1]

colnames(real_train)
colnames(real_test)

#Random Forest
m = sqrt(55) #mtry
rf1 <- randomForest(Cover_Type ~., data = real_train,
                    importance = TRUE, mtry = 7)
summary(rf1)
?randomForest
###See importance
varImpPlot(rf1)

###Prediction
pred_rf1 <- predict(rf1, newdata = real_test)
# Error in predict.randomForest(rf1, newdata = real_test) : 
#   New factor levels not present in the training data


#New random Forest model (Get rid of soils and Wildness Areas)
train1 <- real_train[, -(12 : 55)]
test1 <- real_test[, -(12 : 55)]

rf2 <- 
  colnames(real_train)