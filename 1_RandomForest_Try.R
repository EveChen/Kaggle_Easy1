#Forest Cover Prediction - Kaggle

#Environment Setup
###Remove Objects
rm(list = ls())

###Clear Memory
gc(reset = TRUE)

###Set Working Directory
setwd("/Users/eve/Dropbox/Activities_2017/Kaggle1_ForestCoverTypePredictions")

###Library
library(dplyr)
library(randomForest)
library(ggplot2)

#Data Summary
dat <- read.csv("train.csv", header = TRUE)
nrow(dat) #Totally 15120 observations

dat$Wilderness_Area1 <- as.factor(dat$Wilderness_Area1)
for(i in 12 : 56) {
  dat[, i] <- as.factor(dat[, i])
}

dat <- dat[, -1]
summary(dat)
colnames(dat)

###See outliers ???
unique(sort(dat$Slope))
boxplot(dat$Slope)


#Quick view from plots ???
dat <- dat[, -1]
elev <- dat %>% group_by(dat$Cover_Type) %>% 
  summarize(average = mean(dat$Horizontal_Distance_To_Hydrology)) #Same?!


#Split to 80% train, 20% test
#Test: sample(1:10, 8, replace = FALSE)
set.seed(123)
sample <- sample.int(n = nrow(dat), size = floor(0.8 * nrow(dat)), replace = FALSE)
train <- dat[sample, ]  #12096
test <- dat[-sample, ]  #3024

####Split other method - use package caret (function createDataPartition)
#https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
length(train)
colnames(dat)
#Model
####Random Forest
model_rf <- randomForest(y = train$Cover_Type, x = train[, -length(train)],
                         data = train, ntree = 100, importance = TRUE)
model_rf

pred_rf <- predict(model_rf, test)
test$predict <- predict(model_rf, test) #For plotting ROC curve
table(pred_rf, test$Cover_Type)  #Conversion matrix

###AUC - use pakcage "pROC"
library(pROC)
roc_rf <- roc(test$Cover_Type, test$predict)
#######Warning: 'response' has more than two levels. Consider setting 'levels' explicitly or using 'multiclass.roc' instead
roc_rf <- multiclass.roc(test$Cover_Type, as.numeric(test$predict))
auc(roc_rf)  #0.8786
plot(roc_rf, print.auc = TRUE)
