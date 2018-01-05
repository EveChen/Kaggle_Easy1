#3. Apply Bagging, Random Forest, Boosting Models to Forest Data

#Environment Setup
###Remove all stuffs
rm(list = ls())

###Clean memory
gc(reset = TRUE)

###Set Working directory
setwd("/Users/eve/Dropbox/Activities_2017/Kaggle1_ForestCoverTypePredictions")

#Load data & Split into train/test data sets
dat <- read.csv("train.csv")
for (i in 12 : 56){
  dat[, i] <- as.factor(dat[, i])
}

sample <- sample(nrow(dat), size = floor(0.8 * nrow(dat)))
train <- dat[sample, ]  #12096
test <- dat[-sample, ]  #3024

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
model_boost <- gbm(Cover_Type ~ ., data = train, n.trees = 5000,
                   shrinkage = 0.01, distribution = "multinomial",
                   interaction.depth = 4)
