#5. Merge train & test sets

#Step0 Set up
#Set Working directory
setwd("/Users/eve/Dropbox/Activities_2017/Kaggle1_ForestCoverTypePredictions")

###library
library(randomForest)
library(MASS)
library(gbm)
library(pROC)

#Step1 Merge - trainold, testold, merge(dat), train_m1, test_m1
trainold <- read.csv("train.csv")
testold <- read.csv("test.csv")
testold$Cover_Type <- 1
merge <- rbind(trainold, testold)
dat <- merge

View(merge)
nrow(trainold) #15120
nrow(testold)  #565892
nrow(merge) #581012

#Step2 Clean data
for(i in 12 : 56) {
  dat[, i] <- as.factor(dat[, i])
}

for(i in 12 : 56) {
  trainold[, i] <- as.factor(trainold[, i])
}
nrow(dat)
colnames(dat)
str(dat)


#Step3 Split data - trainold, testold, merge(dat), train_m1, test_m1
sample <- sample(nrow(trainold), size = floor(0.8 * nrow(trainold)))
train_m1 <- trainold[sample, ]
test_m1 <- trainold[-sample, ]
nrow(test_m1)  #3024

true_test <- dat[15121 : nrow(dat), ] 
summary(true_test)
str(true_test)
str(trainold)
colnames(true_test)

#Step4 Fit models
model_rf <- randomForest(Cover_Type ~., data = train_m1, mtry = 7, importance = TRUE)
### m = floor(sqrt(55))

model_bagging <- randomForest(Cover_Type ~., data = train, mtry = 55, importance = TRUE)

model_boost <- gbm(Cover_Type ~., data = train, n.trees = 500, 
                   importance = TRUE, distribution = "multinomial",
                   interaction.depth = 4)

#Step5 Predict1 - use data "test_m1" and "true_test"
###Use train data to make a model
pred_rf_m1 <- predict(model_rf, newdata = test_m1)
table(pred_rf_m1, test_m1$Cover_Type)

###Apply the above model to a true test set - Failed
pred_rf_true <- predict(model_rf, newdata = true_test)


#Step6 AUC




#Step7 Write.csv, Upload

