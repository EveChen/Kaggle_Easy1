#5. Merge train & test sets
###From Kernel: https://www.kaggle.com/prabhats/forest-cover-type-prediction-with-random-forest

#Step0 Set up
###Remove all stuffs
rm(list = ls())

###Clean memory
gc(reset = TRUE)

#Set Working directory
setwd("/Users/eve/Dropbox/Activities_2017/Kaggle1_ForestCoverTypePredictions")

###library
library(dplyr) #For merging data
library(ggplot2) #For plotting data
library(readr)  #For using function read_csv to read large csv files
library(data.table) #For making csv files into a data table
library(randomForest) #For randomForest function
library(caret) #For making confusionMatrix

#Step1 Read train data set - Use "data.table"
trainold <- fread("train.csv", header = TRUE)
finaltest <- fread("test.csv", header = TRUE)
trainmerge <- trainold

#Step2 Change data type into factors
trainmerge[, 12 : 56] <- lapply(trainmerge[, 12 : 56], as.factor)
trainmerge$Soil_Type7 <- factor(trainmerge$Soil_Type7, levels = c(0, 1))
trainmerge$Soil_Type15 <- factor(trainmerge$Soil_Type15, levels = c(0, 1))

finaltest[, 12 : 55] <- lapply(finaltest[, 12 : 55], as.factor)
str(trainmerge)
str(finaltest)

#Step3 Group dummy variables into one column (Wilderness & Soil Types)
###Make a function first
regroup <- function(oldcolumns, newlabels, columnnames, data) {
  for (i in 1 : length(oldcolumns)) {
    data <- data[get(oldcolumns[i]) == 1, 
                         paste(columnnames) := newlabels[i]]
  }
}

###Clean trainmerge data

wild_newlabels <- c("Rawah","Neota","Comanche Peak","Cache la Poudre")
wild_oldcolumns <- c("Wilderness_Area1","Wilderness_Area2","Wilderness_Area3","Wilderness_Area4")
wild_columnname <- "Wilderness_Area"
regroup(wild_oldcolumns, wild_newlabels, wild_columnname, trainmerge)
trainmerge$Wilderness_Area <- as.factor(trainmerge$Wilderness_Area)
str(trainmerge)

soil_newlabels<-c('1','2','3','4','5','6','7', '8','9','10','11','12','13','14','15', '16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32','33','34','35','36','37','38','39','40')
soil_oldcolumns <- c("Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6","Soil_Type7", "Soil_Type8",
                     "Soil_Type9","Soil_Type10","Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type15", "Soil_Type16",
                     "Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24",
                     "Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32",
                     "Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40")
soil_columnname <- "Soil_Type"
regroup(soil_oldcolumns, soil_newlabels, soil_columnname, trainmerge)
trainmerge$Soil_Type <- as.factor(trainmerge$Soil_Type)

trainmerge <- trainmerge[, -(12 : 55)]  #Totally 14 variables
str(trainmerge)
?as.factor
###Clean trainold data (not merge)
#trainold <- trainold[, Wilderness_Area := NULL]
#trainold <- trainold[, -c(22, 30)]  #Remove Soil_Type 7 & 15
#str(trainold) #Totally 54 variables

###Clean finaltest data (make it merged)
FinalTestMerge <- finaltest 
#FinalTestMerge <- FinalTestMerge[, -c(22, 30)]
str(FinalTestMerge)

wild_newlabels <- c("Rawah","Neota","Comanche Peak","Cache la Poudre")
wild_oldcolumns <- c("Wilderness_Area1","Wilderness_Area2","Wilderness_Area3","Wilderness_Area4")
wild_columnname <- "Wilderness_Area"
regroup(wild_oldcolumns, wild_newlabels, wild_columnname, FinalTestMerge)
FinalTestMerge$Wilderness_Area <- as.factor(FinalTestMerge$Wilderness_Area)


soil_newlabels<-c('1','2','3','4','5','6', '7', '8','9','10','11','12','13','14', '15', '16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32','33','34','35','36','37','38','39','40')
soil_oldcolumns <- c("Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6", "Soil_Type7","Soil_Type8",
                     "Soil_Type9","Soil_Type10","Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14", "Soil_Type15", "Soil_Type16",
                     "Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20","Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24",
                     "Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30","Soil_Type31","Soil_Type32",
                     "Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40")
soil_columnname <- "Soil_Type"
regroup(soil_oldcolumns, soil_newlabels, soil_columnname, FinalTestMerge)
FinalTestMerge$Soil_Type <- as.factor(FinalTestMerge$Soil_Type)
#FinalTestMerge <- FinalTestMerge[, -(12 : 53)]  #Totally 13 variables
colnames(FinalTestMerge)
FinalTestMerge <- FinalTestMerge[, -(12 : 55)]
str(FinalTestMerge)

#####Now, we have three data sets: 
### 1. trainold(54 features) - Not merge 
### 2. trainmerge (14 features) - Merged Wilderness & Soil_Types 
### 3. FinalTestMerge (13 features) - Merged, data used to submission

#Step4 The difference between FinalTestMerge & TrainMerge sets - Soil_Type
str(FinalTestMerge) #Soil_Type = 40 factors
str(trainmerge)     #Soil_Type = 38 factors - Type7 & Type15 = 0
trainmerge[which(trainmerge$Soil_Type == 7),] #empty
trainmerge[which(trainmerge$Soil_Type == 15),] #empty
summary(trainmerge)

#Step4 Data preparation for modeling
###split to train & validation data sets from trainmerge data
sample <- sample(nrow(trainmerge), size = floor(0.8 * nrow(trainmerge)))

####Create train & valid sets from "trainold"
trainall <- trainmerge[sample, ]   #12096
validall <- trainmerge[-sample, ]  #3024

str(trainmerge) #Q: Soil_Type has 38 
str(FinalTestMerge)  #Q: Soil_Type has 40

#Step7 Random Forest model - two models
#### Know the accuracy, feature importance toward this model
#### Use validation set to confirm the robustness of this model

### Model 1: Apply rf to merged data (Merged Wilderness & Soil Type)
####Find the best mtry first
bestmtry <- tuneRF(trainall, trainall$Cover_Type, 
                   ntreeTry = 500, trace = T, plot = T)

####Apply random forest to merged data using the bestmtry 7
rf_merge <- randomForest(Cover_Type ~ .-Id, data = trainall, 
                           mtry = 7, importance = TRUE)
### Accuracy = 77.14%, OOB = 22.86%

###Prediction & Confusion Matrix
validall$Result <- predict(rf_merge, newdata = validall) 
confusionMatrix(validall$Result, validall$Cover_Type) #Accuracy = 78.6%

###Prediction on finaltest data set


FinalTestMerge$Result <- predict(rf_merge, FinalTestMerge)
output_merged <- cbind(FinalTestMerge$Id, FinalTestMerge$Result) %>% as.data.frame() 
colnames(output_merged) <- c("Id", "Cover_Type")

View(output_merged) #Some output variables are "NA"

###Change all 108 NAs into 2 (mean)
junk1 <- output_merged %>% as.data.frame()
junk1[which(junk1$Id == 82897), 2] <- 2
junk1[which(junk1$Id == 82897), ]

junk1[is.na(junk1$Cover_Type), 2] <- 2
junk1[is.na(junk1$Cover_Type), ]

summary(junk1)


write.csv(junk1, "output_merged1.csv", row.names = FALSE)
###Score: 0.70826

#Step5: Boosting - GBM
gbm1 <- gbm(Cover_Type ~ .-Id, data = trainall, n.trees = 500, 
            shrinkage = 0.01, distribution = "multinomial", 
            interaction.depth = 4)

###Failed to perform validation
validall$Result_gbm1 <- predict(gbm1, newdata = validall, n.trees = 500)
confusionMatrix(validall$Result_gbm1, validall$Cover_Type)

###Apply gbm1 model to FinalTestMerge data
FinalTestMerge$ResultGBM1 <- predict(gbm1, newdata = FinalTestMerge,
                                     n.trees = 500)
output_gbm1 <- cbind(FinalTestMerge$Id, FinalTestMerge$ResultGBM1)
summary(FinalTestMerge)


#Step6: Curve rf model by factor importance
varImpPlot(rf_merge)

###Remove Soil Type
dat2 <- trainmerge[, -c(1, 4, 14)]
sample <- sample(nrow(dat2), size = floor(0.8 * nrow(dat2)))
train2 <- dat2[sample, ]
test2 <- dat2[-sample, ]

rf2 <- randomForest(Cover_Type ~., data = train2, importance = TRUE,
                    mtry = floor(sqrt(length(train2))))

test2$Result <- predict(rf2, test2)
confusionMatrix(test2$Result, test2$Cover_Type)   #Accuracy = 0.8522

FinalTestMerge$ResultRf2 <- predict(rf2, FinalTestMerge)
output_rf2 <- cbind(FinalTestMerge$Id, FinalTestMerge$ResultRf2) %>% as.data.frame()
colnames(output_rf2) <- c("Id", "Cover_Type")
write.csv(output_rf2, "output_rf2.csv", row.names = FALSE)

#Score: 0.71823


#Final Score: 0.69831 -unmerged1
#Final Score: 0.69284 - unmerged2

### Model 2: Apply rf to merged data (merged Wilderness & Soil Types)
# rf_merge <- randomForest(Cover_Type ~ .-Id, data = trainfrommerge,
#                          mtry = floor(sqrt(13)), importance = TRUE)
# 
# #Accuracy = 80.2%
# summary(FinalTestMerge) #Has NA data in Soil Type
# 
# ###Get rid of NA data in Soil Type
# FinalTestMerge[is.na(FinalTestMerge$Soil_Type) == NULL,]
# summary(FinalTestMerge)
# nrow(FinalTestMerge)
# View(FinalTestMerge)
# 
# which(FinalTestMerge$Soil_Type == "NA")
# na.omit(FinalTestMerge)
# nrow(FinalTestMerge)
# 
# FinalTestMerge$Result1 <- predict(rf_merge, newdata = FinalTestMerge)
# output_merged <- cbind(FinalTestMerge$Id, FinalTestMerge$Result1) %>% as.data.frame()
# unique(FinalTestMerge$Result1) 
# 
# 
# 
# 
# colnames(output_merged) <- c("Id", "Cover_Type")
# write.csv(output_merged, "output_merged1.csv", row.names = FALSE) 
#Step4 Change codes to names (Wilderness & Cover_Type)


#Step5 several plots
#### 1. Boxplot (Elevation v.s. Cover_Type)
#### To see how do different cover types fall into a specific elevation

#### 2. Aspect v.s. Cover_Type
#### To see how do different cover types of forests grow in different direction


#### 3. Barplot (Wilderness v.s. Cover_Type)
#### To see how different types of forests grow in different wilderness


#### 4. Barplot (Soil type v.s. Cover_Type)
#### To see how different types of forests grow in different soil types





#Step7 Random Forest model - two models
#### Know the accuracy, feature importance toward this model
#### Use validation set to confirm the robustness of this model

### Model 1: Apply rf to unmerged data (Not merging Wilderness & Soil Type)


### Model 2: Apply rf to merged data (merged Wilderness & Soil Types)


#Step8 Make prediction to the true test data



###########################Old
# 
# #Step1 Merge - trainold, testold, merge(dat), train_m1, test_m1
# trainold <- read.csv("train.csv")
# testold <- read.csv("test.csv")
# testold$Cover_Type <- 1
# merge <- rbind(trainold, testold)
# dat <- merge
# 
# View(merge)
# nrow(trainold) #15120
# nrow(testold)  #565892
# nrow(merge) #581012
# 
# #Step2 Clean data
# for(i in 12 : 56) {
#   dat[, i] <- as.factor(dat[, i])
# }
# 
# for(i in 12 : 56) {
#   trainold[, i] <- as.factor(trainold[, i])
# }
# nrow(dat)
# colnames(dat)
# str(dat)
# 
# #Step3 Merge Columns - Soil_Type (from 16 to 55)
# trainold$Soil_Type <- 

#Step4 Merge Columns - Wilderness_Area (fro 12 to 15)


# #Step3 Split data - trainold, testold, merge(dat), train_m1, test_m1
# sample <- sample(nrow(trainold), size = floor(0.8 * nrow(trainold)))
# train_m1 <- trainold[sample, ]
# test_m1 <- trainold[-sample, ]
# nrow(test_m1)  #3024
# 
# true_test <- dat[15121 : nrow(dat), ]
# summary(true_test)
# str(true_test)
# str(trainold)
# colnames(true_test)
# 
# #Step4 Fit models
# model_rf <- randomForest(Cover_Type ~., data = train_m1, mtry = 7, importance = TRUE)
# ### m = floor(sqrt(55))
# 
# model_bagging <- randomForest(Cover_Type ~., data = train, mtry = 55, importance = TRUE)
# 
# model_boost <- gbm(Cover_Type ~., data = train, n.trees = 500,
#                    importance = TRUE, distribution = "multinomial",
#                    interaction.depth = 4)
# 
# #Step5 Predict1 - use data "test_m1" and "true_test"
# ###Use train data to make a model
# test_m1$Result <- predict(model_rf, newdata = test_m1)
# table(test_m1$Result, test_m1$Cover_Type)
# 
# ###Apply the above model to a true test set - Failed
# true_test$Result <- predict(model_rf, newdata = true_test)

#
# #Step6 AUC - Failed, categorical cannot find AUC/ROC
# library(pROC)
# roc_m1 <- roc(test_m1$Result, test_m1$Cover_Type)

# #Step7 Write.csv, Upload
# 
