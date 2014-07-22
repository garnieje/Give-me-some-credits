# process to compute the classification of the kaggle problem : give me some credit
# methods used are the random forest and the GBM

setwd("../workingDirectory")
library(ggplot2)
library(caret)
library(e1071)
library(glmnet)
library(caTools)
library(pROC)
library(gbm)
library(plyr)
source("formatData_final.R")

data <- read.table("cs-training.csv", sep = ",", dec = ".", header = T)
data <- formatData(data)
sapply(data, function(v) {sum(is.na(v))})



# create training and test data set
set.seed(1)
training <- sample(1:nrow(data), round(0.8 * nrow(data)))

data[["SeriousDlqin2yrs"]] <- factor(data[["SeriousDlqin2yrs"]])
dataFactor <- rep("", nrow(data))
dataFactor[data[["SeriousDlqin2yrs"]] == 1] <- "Bad"
dataFactor[data[["SeriousDlqin2yrs"]] == 0] <- "Good"
dataFactor <- factor(dataFactor)
data[["SeriousDlqin2yrs"]] <- dataFactor
dataTraining <- data[training, ]
dataTest <- data[-training, ]

# random forest


mtryRef <- round(sqrt(ncol(dataTest) - 2))
rfGrid <- expand.grid(mtry = c(round(mtryRef / 2), mtryRef, 2 * mtryRef)) # grid of the parameter to be tested in CV
ctrl <- trainControl(method = "cv",
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary, 
                     number = 5) # control parameter of the cross validation

# cross validation
set.seed(2)
rfDownsampled <- train(SeriousDlqin2yrs ~ . - Id, data = dataTraining,
                       method = "rf",
                       ntree = 200,
                       tuneGrid = rfGrid,
                       metric = "ROC",
                       trControl = ctrl,
                       ## sample by strata to take into account unbalanced nature of the data set
                       strata = factor(data$SeriousDlqin2yrs),
                       ## size of each sample, here they have the same size, so we created balanced sample
                       sampsize = c(5000, 5000))



predProb <- predict(rfDownsampled, dataTest, type = "prob")
colAUC(predProb[, 1], dataTest[, 2]) # AUC on the test set

# random forest on all the data set with the tuned parameter
RFSamplingAllSample <- randomForest(data[,-c(1,2)], factor(data$SeriousDlqin2yrs),
                                    strata = factor(data$SeriousDlqin2yrs),
                                    sampsize = c(5000, 5000),
                                    do.trace = T,importance = T, ntree = 200,mtry = 7,
                                    forest= T)

# look at the importance of the variable
varImp <- importance(RFSamplingAllSample, type = 2)
rownames(varImp)[order(varImp, decreasing = T)]

# prediction on the kaggle test data set
dataTestKaggle <- read.table("cs-test.csv", sep = ",", dec = ".", header = T)
dataTestKaggle <- formatData(dataTestKaggle)

predictionTestKaggle <- predict(RFSamplingAllSample, dataTestKaggle[, -c(1, 2)], type = "prob")
submission <- cbind(dataTestKaggle[, 1], predictionTestKaggle[, 1])
colnames(submission) <- c("Id", "probability")
write.table(submission, "20140305_RF_strataSampling_4.csv", sep = ",", dec = ".", row.names = F)


# GBM

weight <- rep(1, nrow(dataTraining))
weight[dataTraining$SeriousDlqin2yrs == 0] <- 0.1 

# GBM with "default" values of the parameter on the training set
gbm.test <- gbm(SeriousDlqin2yrs ~ . - Id, data = dataTraining,
                n.tree = 1000, 
                shrinkage = 0.01, 
                bag.fraction = 0.5, 
                interaction.depth = round(sqrt(ncol(dataTraining) - 2)), 
                weights = weight)

# look for the optimal number of tree on the test set
pred.gbm.test <- predict(gbm.test, dataTest, n.tree = seq(100, 1000, by = 100), type = "response")
apply(pred.gbm.test, 2, FUN = colAUC, dataTest[, 1])
which.max(colAUC(pred.gbm.test, dataTest[, 2]))

weightAll <- rep(1, nrow(data))
weightAll[data$SeriousDlqin2yrs == 0] <- 0.1

# GBM on all the data set
gbm.all <- gbm(SeriousDlqin2yrs ~ . - Id, data = data,
               n.tree = 800, 
               shrinkage = 0.01, 
               bag.fraction = 0.5, 
               interaction.depth = round(sqrt(ncol(dataTraining) - 2)), 
               weights = weightAll)

# tentative to determine the GBM's parameter by cross validation
# but it's too long to be computed
gbmGrid <- expand.grid(shrinkage = c(0.1, 0.01), 
                       n.tree = 1000,
                       interaction.depth = c(4, 7))


gbmCtrl <- trainControl(method = "cv",
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary, 
                        number = 5)

gbmTrain <- train(SeriousDlqin2yrs ~ . - Id, data = dataTraining,
                       method = "gbm",
                       tuneGrid = gbmGrid,
                       metric = "ROC",
                       trControl = gbmCtrl, 
                       weights = weight)


# prediction of the GBM on the kaggle test data set
predictionGbmKaggle <- predict(gbm.all, dataTestKaggle[, -c(1, 2)], type = "response", n.trees = 800)
submissionGbm <- cbind(dataTestKaggle[, 1], predictionGbmKaggle)
colnames(submissionGbm) <- c("Id", "probability")
write.table(submissionGbm, "20140306_GBM_1.csv", sep = ",", dec = ".", row.names = F)
