#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
mosq.data <- read.csv(args[1])
inputData <- mosq.data

library(randomForest)
print("Loading 'randomForest'")
library(party)
print("Loading 'party'")

print("Importance Value Selection")
rffile1 <- paste(args[1],".rflog.Rdata",sep="")
print(rffile1)
#cf1 <- randomForest(Mosq ~ ., inputData, keep.forest=FALSE, ntree=500,log="y")
cf1 <- cforest(Mosq ~ ., data=inputData, control=cforest_unbiased(mtry=0,ntree=500))
save(cf1,file=rffile1)
varimp(cf1)
#varimp(cf1, conditional=TRUE)
#varimpAUC(cf1)

print("Importance Value Selection 2")
rffile2 <- paste(args[1],".rfimportance.Rdata",sep="")
print(rffile2)
set.seed(654)
mosq.rf <- randomForest(Mosq ~ .,data=inputData,ntree=500,keep.forest=FALSE,importance=TRUE)
save(mosq.rf,file=rffile2)
mosq.imp <- importance(mosq.rf)
mosq.imp
mosq.imp <- importance(mosq.rf, type=1)
mosq.imp
q()
print("Prediction")
set.seed(91)
print("Data Sampling")
train <- sample(1:nrow(inputData), 0.7*nrow(inputData))
trainData <- inputData[train,]
testData <- inputData[-train,]
print("Learning...")
cForestMod <- cforest(Mosq ~ ., data = trainData)
actuals <- testData$Mosq
predicted <- predict(cForestMod, newdata = testData)

table(true=actuals,pred=predicted)
