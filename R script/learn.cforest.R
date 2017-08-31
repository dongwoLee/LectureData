#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
mosq.data <- read.csv(args[1])
inputData <- mosq.data
#inputData$Mosq <- log2(inputData$Mosq/10+0.01)

library(randomForest)
library(party)

set.seed(91)
print("Data Sampling")
train <- sample(1:nrow(inputData), 0.7*nrow(inputData))
trainData <- inputData[train,]
testData <- inputData[-train,]
print("Learning...")
cForestMod <- cforest(Mosq ~ ., data = trainData)
rffile <- paste(args[1],".learn.Rdata",sep="")
save(cForestMod,file=rffile)
rffile <- paste(args[1],".testData.csv",sep="")
write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
actuals <- testData$Mosq
predicted <- predict(cForestMod, newdata = testData)

table(true=actuals,pred=predicted)
