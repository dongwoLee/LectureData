#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(randomForest)
library(party)

print(paste("Read",args[1]))
mosq.data <- read.csv(args[1])
inputData <- mosq.data
testData <- inputData
testData$LandUse <- factor(testData$LandUse,levels=c(1:3))
testData$PrevMosq<- factor(testData$PrevMosq,levels=c(1:8))
head(testData$PrevMosq)
#inputData[,1:6] <- as.numeric(inputData[,1:6])
#train <- sample(1:nrow(inputData), 0.7*nrow(inputData))
#trainData <- inputData[train,]
#testData <- inputData[-train,]
#testData <- inputData[340:350,]
print(paste("Loading ",args[2]))
Model <- get(load(args[2]))
Model
print(paste("Loaded ",args[2]))
print("Predict...")
#inputData$PMosq <- as.numeric(inputData$PMosq)
#testData <- subset(inputData,Class=="C")
class.act <- ceiling(log2(testData$Mosq/10+0.01))
class.act[class.act<1] <- 1
class.act[class.act>8] <- 8
class.act <- ceiling(mosq.data$Mosq/100+0.01)
#proximity(Model, newdata = testData)
predicted <- predict(Model, newdata = testData, type="prob",OOB=TRUE)
probs <- t(data.frame(matrix(unlist(predicted),nrow=8)))
rownames(probs) <- NULL
head(probs)
predicted <- t(apply(probs,1,function(row) { 
    c(which.max(row),max(row)) 
}))
#probs <- t(apply(predicted,1,function(row) {
#  mosq <- row
#  level <- mosq
#  if( level < 20 ) { level = 20 }
#  level = ceiling(log2(level/10))
#  if( level > 8 ) { level = 8 }
#  radius = ((2^level*10)-(2^(level-1)*10))/2
#  if( radius < 10 ) { radius = 10 }
#  center = 2^level*10 - radius
#  dist = 1-abs(mosq-center)/radius
#  c(level,dist,center,mosq)
#}))
head(predicted)
#t(apply(probs,1,function(row) { 
#  c(which.max(row),max(row)) 
#}))
q()
predicted <- predict(Model, newdata = testData)
head(predicted)
class <- ceiling(log2(predicted/10+0.01))
class[class>8] <- 8
class[class<1] <- 1
#class <- ceiling(predicted/100+0.01)
#class[class>20] <- 20
range <- (10*(2^class))-ifelse(class>1,(10*(2^(class-1))),0)
center <- ifelse(class>1,(10*(2^(class-1))),0)+range/2
distance <- (predicted-center)/range
colnames(distance) <- "Distance"
ret <- data.frame(class,class.act,predicted,testData$Mosq,distance)
colnames(ret) <- c("Mosq","Actual","Mosq#","Actual#","Distance")
print(paste("Writing...",args[3],".debug.csv"))
write.csv(ret,paste(args[3],".debug.csv"),quote=FALSE,row.names=FALSE)
ret <- data.frame(class,class.act,distance)
colnames(ret) <- c("Mosq","Actual","Distance")
print(paste("Writing...",args[3]))
write.csv(ret,args[3],quote=FALSE,row.names=FALSE)
acc <- mean(class.act==class)
print(paste("HIT = ",acc*100,"%"))
acc <- mean(class-class.act)
print(paste("BIAS = ",acc))
acc <- sqrt(mean((class-class.act)^2))
print(paste("RMSE = ",acc))
acc <- mean(abs(class-class.act))
print(paste("MAE = ",acc))
acc <- cor(class,class.act)
print(paste("R = ",acc))
#table(true=actuals,pred=predicted)
