#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
mosq.data <- read.csv(paste(args[1],".txt",sep=""))
inputData <- mosq.data#[mosq.data[,"Mosq"]>640,]
#inputData$Mosq <- log2(inputData$Mosq/10+0.01)

library(tree)
library(randomForest)
library(party)

set.seed(1231)
print("Data Sampling")
train <- sample(1:nrow(inputData), 0.7*nrow(inputData))
trainData <- inputData[train,]
train.Mosq <- trainData$Mosq;
trainData$Mosq[trainData$Mosq<20] <- 20
trainData$Mosq <- ceiling(log2(trainData$Mosq/10))
trainData$Mosq[trainData$Mosq>8] <- 8
trainData$Mosq <- factor(trainData$Mosq,levels=c(1:8))
trainData$PrevMosq[trainData$PrevMosq<20] <- 20
trainData$PrevMosq <- ceiling(log2(trainData$PrevMosq/10))
trainData$PrevMosq[trainData$PrevMosq>8] <- 8
trainData$PrevMosq <- factor(trainData$PrevMosq,levels=c(1:8))
trainData$LandUse <- factor(trainData$LandUse,levels=c(1:3))
testData <- inputData[-train,]
test.Mosq <- testData$Mosq;
testData$Mosq[testData$Mosq<20] <- 20
testData$Mosq <- ceiling(log2(testData$Mosq/10))
testData$Mosq[testData$Mosq>8] <- 8
testData$Mosq <- factor(testData$Mosq,levels=c(1:8))
testData$PrevMosq[testData$PrevMosq<20] <- 20
testData$PrevMosq <- ceiling(log2(testData$PrevMosq/10))
testData$PrevMosq[testData$PrevMosq>8] <- 8
testData$PrevMosq <- factor(testData$PrevMosq,levels=c(1:8))
testData$LandUse <- factor(testData$LandUse,levels=c(1:3))
# 저장
  rffile <- paste(args[1],".testset.pmosq.log.origin.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  rffile <- paste(args[1],".trainset.pmosq.log.origin.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)

print("Learning... with #Mosq of yesterday(PrevMosq) and apply log function")
# 데이터 학습(전날모기데이터 + 로그미리 적용)
cTreeMod <- ctree(Mosq ~ ., data = trainData)
  rffile <- paste(args[1],".ctree.pmosq.log.Rdata",sep="")
  save(cTreeMod,file=rffile)

# 평가: 학습데이터로 평가
actuals <- trainData$Mosq
predicted <- predict(cTreeMod, newdata = trainData, type="prob")
  probs <- t(as.data.frame(predicted))
  rownames(probs) <- NULL
  predicted <- t(apply(probs,1,function(row) { 
    c(which.max(row),max(row)) 
  }))
  # 평가 결과 저장.
  trainData$Mosq <- NULL
  trainData$Evals<- predicted[,1]
  trainData$Prob <- predicted[,2]
  rffile <- paste(args[1],".trainset.pmosq.log.evals.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장.
  rffile <- paste(args[1],".trainset.pmosq.log.truth.csv",sep="")
  ret <- data.frame(actuals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with trainData = ",mean(actuals == predicted[,1])))
  # 복원
  trainData$Mosq <- actuals
  trainData$Evals<- NULL
  trainData$Prob <- NULL

# 평가: 테스테셋으로 평가
actuals <- testData$Mosq
predicted <- predict(cTreeMod, newdata = testData, type="prob")
  probs <- t(as.data.frame(predicted))
  rownames(probs) <- NULL
  predicted <- t(apply(probs,1,function(row) { 
    c(which.max(row),max(row)) 
  }))
  # 평가 결과 저장.
  testData$Mosq <- NULL
  testData$Evals<- predicted[,1]
  testData$Prob <- predicted[,2]
  rffile <- paste(args[1],".testset.pmosq.log.evals.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장.
  rffile <- paste(args[1],".testset.pmosq.log.truth.csv",sep="")
  ret <- data.frame(actuals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with testData = ",mean(actuals == predicted[,1])))
  # 복원
  testData$Mosq <- actuals
  testData$Evals<- NULL
  testData$Prob <- NULL

print("Learning... with #Mosq of yesterday(PrevMosq)")
# 복원
  trainData$Mosq <- train.Mosq
  testData$Mosq <- test.Mosq
# 저장
  rffile <- paste(args[1],".testset.pmosq.origin.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  rffile <- paste(args[1],".trainset.pmosq.origin.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)
# 데이터 학습(전날모기데이터 + 로그미리 적용안함)
cTreeMod <- ctree(Mosq ~ ., data = trainData)
  rffile <- paste(args[1],".ctree.pmosq.Rdata",sep="")
  save(cTreeMod,file=rffile)

# 실제 데이터 로그식으로 변경.
actuals <- trainData$Mosq
  evals <- actuals
  evals[evals<20] <- 20
  evals <- ceiling(log2(evals/10))
  evals[evals>8] <- 8

# 평가: 학습셋으로 평가(전날모기데이터)
predicted <- predict(cTreeMod, newdata = trainData)
  predicted <- t(apply(predicted,1,function(row) {
    mosq <- row
    level <- mosq
    if( level < 20 ) { level = 20 }
    level = ceiling(log2(level/10))
    if( level > 8 ) { level = 8 }
    radius = ((2^level*10)-(2^(level-1)*10))/2
    if( radius < 10 ) { radius = 10 }
    center = 2^level*10 - radius
    dist = 1-abs(mosq-center)/radius
    if( dist < 0 ) { dist = 0.000000001 }
    c(level,dist)
  }))
  # 평가 결과 저장
  trainData$Mosq <- NULL
  trainData$Evals<- predicted[,1]
  trainData$Prob <- predicted[,2]
  rffile <- paste(args[1],".trainset.pmosq.evals.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장
  rffile <- paste(args[1],".trainset.pmosq.truth.csv",sep="")
  ret <- data.frame(evals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with trainData = ",mean(evals == predicted[,1])))
  # 복원
  trainData$Mosq <- actuals
  trainData$Evals<- NULL
  trainData$Prob <- NULL

# 입력 데이터 로그식으로 변경.
actuals <- testData$Mosq
  evals <- actuals
  evals[evals<20] <- 20
  evals <- ceiling(log2(evals/10))
  evals[evals>8] <- 8

# 평가: 테스트셋으로 평가(전날모기데이터)
predicted <- predict(cTreeMod, newdata = testData)
  predicted <- t(apply(predicted,1,function(row) {
    mosq <- row
    level <- mosq
    if( level < 20 ) { level = 20 }
    level = ceiling(log2(level/10))
    if( level > 8 ) { level = 8 }
    radius = ((2^level*10)-(2^(level-1)*10))/2
    if( radius < 10 ) { radius = 10 }
    center = 2^level*10 - radius
    dist = 1-abs(mosq-center)/radius
    if( dist < 0 ) { dist = 0.000000001 }
    c(level,dist)
  }))
  # 평가 결과 저장
  testData$Mosq <- NULL
  testData$Evals<- predicted[,1]
  testData$Prob <- predicted[,2]
  rffile <- paste(args[1],".testset.pmosq.evals.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장
  rffile <- paste(args[1],".testset.pmosq.truth.csv",sep="")
  ret <- data.frame(evals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with testData = ",mean(evals == predicted[,1])))
  # 복원
  testData$Mosq <- actuals
  testData$Evals<- NULL
  testData$Prob <- NULL


# 전날모기데이터 제거
inputData$PrevMosq <- NULL
  trainData <- inputData[train,]
  train.Mosq <- trainData$Mosq;
  trainData$Mosq[trainData$Mosq<20] <- 20
  trainData$Mosq <- ceiling(log2(trainData$Mosq/10))
  trainData$Mosq[trainData$Mosq>8] <- 8
  trainData$Mosq <- factor(trainData$Mosq,levels=c(1:8))
  trainData$LandUse <- factor(trainData$LandUse,levels=c(1:3))
  testData <- inputData[-train,]
  test.Mosq <- testData$Mosq;
  testData$Mosq[testData$Mosq<20] <- 20
  testData$Mosq <- ceiling(log2(testData$Mosq/10))
  testData$Mosq[testData$Mosq>8] <- 8
  testData$Mosq <- factor(testData$Mosq,levels=c(1:8))
  testData$LandUse <- factor(testData$LandUse,levels=c(1:3))
# 저장
  rffile <- paste(args[1],".testset.log.origin.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  rffile <- paste(args[1],".trainset.log.origin.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)

print("Learning... without #Mosq of yesterday(PrevMosq) + apply log funciton")
# 데이터 학습(전날모기데이터 + 로그미리 적용)
cTreeMod <- ctree(Mosq ~ ., data = trainData)
  rffile <- paste(args[1],".ctree.log.Rdata",sep="")
  save(cTreeMod,file=rffile)

# 평가: 학습데이터로 평가
actuals <- trainData$Mosq
predicted <- predict(cTreeMod, newdata = trainData,type="prob")
  probs <- t(as.data.frame(predicted))
  rownames(probs) <- NULL
  predicted <- t(apply(probs,1,function(row) { 
    c(which.max(row),max(row)) 
  }))
  # 평가 결과 저장.
  trainData$Mosq <- NULL 
  trainData$Evals<- predicted[,1]
  trainData$Prob <- predicted[,2]
  rffile <- paste(args[1],".trainset.log.evals.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장.
  rffile <- paste(args[1],".trainset.log.truth.csv",sep="")
  ret <- data.frame(actuals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with trainData = ",mean(actuals == predicted[,1])))
  # 복원
  trainData$Mosq <- actuals
  trainData$Evals<- NULL
  trainData$Prob <- NULL

# 평가: 테스테셋으로 평가
actuals <- testData$Mosq
predicted <- predict(cTreeMod, newdata = testData, type="prob")
  probs <- t(as.data.frame(predicted))
  rownames(probs) <- NULL
  predicted <- t(apply(probs,1,function(row) { 
    c(which.max(row),max(row)) 
  }))
  # 평가 결과 저장.
  testData$Mosq <- NULL
  testData$Evals<- predicted[,1]
  testData$Prob <- predicted[,2]
  rffile <- paste(args[1],".testset.log.evals.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장.
  rffile <- paste(args[1],".testset.log.truth.csv",sep="")
  ret <- data.frame(actuals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with testData = ",mean(actuals == predicted[,1])))
  # 복원
  testData$Mosq <- actuals
  testData$Evals<- NULL
  testData$Prob <- NULL

print("Learning... without #Mosq of yesterday(PrevMosq)")
# 복원
  trainData$Mosq <- train.Mosq
  testData$Mosq <- test.Mosq
# 저장
  rffile <- paste(args[1],".testset.origin.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  rffile <- paste(args[1],".trainset.origin.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)
# 데이터 학습(전날모기데이터 + 로그미리 적용안함)
cTreeMod <- ctree(Mosq ~ ., data = trainData)
  rffile <- paste(args[1],".ctree.Rdata",sep="")
  save(cTreeMod,file=rffile)

# 실제 데이터 로그식으로 변경.
actuals <- trainData$Mosq
  evals <- actuals
  evals[evals<20] <- 20
  evals <- ceiling(log2(evals/10))
  evals[evals>8] <- 8

# 평가: 학습셋으로 평가
predicted <- predict(cTreeMod, newdata = trainData)
  predicted <- t(apply(predicted,1,function(row) {
    mosq <- row
    level <- mosq
    if( level < 20 ) { level = 20 }
    level = ceiling(log2(level/10))
    if( level > 8 ) { level = 8 }
    radius = ((2^level*10)-(2^(level-1)*10))/2
    if( radius < 10 ) { radius = 10 }
    center = 2^level*10 - radius
    dist = 1-abs(mosq-center)/radius
    if( dist < 0 ) { dist = 0.000000001 }
    c(level,dist)
  }))
  # 평가 결과 저장
  trainData$Mosq <- NULL
  trainData$Evals<- predicted[,1]
  trainData$Prob <- predicted[,2]
  rffile <- paste(args[1],".trainset.evals.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장
  rffile <- paste(args[1],".trainset.truth.csv",sep="")
  ret <- data.frame(evals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with trainData = ",mean(evals == predicted[,1])))
  # 복원
  trainData$Mosq <- actuals
  trainData$Evals<- NULL
  trainData$Prob <- NULL

# 입력 데이터 로그식으로 변경.
actuals <- testData$Mosq
  evals <- actuals
  evals[evals<20] <- 20
  evals <- ceiling(log2(evals/10))
  evals[evals>8] <- 8

# 평가: 테스트셋으로 평가
predicted <- predict(cTreeMod, newdata = testData)
  predicted <- t(apply(predicted,1,function(row) {
    mosq <- row
    level <- mosq
    if( level < 20 ) { level = 20 }
    level = ceiling(log2(level/10))
    if( level > 8 ) { level = 8 }
    radius = ((2^level*10)-(2^(level-1)*10))/2
    if( radius < 10 ) { radius = 10 }
    center = 2^level*10 - radius
    dist = 1-abs(mosq-center)/radius
    if( dist < 0 ) { dist = 0.000000001 }
    c(level,dist)
  }))
  # 평가 결과 저장
  testData$Mosq <- NULL
  testData$Evals<- predicted[,1]
  testData$Prob <- predicted[,2]
  rffile <- paste(args[1],".testset.evals.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장
  rffile <- paste(args[1],".testset.truth.csv",sep="")
  ret <- data.frame(evals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with testData = ",mean(evals == predicted[,1])))
  # 복원
  testData$Mosq <- actuals
