#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
mosq.data <- read.csv(paste(args[1],".txt",sep=""))
inputData <- mosq.data
#inputData <- mosq.data[mosq.data[,"Mosq"]<320,]
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
trainData$Mosq <- as.factor(trainData$Mosq)
trainData$PrevMosq[trainData$PrevMosq<20] <- 20
trainData$PrevMosq <- ceiling(log2(trainData$PrevMosq/10))
trainData$PrevMosq[trainData$PrevMosq>8] <- 8
trainData$PrevMosq <- as.factor(trainData$PrevMosq)
trainData$LandUse <- as.factor(trainData$LandUse)
testData <- inputData[-train,]
test.Mosq <- testData$Mosq;
testData$Mosq[testData$Mosq<20] <- 20
testData$Mosq <- ceiling(log2(testData$Mosq/10))
testData$Mosq[testData$Mosq>8] <- 8
testData$Mosq <- as.factor(testData$Mosq)
testData$PrevMosq[testData$PrevMosq<20] <- 20
testData$PrevMosq <- ceiling(log2(testData$PrevMosq/10))
testData$PrevMosq[testData$PrevMosq>8] <- 8
testData$PrevMosq <- as.factor(testData$PrevMosq)
testData$LandUse <- as.factor(testData$LandUse)
# 저장
  rffile <- paste(args[1],".testset.pmosq.log.origin.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  rffile <- paste(args[1],".trainset.pmosq.log.origin.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)

print("Learning... with #Mosq of yesterday(PrevMosq) and apply log function")
# 데이터 학습(전날모기데이터 + 로그미리 적용)
cForestMod <- cforest(Mosq ~ .,data=trainData)
#, control=cforest_unbiased(mtry=2,ntree=500))
  rffile <- paste(args[1],".randomForest.pmosq.log.Rdata",sep="")
  save(cForestMod,file=rffile)
cForestMod
# 평가: 학습데이터로 평가
actuals <- trainData$Mosq
predicted <- proximity(cForestMod, newdata = trainData)
predicted
q()
predicted <- predict(cForestMod, newdata = trainData)
  # 평가 결과 저장.
  trainData$Mosq <- predicted
  rffile <- paste(args[1],".trainset.pmosq.log.evals.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장.
  rffile <- paste(args[1],".trainset.pmosq.log.truth.csv",sep="")
  ret <- data.frame(actuals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with trainData = ",mean(actuals == predicted)))
  # 복원
  trainData$Mosq <- actuals

# 평가: 테스테셋으로 평가
actuals <- testData$Mosq
predicted <- predict(cForestMod, newdata = testData)
  # 평가 결과 저장.
  testData$Mosq <- predicted
  rffile <- paste(args[1],".testset.pmosq.log.evals.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장.
  rffile <- paste(args[1],".testset.pmosq.log.truth.csv",sep="")
  ret <- data.frame(actuals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with testData = ",mean(actuals == predicted)))
  # 복원
  testData$Mosq <- actuals

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
cForestMod <- randomForest(Mosq ~ ., data = trainData)
  rffile <- paste(args[1],".randomForest.pmosq.Rdata",sep="")
  save(cForestMod,file=rffile)

# 실제 데이터 로그식으로 변경.
actuals <- trainData$Mosq
  evals <- actuals
  evals[evals<20] <- 20
  evals <- ceiling(log2(evals/10))
  evals[evals>8] <- 8

# 평가: 학습셋으로 평가(전날모기데이터)
predicted <- predict(cForestMod, newdata = trainData)
  evaluated <- predicted
  evaluated[evaluated<20] <- 20
  evaluated <- ceiling(log2(evaluated/10))
  evaluated[evaluated>8] <- 8
  # 평가 결과 저장
  trainData$Mosq <- evaluated
  rffile <- paste(args[1],".trainset.pmosq.evals.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장
  rffile <- paste(args[1],".trainset.pmosq.truth.csv",sep="")
  ret <- data.frame(evals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with trainData = ",mean(evals == evaluated)))
  # 복원
  trainData$Mosq <- actuals

# 입력 데이터 로그식으로 변경.
actuals <- testData$Mosq
  evals <- actuals
  evals[evals<20] <- 20
  evals <- ceiling(log2(evals/10))
  evals[evals>8] <- 8

# 평가: 테스트셋으로 평가(전날모기데이터)
predicted <- predict(cForestMod, newdata = testData)
  evaluated <- predicted
  evaluated[evaluated<20] <- 20
  evaluated <- ceiling(log2(evaluated/10))
  evaluated[evaluated>8] <- 8
  # 평가 결과 저장
  testData$Mosq <- evaluated
  rffile <- paste(args[1],".testset.pmosq.evals.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장
  rffile <- paste(args[1],".testset.pmosq.truth.csv",sep="")
  ret <- data.frame(evals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with testData = ",mean(evals == evaluated)))
  # 복원
  testData$Mosq <- actuals


# 전날모기데이터 제거
inputData$PrevMosq <- NULL
  trainData <- inputData[train,]
  train.Mosq <- trainData$Mosq;
  trainData$Mosq[trainData$Mosq<20] <- 20
  trainData$Mosq <- ceiling(log2(trainData$Mosq/10))
  trainData$Mosq[trainData$Mosq>8] <- 8
  trainData$Mosq <- as.factor(trainData$Mosq)
  trainData$LandUse <- as.factor(trainData$LandUse)
  testData <- inputData[-train,]
  test.Mosq <- testData$Mosq;
  testData$Mosq[testData$Mosq<20] <- 20
  testData$Mosq <- ceiling(log2(testData$Mosq/10))
  testData$Mosq[testData$Mosq>8] <- 8
  testData$Mosq <- as.factor(testData$Mosq)
  testData$LandUse <- as.factor(testData$LandUse)
# 저장
  rffile <- paste(args[1],".testset.log.origin.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  rffile <- paste(args[1],".trainset.log.origin.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)

print("Learning... without #Mosq of yesterday(PrevMosq) + apply log funciton")
# 데이터 학습(전날모기데이터 + 로그미리 적용)
cForestMod <- randomForest(Mosq ~ ., data = trainData)
  rffile <- paste(args[1],".randomForest.Rdata",sep="")
  save(cForestMod,file=rffile)

# 평가: 학습데이터로 평가
actuals <- trainData$Mosq
predicted <- predict(cForestMod, newdata = trainData)
  # 평가 결과 저장.
  trainData$Mosq <- predicted
  rffile <- paste(args[1],".trainset.log.evals.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장.
  rffile <- paste(args[1],".trainset.log.truth.csv",sep="")
  ret <- data.frame(actuals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with trainData = ",mean(actuals == predicted)))
  # 복원
  trainData$Mosq <- actuals

# 평가: 테스테셋으로 평가
actuals <- testData$Mosq
predicted <- predict(cForestMod, newdata = testData)
  # 평가 결과 저장.
  testData$Mosq <- predicted
  rffile <- paste(args[1],".testset.log.evals.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장.
  rffile <- paste(args[1],".testset.log.truth.csv",sep="")
  ret <- data.frame(actuals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with testData = ",mean(actuals == predicted)))
  # 복원
  testData$Mosq <- actuals

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
cForestMod <- randomForest(Mosq ~ ., data = trainData)
  rffile <- paste(args[1],".randomForest.Rdata",sep="")
  save(cForestMod,file=rffile)

# 실제 데이터 로그식으로 변경.
actuals <- trainData$Mosq
  evals <- actuals
  evals[evals<20] <- 20
  evals <- ceiling(log2(evals/10))
  evals[evals>8] <- 8

# 평가: 학습셋으로 평가
predicted <- predict(cForestMod, newdata = trainData)
  evaluated <- predicted
  evaluated[evaluated<20] <- 20
  evaluated <- ceiling(log2(evaluated/10))
  evaluated[evaluated>8] <- 8
  # 평가 결과 저장
  trainData$Mosq <- evaluated
  rffile <- paste(args[1],".trainset.evals.csv",sep="")
  write.csv(trainData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장
  rffile <- paste(args[1],".trainset.truth.csv",sep="")
  ret <- data.frame(evals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with trainData = ",mean(evals == evaluated)))
  # 복원
  trainData$Mosq <- actuals

# 입력 데이터 로그식으로 변경.
actuals <- testData$Mosq
  evals <- actuals
  evals[evals<20] <- 20
  evals <- ceiling(log2(evals/10))
  evals[evals>8] <- 8

# 평가: 테스트셋으로 평가
predicted <- predict(cForestMod, newdata = testData)
  evaluated <- predicted
  evaluated[evaluated<20] <- 20
  evaluated <- ceiling(log2(evaluated/10))
  evaluated[evaluated>8] <- 8
  # 평가 결과 저장
  testData$Mosq <- evaluated
  rffile <- paste(args[1],".testset.evals.csv",sep="")
  write.csv(testData,rffile,quote=FALSE,row.names=FALSE)
  # 정답 파일 저장
  rffile <- paste(args[1],".testset.truth.csv",sep="")
  ret <- data.frame(evals)
  colnames(ret) <- c("Truth")
  write.csv(ret,rffile,quote=FALSE,row.names=FALSE)
  print(paste("HIT with testData = ",mean(evals == evaluated)))
  # 복원
  testData$Mosq <- actuals
