#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
inputData <- read.csv(paste(args[1],".evals.csv",sep=""))
truth <- read.csv(paste(args[1],".truth.csv",sep=""))
class <- as.numeric(inputData$Evals)
class.act <- as.numeric(truth$Truth)
acc <- mean(class.act==class)
print(paste("Acc(exactly) ", acc))
acc <- mean((class.act==class | class.act+1==class))
print(paste("Acc(+1) ", acc))
acc <- mean((class.act==class | class.act+1==class | class.act==class+1))
print(paste("Acc(-1 +1) ", acc))
acc <- mean(class-class.act)
print(paste("BIAS = ",acc))
acc <- sqrt(mean((class-class.act)^2))
print(paste("RMSE = ",acc))
acc <- mean(abs(class-class.act))
print(paste("MAE = ",acc))
acc <- cor(class,class.act)
print(paste("R = ",acc))
acc <- mean(2^class*5-2^class.act*5)
print(paste("BIAS = ",acc))
acc <- sqrt(mean((2^class*5-2^class.act*5)^2))
print(paste("RMSE = ",acc))
acc <- mean(abs(2^class*5-2^class.act*5))
print(paste("MAE = ",acc))
acc <- cor(2^class*5,2^class.act*5)
print(paste("R = ",acc))
nclass <- class
nclass[class=1] <- 1
nclass[class=2] <- 1
nclass[class=3] <- 2
nclass[class=4] <- 3
nclass[class=5] <- 3
nclass[class>5] <- 4
nclass.act <- class.act
nclass.act[class.act=1] <- 1
nclass.act[class.act=2] <- 1
nclass.act[class.act=3] <- 2
nclass.act[class.act=4] <- 3
nclass.act[class.act=5] <- 3
nclass.act[class.act>5] <- 4
acc <- mean((nclass.act==nclass | nclass.act+1==nclass | nclass.act==nclass+1))
print(paste("Acc(mapping) ", acc))
#table(true=actuals,pred=predicted)
#table(true=actuals,pred=predicted)
