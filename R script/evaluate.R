#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(randomForest)
print("Loading 'randomForest'")
library(party)
print("Loading 'party'")

#rffile1 <- paste(args[1],".rflog.Rdata",sep="")
#cf1 <- get(load(rffile1))
#print("Importance Value Selection")
#varimp(cf1)
#cf1

rffile2 <- paste(args[1],".rfimportance.Rdata",sep="")
mosq.rf <- get(load(rffile2))
print("Importance Value Selection 2")
mosq.imp <- importance(mosq.rf)
mosq.imp
mosq.imp <- importance(mosq.rf, type=1)
mosq.imp
q()
