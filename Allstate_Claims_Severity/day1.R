rm(list=ls())
set.seed(666)
library(doMC)
registerDoMC(cores = 8)
library(caret); library(kernlab); library(ISLR); library(RANN);
library(DMwR);library(MASS); library(randomForest); library(e1071)
library(dplyr)


Train_FULL=read.table(file="./train.csv", header = TRUE, sep=",")
test=read.csv(file="./test.csv", header = TRUE)

Train_v2 = Train_FULL

features_corr <- findCorrelation(cor(Train_v2[,-c(1,132)]), cutoff=0.2)


