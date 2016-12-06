rm(list=ls())
set.seed(666)
library(doMC)
registerDoMC(cores = 8)
library(caret); library(kernlab); library(ISLR); library(RANN);
library(DMwR);library(MASS); library(randomForest); library(e1071)
library(dplyr)


train=read.csv(file="./train.csv", header = TRUE)
test=read.csv(file="./test.csv", header = TRUE)

str(train)
summary(train)

Train_v2 = train

features_corr <- findCorrelation(cor(Train_v2[,-c(6,7)]), cutoff=0.3)
features_corr
names(Train_v2[, -c(6,7)])[features_corr]

table(Train_v2$type)

set.seed(666)
inTrain = createDataPartition(y=Train_v2$type, p=0.8, list = FALSE)

Train_v3 = Train_v2[inTrain,]
Test_v3 = Train_v2[-inTrain,]

Train_v44 =rbind(Train_v2[Train_v2$type=="Ghost",],Train_v2[Train_v2$type=="Ghoul",],Train_v2[Train_v2$type=="Goblin",])
#qplot(1:nrow(Train_v44),Train_v44$color, col=Train_v44$type)


#RASISM
Train_v4 = Train_v3
Test_v4 = Test_v3

conditionTr_v4 = (Train_v3$color == "blue") | (Train_v3$color == "blood")
Train_v4$color = as.character(Train_v4$color)
Train_v4[conditionTr_v4,"color"] = NA
Train_v4$color= as.factor(Train_v4$color)

conditionTe_v4 = (Test_v3$color == "blue") | (Test_v3$color == "blood") 
Test_v4$color = as.character(Test_v4$color)
Test_v4[conditionTe_v4,"color"] = NA
Test_v4$color= as.factor(Test_v4$color)

Train_v4= knnImputation(Train_v4, k=100)
Test_v4 = knnImputation(Test_v4, k=25)


control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
set.seed(666)
results <- rfe(Train_v4[,-c(1,7)], Train_v4[,7], sizes=c(1:5), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)

rControl <- trainControl(method="repeatedcv", number=10, repeats=10, classProbs = TRUE)

fitControl <- trainControl(method = "cv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = FALSE
                           ## Evaluate performance using
                           ## the following function
                           )


set.seed(666)
model_NN = train(type~., data=Train_v4[,-c(1)],  method="nnet", preProcess="range", tuneLength=2, trControl=fitControl, maxit=2000)
PREDICTION_NN = predict(model_NN, newdata=Test_v4[,-c(1)])

confusionMatrix(PREDICTION_NN,Test_v4[,"type"])

set.seed(666)
model_avNN = train(type~., data=Train_v4[,-c(1)],  method="avNNet", preProcess="range", tuneLength=2, trControl=fitControl, maxit=2000)
PREDICTION_avNN = predict(model_avNN, newdata=Test_v4[,-c(1)])

confusionMatrix(PREDICTION_avNN,Test_v4[,"type"])

set.seed(666)
model_ada1 = train(type~., data=Train_v4[,-c(1)],  method="AdaBag", preProcess="range", trControl=fitControl)
model_ada2 = train(type~., data=Train_v4[,-c(1)],  method="AdaBag", preProcess="range", trControl=rControl)
PREDICTION_ada = predict(model_ada1, newdata=Test_v4[,-c(1,7)])

confusionMatrix(PREDICTION_ada1,Test_v4[,"type"])


