rm(list=ls())
set.seed(666)
library(doMC)
registerDoMC(cores = 8)
library(caret); library(kernlab); library(ISLR); library(RANN);
library(DMwR);library(MASS); library(randomForest); library(e1071)


train=read.csv(file="./train.csv", header = TRUE)
test=read.csv(file="./test.csv", header = TRUE)

str(train)
summary(train)

train_v1 = train

#Removing near zero variation. V2

nearzero <- nearZeroVar(train_v1, saveMetrics = TRUE)
#nearzero
train_v2 <- train_v1[, !nearzero$nzv]

features_corr <- findCorrelation(cor(train_v2[,-c(6,7)]), cutoff=0.2)
features_corr
names(train_v2[, -c(6,7)])[features_corr]

table(train_v2$type)


inTrain = createDataPartition(y=train_v2$type, p=0.8, list = FALSE)

Train_v3 = train_v2[inTrain,]
Test_v3 = train_v2[-inTrain,]


control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(Train_v3[,-c(1,4,7)], Train_v3[,7], sizes=c(1:4), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)

BACKUP = Train_v3 

RFcontrol <- trainControl(method="cv", number=10, repeats=5, classProbs = TRUE)

fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10, allowParallel=TRUE,
                           ## repeated ten times
                           repeats = 10)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

#gbmFit1 <- train(type ~ ., data = Train_v3[,-c(1,6)], 
#                 method = "gbm", 
#                 trControl = fitControl, tuneGrid = gbmGrid,
##                 ## This last option is actually one
#                 ## for gbm() that passes through
#                 verbose = FALSE, metric="Accuracy")

#gbmFit1

# all.equal(predict(gbmFit1, type='prob'), predict(gbmFit1_1, type='prob'))

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           )

set.seed(666)
gbmFit3 <- train(type ~ ., data =train[,-c(1,6)] , 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid,
                 ## Specify which metric to optimize
                 metric = "Accuracy")
gbmFit3

#PREDICTION_FULL = predict(gbmFit3, newdata=Test_v3[,-c(1,6)])
#confusionMatrix(PREDICTION_FULL,Test_v3[,"type"])
#PREDICTION_FULL

PREDICTION_SUBMIT = predict(gbmFit3, newdata=test[,-c(1,6)])
test$type=PREDICTION_SUBMIT
str(test)

answer = data.frame(id=test$id,type=test$type)
str(answer)
write.table(answer,file="answer_day2.csv",sep=",", quote = FALSE, row.names = FALSE)
