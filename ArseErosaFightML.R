################################################################################

ArseErosa <- read.csv('ArseErosaFightAudit.csv', 
                       sep=',', header=TRUE, 
                       na.strings=c('','NA'))#224X18

library(caret)
library(randomForest)
library(MASS)
library(gbm)

# There is a problem with dplyr after those installs above, fix with this:
# install.packages('devtools')
# devtools::install_github("tidyverse/dplyr")
# install.packages('pillar')
library(dplyr)


set.seed(189678345) # this will reproduce the same numbers each time sampling is done
# run set.seed value to get exact results, otherwise results differ

# creates a partition of the data by indices


inTrain <- createDataPartition(y=ArseErosa$Hits.Lnd.A, p=0.7, list=FALSE)

trainingSet <- ArseErosa[inTrain,]#157X18
testingSet <- ArseErosa[-inTrain,]#67X18

# randomForest, cross-validation (cv) = 5
rfMod <- train(Hits.Lnd.A~., method='rf', data=(trainingSet), 
               trControl=trainControl(method='cv'), number=5)
plot(rfMod)

# generalizedBoostedModel
gbmMod <- train(Hits.Lnd.A~., method='gbm', data=trainingSet, verbose=FALSE )

plot(gbmMod)



# run predictions on the testing set
predRF <- round(predict(rfMod, testingSet))
predGbm <- round(predict(gbmMod, testingSet))

predDF <- data.frame(predRF, predGbm, type=testingSet$Hits.Lnd.A)
predDF

CombinedModels <- train(type~., method='gam', data=predDF)
CombinedPredictions <- round(predict(CombinedModels, predDF))
CombinedPredictions

sum <- sum(CombinedPredictions==testingSet$Hits.Lnd.A)
length <- length(testingSet$Hits.Lnd.A)
accuracy_CP1 <- sum/length #97.01

sum <- sum(predRF==testingSet$Hits.Lnd.A)
length <- length(testingSet$Hits.Lnd.A)
accuracy_rfMod <- (sum/length) #97.01

sum <- sum(predGbm==testingSet$Hits.Lnd.A)
accuracy_Gbm <- (sum/length) #97.01

#try 'rpart', 'knn', 'glm', 
knnMod <- train(Hits.Lnd.A  ~ .,
                method='knn', preProcess=c('center','scale'),
                tuneLength=10, trControl=trainControl(method='cv'), data=trainingSet)#list 23
plot(knnMod)

rpartMod <- train(Hits.Lnd.A ~ ., method='rpart', tuneLength=9, data=trainingSet) #lists 23
plot(rpartMod)

glmMod <- train(Hits.Lnd.A ~ ., 
                method='glm', data=trainingSet)

predKNN <- round(predict(knnMod, testingSet))
predRPART <- round(predict(rpartMod, testingSet))
predGLM <- round(predict(glmMod, testingSet))

df3 <- cbind(predKNN, predRPART, predGLM,testingSet$Hits.Lnd.A)
colnames(df3)[4] <- 'TrueValue'

length=length(testingSet$Hits.Lnd.A)

sumKNN <- sum(predKNN==testingSet$Hits.Lnd.A)
sumRPart <- sum(predRPART==testingSet$Hits.Lnd.A)
sumGLM <- sum(predGLM==testingSet$Hits.Lnd.A)

accuracy_KNN <- sumKNN/length 
accuracy_RPART <- sumRPart/length 
accuracy_GLM <- sumGLM/length 

predDF3 <- data.frame(predRF,predGbm,df3)

CombinedModels <- train(TrueValue ~ ., method='gam', data=predDF3)
CombinedPredictions2 <- round(predict(CombinedModels, predDF3))
accuracy_CP2 <- sum(CombinedPredictions2==testingSet$Hits.Lnd.A)/length 

predDF4 <- data.frame(predDF3, CombinedPredictions2)
colnames(predDF4)
# [1] "predRF"               "predGbm"              "predKNN"              "predRPART"           
# [5] "predGLM"              "TrueValue"            "CombinedPredictions2"

predDF4 <- predDF4[,c(1:5,7,6)]
colnames(predDF4)
# [1] "predRF"               "predGbm"              "predKNN"              "predRPART"           
# [5] "predGLM"              "CombinedPredictions2" "TrueValue" 

results <- c(round(accuracy_rfMod,2),  
              round(accuracy_Gbm,2), 
             round(accuracy_KNN,2), round(accuracy_RPART,2),
             round(accuracy_GLM,2), 
             round(accuracy_CP2,2), round(100,2))

results <- as.factor(results)
results <- t(data.frame(results))#1X7
colnames(results) <- colnames(predDF4)
Results <- rbind(predDF4, results) #68X7

write.csv(Results,'TrueHitsLanded_ML_predictionResults.csv', row.names=FALSE)



