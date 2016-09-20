##############################################################################
#                                                                            #
#                                                                            #
#                                                                            #
#                         HomeWork #1                                        #
#                                                                            #
#                                                                            #
#                                                                            #
#                                                                            #
##############################################################################


########## Read Data ##########
library(MASS)

bostonData <- Boston

# plot the data
pairs(bostonData)
library(PerformanceAnalytics)
chart.Correlation(bostonData)



# (d)
par(mfrow=c(1,3))
hist(bostonData$crim[bostonData$crim>1], breaks=25)
hist(bostonData$tax, breaks=25)
hist(bostonData$ptratio, breaks=25)

# (e)
dim(subset(Boston, chas == 1))

#(g)
t(subset(Boston, medv == min(Boston$medv)))
summary(Boston)

#(h)
rm8 <- subset(Boston, rm > 8)
summary(rm8)


#Problem 5

#predictors   <- bostonData[,2:14]
#response     <- as.data.frame(bostonData[,1])

library(MASS)

bostonData <- Boston
sampleSet <- sample(1:nrow(bostonData), nrow(bostonData)/2)
trainSetData        <- bostonData[sampleSet,]
testSetData         <- bostonData[-sampleSet,]
rownames(trainSetData)   <- NULL
rownames(testSetData)    <- NULL
trainPredictors   <- trainSetData[,2:14]
testPredictors    <- testSetData[,2:14]

lm.train = lm(crim~., data=trainSetData)
summary(lm.train)

layout(matrix(c(1,2,3,4),2,2))
plot(lm.train)

trainMeanSQErr   = sqrt(mean(lm.train$residuals^2))
trainSE    = sqrt( sum( residuals(lm.train)^2 ) / lm.train$df.residual )
trainMAPE  = (sum(trainSetData$crim)-sum(as.numeric(predict(lm.train,trainPredictors[]))))/sum(trainSetData$crim)

testMAPE  = (sum(testSetData$crim)-sum(as.numeric(predict(lm.train,testPredictors[]))))/sum(testSetData$crim)


#Problem 6

testSetLogistic = testSetData
testSetLogistic$Y <- as.numeric(testSetData$crim > median(bostonData$crim))
trainSetLogistic = trainSetData
trainSetLogistic$Y <- as.numeric(trainSetData$crim > median(bostonData$crim))

glm.train = glm(Y ~ . - Y - crim, data = trainSetLogistic, family = binomial)

trainLogisticErr  = (sum(trainSetLogistic$Y != as.numeric(predict(glm.train, trainSetLogistic, type = "response") > 0.5)))/length(trainSetLogistic$Y)

testLogisticErr  = (sum(testSetLogistic$Y != as.numeric(predict(glm.train, testSetLogistic, type = "response") > 0.5)))/length(testSetLogistic$Y)

summary(glm.train)