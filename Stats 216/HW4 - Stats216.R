##############################################################################
#                         HomeWork #4                                        #
##############################################################################


##########################
#
#  Problem 2
#
##########################
#2a
# Create data for 3 classes - combine using rbind
d1 = matrix( rnorm(20*50), nrow=20, ncol=50 )
d2 = matrix( rnorm(20*50), nrow=20, ncol=50 )
d3 = matrix( rnorm(20*50), nrow=20, ncol=50 )

#means
for( i in 1:20 ){
  d1[i,] = d1[i,] + rep(1,50) 
}

for( i in 1:20 ){
  d2[i,] = d2[i,] + rep(-1,50) 
}

for( i in 1:20 ){
  d3[i,] = d3[i,] + c(rep(1,25), rep(-1,25) ) 
}

x = rbind(d1,d2,d3)
classes = c( rep(1,20), rep(2,20), rep(3,20) ) 

#2b
pca.out = prcomp( x, scale=TRUE ) 
summary(pca.out)
pca.out$x[,1:2]
plot( pca.out$x[,1:2],col=classes,pch=19 )

#2c
km.out = kmeans(x, 3, nstart=20)
table(km.out$cluster, classes)

#2d
km.out = kmeans(x, 2, nstart=20)
km.out$cluster
table(km.out$cluster, classes)

#2e
km.out = kmeans(x, 4, nstart=20)
km.out$cluster
table(km.out$cluster, classes)

#2f
km.out = kmeans(pca.out$x[,1:2], 3, nstart=20)
table(km.out$cluster, classes)

#2g
km.out = kmeans(scale(x), 3, nstart=20)
km.out$cluster

##########################
#
#  Problem 3
#
##########################
#3a
library(randomForest)
setwd('C:/Users/Arun/Desktop/Stats216')
load('body.RData')

gender <- as.factor(Y$Gender)

set.seed(33721)

# test and train set for X
testSet  <- sort(sample(1:nrow(X), 200))
trainSet <- (1:nrow(X))[-testSet]
data <- as.data.frame(cbind('Weight' = Y$Weight, X))

testErrBagging <- numeric(100)
testErrRandForest <- numeric(100)
for (i in 1:100) {
  baggingIter   <- randomForest(Y$Weight ~ ., data = X, subset = trainSet, mtry = 21, importance = TRUE, ntree = i*5)
  rForestIter   <- randomForest(Y$Weight ~ ., data = X, subset = trainSet, mtry = 7,  importance = TRUE, ntree = i*5) #Default tree = 500; grow from there
  yhatBagging     <- predict(baggingIter, newdata = X[testSet, ])
  yhatRandFrst    <- predict(rForestIter, newdata = X[testSet, ])
  testErrBagging[i]    <- mean((yhatBagging  - Y[testSet, ]$Weight)^2)
  testErrRandForest[i] <- mean((yhatRandFrst - Y[testSet, ]$Weight)^2)
}
numTree <- c(1:100) * 5
ylimMin = min(min(testErrRandForest),min(testErrBagging))-0.25
ylimMax = max(max(testErrRandForest),max(testErrBagging))+0.25
yLimits = c(ylimMin,ylimMax)
plot(numTree, testErrBagging, xlab = "Number of Trees", ylab = "Test Error", col = "blue", pch = 19,ylim=yLimits,type='l')
points(numTree, testErrRandForest, col = "red",pch = 19, type = "l")
legend("topright", legend = paste(c("Bagging", "Random Forest")), col = c("blue", "red"), lty = 1)

#3b
randForestTrain <- randomForest(Y$Weight ~ ., data = X, subset = trainSet, mtry = 7, importance = TRUE,ntree=500)
importance(randForestTrain)
varImpPlot(randForestTrain)

baggingTrain = randomForest(Y$Weight ~ ., data = X, subset = trainSet, mtry = 21,importance = TRUE,ntree=500)
importance(baggingTrain)
varImpPlot(baggingTrain)

#3c
set.seed(51890)
randForestTest   <- randomForest(Y$Weight ~ ., data = X, subset = trainSet, mtry = 7,importance = TRUE, ntree = 500)
yhatRandFrstTest <- predict(randForestTest, newdata = X[testSet, ])
testErrRandForest <- mean((yhatRandFrstTest - Y$Weight[testSet])^2)
testErrRandForest

#3d
for (i in 1:120) {
  baggingIter   <- randomForest(Y$Weight ~ ., data = X, subset = trainSet, mtry = 21, importance = TRUE, ntree = i*5)
  rForestIter   <- randomForest(Y$Weight ~ ., data = X, subset = trainSet, mtry = 7,  importance = TRUE, ntree = i*5) #Default tree = 500; grow from there
  yhatBagging     <- predict(baggingIter, newdata = X[testSet, ])
  yhatRandFrst    <- predict(rForestIter, newdata = X[testSet, ])
  testErrBagging[i]    <- mean((yhatBagging  - Y[testSet, ]$Weight)^2)
  testErrRandForest[i] <- mean((yhatRandFrst - Y[testSet, ]$Weight)^2)
}
numTree <- c(1:120) * 5
ylimMin = min(min(testErrRandForest),min(testErrBagging))-0.25
ylimMax = max(max(testErrRandForest),max(testErrBagging))+0.25
yLimits = c(ylimMin,ylimMax)
plot(numTree, testErrBagging, xlab = "Number of Trees", ylab = "Test Error", col = "blue", pch = 19,ylim=yLimits,type='l')
points(numTree, testErrRandForest, col = "red",pch = 19, type = "l")
legend("topright", legend = paste(c("Bagging", "Random Forest")), col = c("blue", "red"), lty = 1)




##########################
#
#  Problem 4
#
##########################
#4a
x1 = c(3, 2, 4, 1, 2, 4, 4)
x2 = c(4, 2, 4, 4, 1, 3, 1)
colors = c("red", "red", "red", "red", "blue", "blue", "blue")
plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0, 5))

#4b
plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0, 5))
abline(-0.5, 1)

#4d
plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0, 5))
abline(-0.5, 1)
abline(-1, 1, lty = 2)
abline(0, 1, lty = 2)

#4g
plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0, 5))
abline(-0.7, 1)


#4h
plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0, 5))
points(c(4), c(2), col = c("red"))



