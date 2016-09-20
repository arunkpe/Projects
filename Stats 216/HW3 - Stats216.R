##############################################################################
#                         HomeWork #3                                        #
##############################################################################

##########################
#
#  Problem 3
#
##########################
library(MASS)
attach(Boston)

#3a
lm.fit = lm(nox~poly(dis, 3), data=Boston)
summary(lm.fit)
disRange = range(dis)
disList = seq(from=disRange[1], to=disRange[2], by=0.1)
lm.pred = predict(lm.fit, list(dis=disList))
plot(nox~dis, data=Boston)
lines(disList, lm.pred, col="red")

#3b
polyRSS = rep(0, 10)
for (i in 1:10) {
  lm.model = lm(nox ~ poly(dis, i), data = Boston)
  polyRSS[i] = sum(lm.model$residuals^2)
}
print(polyRSS)
plot(polyRSS)
lines(polyRSS,col="blue")

#3c
#Using hints from here - http://stackoverflow.com/questions/25391623/how-to-predict-with-cv-glm-in-r
library(boot)
cvErr = rep(0, 10)
for (i in 1:10) 
  {
    glm.model = glm(nox ~ poly(dis, i), data = Boston)
    cvErr[i] = cv.glm(Boston, glm.model, K = 10)$delta[2]
  }
plot(1:10, cvErr*100, xlab = "Polynomial Fit Degree", ylab = "Cross Validation Error %", type = "l")
CVErr

#3d
library(splines)
spline.model = lm(nox ~ bs(dis, df = 4,knots=c(3,6,9)), data = Boston)
summary(spline.model)

pred.spline = predict(spline.model, list(dis = disList))
plot(nox ~ dis, data = Boston)
lines(disList, pred.spline, col = "red")

#3e
splineRSS = rep(0, 30)
for (i in 0:30) 
  {
    lm.model = lm(nox ~ bs(dis, df = i), data = Boston)
    splineRSS[i] = sum(lm.model$residuals^2)
  }
print(splineRSS)
plot(splineRSS,xlab ='Degrees of Freedom Splines',ylab='RSS')
lines(splineRSS, col = "red")



#3f
library(boot)
cvErrSpline = rep(0, 30)
for (i in 1:30) 
{
  glm.model = glm(nox ~ bs(dis, df = i), data = Boston)
  cvErrSpline[i] = cv.glm(Boston, glm.model, K = 10)$delta[2]
}
plot(1:30, cvErrSpline*100, xlab = "Spline Degrees of Freedom", ylab = "Cross Validation Error %", type = "l")






##########################
#
#  Problem 4
#
##########################

########## Read Data ##########
setwd('C:/Users/Arun/Desktop/Stats216')

#4a
load('body.RData')
gender <- as.factor(Y$Gender)
par(mfrow = c(2, 1))
plot(Y$Age, Y$Weight, col = c("red", "blue")[gender],xlab = "Age", ylab="Weight")
plot(Y$Age, Y$Height, col = c("red", "blue")[gender],xlab = "Age", ylab="Height")

#4b
library(pls)

set.seed(33721)

# test and train set for X
testSet  <- sort(sample(1:nrow(X), 200))
trainSet <- (1:nrow(X))[-testSet]
data <- as.data.frame(cbind('Weight' = Y$Weight, X))

# PCR for fitting the model
train.pcr <- pcr(Weight ~ ., data = data, subset = trainSet, scale = TRUE, validation = "CV")

# PLSR for fitting the model
train.plsr <- plsr(Weight ~ ., data = data, subset = trainSet, scale = TRUE,validation = "CV")


#4c
# summary for PCR
summary(train.pcr)
# summary for PLSR
summary(train.plsr)
par(mfrow=c(1,2))
validationplot(train.pcr,  val.type="MSEP", main="Mean Sq Err for PCR")
validationplot(train.plsr, val.type="MSEP", main="Mean Sq Err for PLSR")

plot(100*train.pcr$Xvar/train.pcr$Xtotvar, xlab = "Principal Component", ylab = "Prop. Variance Explained",col="red",pch =1)
points(100*train.plsr$Xvar/train.plsr$Xtotvar, xlab = "Principal Component", ylab = "Prop. Variance Explained", col = "blue",pch=2)
legend("topright", legend = c("PCR", "PLSR"), col = c("red", "blue"), pch = c(1,2))

#4d
pred.pcr1 = predict(train.pcr,data = data, subset = trainSet, ncomp=1)
pred.pcr2 = predict(train.pcr,data = data, subset = trainSet, ncomp=2)
pred.pcr3 = predict(train.pcr,data = data, subset = trainSet, ncomp=3)
pred.pcr4 = predict(train.pcr,data = data, subset = trainSet, ncomp=4)
pred.pcr5 = predict(train.pcr,data = data, subset = trainSet, ncomp=5)
pred.pcr6 = predict(train.pcr,data = data, subset = trainSet, ncomp=6)
pred.pcr18 = predict(train.pcr, data = data, subset = trainSet, ncomp=18)
par(mfrow=c(2,2))
# Error for various predictions
plot(pred.pcr5, pch=21, col="red", ylab="PCR5 (red), PCR18(green) and Actual(black) Weights", xlab = 'observation',main="PCR")
points(pred.pcr18, pch=23, col="green")
points(data[c(trainSet),'Weight'], pch=22, col="black")

y.test = as.data.frame(data[c(trainSet),'Weight'])
# Test vs PCR5
plot(data.matrix(y.test), pred.pcr5, pch=21, col="red", xlab="Actual", ylab="PCR5", main="Actual vs PCR5")
# Test vs PCR18
plot(data.matrix(y.test), pred.pcr18, pch=23, col="green", xlab="Actual", ylab="PCR18", main="Actual vs PCR18")
# PCR4 vs PCR 18
plot(pred.pcr5, pred.pcr18, pch=23, col="black", xlab="PCR5", ylab="PCR15", main="PCR5 vs PCR18")

#Mean Squared Errors
y.test = as.data.frame(data[c(trainSet),'Weight'])
mean((as.data.frame(pred.pcr1)-y.test)^2)
mean((as.data.frame(pred.pcr2)-y.test)^2)
mean((as.data.frame(pred.pcr3)-y.test)^2)
mean((as.data.frame(pred.pcr4)-y.test)^2)
mean((as.data.frame(pred.pcr5)-y.test)^2)
mean((as.data.frame(pred.pcr18)-y.test)^2)



#PLSR
pred.plsr1 = predict(train.plsr,data = data, subset = trainSet, ncomp=1)
pred.plsr2 = predict(train.plsr,data = data, subset = trainSet, ncomp=2)
pred.plsr3 = predict(train.plsr,data = data, subset = trainSet, ncomp=3)
pred.plsr4 = predict(train.plsr,data = data, subset = trainSet, ncomp=4)
pred.plsr5 = predict(train.plsr,data = data, subset = trainSet, ncomp=5)
pred.plsr6 = predict(train.plsr, data = data, subset = trainSet, ncomp=6)
pred.plsr7 = predict(train.plsr, data = data, subset = trainSet, ncomp=7)
pred.plsr15 = predict(train.plsr, data = data, subset = trainSet, ncomp=15)

par(mfrow=c(2,2))
# plot 1
plot(pred.plsr5, pch=21, col="red", ylab="PLSR5 (red), PLSR7(green) and Actual(black) Weights", main="PLSR")
points(pred.plsr7, pch=23, col="green")
points(data[c(trainSet),'Weight'], pch=22, col="black")

# Test vs PLSR5
plot(data.matrix(y.test), pred.plsr5, pch=21, col="red", xlab="Test", ylab="PLSR5", main="Actual vs PLSR5")
# Test vs PLSR6
plot(data.matrix(y.test), pred.plsr7, pch=23, col="green", xlab="Test", ylab="PLSR7", main="Actual vs PLSR7")
# PLSR5 vs PLSR6
plot(pred.plsr5, pred.plsr7, pch=23, col="black", xlab="PLSR5", ylab="PLSR7", main="PLSR5 vs PLSR7")

#Mean Squared Errors
mean((as.data.frame(pred.plsr1)-y.test)^2)
mean((as.data.frame(pred.plsr2)-y.test)^2)
mean((as.data.frame(pred.plsr3)-y.test)^2)
mean((as.data.frame(pred.plsr4)-y.test)^2)
mean((as.data.frame(pred.plsr5)-y.test)^2)
mean((as.data.frame(pred.plsr6)-y.test)^2)
mean((as.data.frame(pred.plsr7)-y.test)^2)
mean((as.data.frame(pred.plsr15)-y.test)^2)


#4e)
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
data.x <- scale(model.matrix(Weight ~ ., data = data)[, -1])

data.y <- data$Weight
train.lasso <- glmnet(data.x[trainSet, ], data.y[trainSet], alpha = 1, lambda = grid)

plot(train.lasso)

cv.lasso = cv.glmnet(data.x[trainSet, ], data.y[trainSet],alpha=1)
plot(cv.lasso)

lasso.cv <- cv.glmnet(data.x[trainSet, ], data.y[trainSet], alpha = 1)
bestlam <- lasso.cv$lambda.min
predict(train.lasso, type = "coefficients", s = bestlam)

#4f
y.test = as.data.frame(data[c(testSet),'Weight'])

#PCR
pred.pcr = predict(train.pcr,data[testSet,],ncomp=5)
mean((as.data.frame(pred.pcr)-y.test)^2)


#PLSR
pred.plsr = predict(train.plsr,data[testSet,], ncomp=5)
mean((as.data.frame(pred.plsr)-y.test)^2)

#Lasso
x <- scale(model.matrix(Weight ~ ., data = data[testSet,])[, -1])
pred.lasso = predict(train.lasso, s=bestlam, newx =x)
mean((pred.lasso-y.test)^2)

