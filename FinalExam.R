#Step 1
library(ISLR)
library(caret)
data("Hitters")
head(Hitters)
str(Hitters)
nrow(Hitters)
ncol(Hitters)

#Step 2 
corMatrix<- cor(Hitters[,c(1:12,16:18)])
rho <- 0.85
for (i in 1:dim(corMatrix)[1]){
    for (j in 1:dim(corMatrix)[2]) {
        if  (abs(corMatrix[i,j]) < rho | i==j) {
            corMatrix[i,j] <- NA
        }  else  {
            corMatrix[i,j] <-  corMatrix[i,j] 
        }
    }
}
corMatrix <-  corMatrix[, colSums(is.na(corMatrix))<dim(corMatrix)[1]]
corMatrix <-  corMatrix[rowSums(is.na(corMatrix))<dim(corMatrix)[2],]
corMatrix

#Step 3
Hitters <- Hitters[,c("Salary", "Hits","HmRun", "Runs", "RBI", "Walks", "Years", "PutOuts", "Assists", "Errors")]

#Step 4
cor(Hitters[,1:10])
pairs(Hitters)

#Step 5
sum(complete.cases(Hitters)) /nrow(Hitters)

#Step 6
#install.packages("mice")
library(mice)
ImputedValues <- mice(data=Hitters, method="cart" , seed=2016)
HittersImp <- complete(ImputedValues, 1)

#Step 7
library(ggplot2)
salary <-data.frame(salaries=c(Hitters$Salary, HittersImp$Salary),  set=rep(c( "Missing", "Imputed"), each=322))
salary <- na.omit(salary)
ggplot(data=salary, aes(x=salaries, fill=set)) + geom_density(alpha = 0.4)

#Step 8
rm(corMatrix, Hitters, salary, i, ImputedValues, j, rho)

#Step 9
set.seed(2016)
inTrain <- createDataPartition(HittersImp$Salary, p=.70, list=F)
train <- HittersImp[inTrain,]
test   <- HittersImp[-inTrain,]

#Step 10
set.seed(2016)
#install.packages("tree")
library(tree)
#treefit = tree(log(Salary) ~ .,  controls = tree.control(nobs = nrow(train), mincut = 0, minsize = 1, mindev = 0.01),data = train)
treefit = tree(log(Salary) ~ ., control = tree.control(nobs = nrow(train), mincut = 0, minsize = 1, mindev = 0.01), data = train)
summary(treefit)
plot(treefit) ;text(treefit,pretty=0)

#Step 11
cv.hitters = cv.tree(treefit)
plot(x=cv.hitters$size, y=cv.hitters$dev, type="b" ,  col="blue", xlab="Number of terminal nodes", ylab="cross-validated error")
prunedfit = prune.tree(treefit, best=7)
summary(prunedfit)
plot(prunedfit) ; text(prunedfit, pretty=0)

#Step 12
testYhatTree=predict(prunedfit,  newdata=test)
plot(testYhatTree,  test$Salary,  col="blue", main="Actual salaries vs Predicted salaries",xlab="Predicted salaries",  ylab="Actual salaries")
TestErrorTree <- postResample(pred=testYhatTree,  obs=test$Salary)
TestErrorTree[[1]]^2

#Step 13
library(caret)
ctrl <- trainControl(method="cv",  number=10,
                      classProbs = FALSE,
                      summaryFunction = defaultSummary)
caretTree <- train(log(Salary)  ~ . , data = train, method = "rpart2", trControl = ctrl, tunelength=12, metric= "RMSE")
caretTree
plot(caretTree)

#Step 14
caretTreeYhat <- predict(caretTree,  newdata=test)
TestErrorcaretTree  <- postResample(pred=caretTreeYhat,  obs=test$Salary)
TestErrorcaretTree[[1]]^2

#Step 15
set.seed(2016)
lmfit <-  train(log(Salary)  ~ . ,data = train, method = "lm", trControl = ctrl, preProcess = c("center","scale"), tunelength  = 16, metric= "RMSE")
#lmfit <- train(log(Salary) ~ ., data = train, method = "lm", trControl = ctrl, preProcess = c )
lmfit
summary(lmfit )

#Step 16
source("diagonostic_plots.R")
myDiag(lmfit)

#Step 17
#testYhatOLS  <- predict(1mfit,  newdata = test)
testYhatOLS <- predict(lmfit, newdata = test)
testErrorOLS <- postResample(pred=testYhatOLS,  obs=test$Salary)
testErrorOLS[[1]]^2

#Step 18
set.seed(2016)
rrfit <-  train(log(Salary)  ~ . ,data = train, method = "foba", trControl = ctrl,preProcess =c("center","scale"),tunelength  = 4,metric= "RMSE")
rrfit

#Step 19
#findBestLambda <- subset(rrfit$results, k==9)
findBestLambda <- subset(rrfit$results, k == 9)
plot(x=findBestLambda$lambda, y=findBestLambda$RMSE
     , col="blue" , pch=19, type ="b"
     , main="RMSE vs Lambada" , xlab="Lambda" , ylab="RMSE")
testYhatRidge <- predict(rrfit,  newdata=test)
TestErrorRidge <- postResample(pred=testYhatRidge,  obs=test$Salary)
TestErrorRidge[[1]]^2

#Step 20
set.seed(2016)
lassofit <-  train(log(Salary)  ~ . ,
                   data = train,
                   method = "lars2",
                   trControl = ctrl,
                   preProcess =c( "center","scale"),
                   tunelength  = 16,
                   metric= "RMSE")
lassofit$finalModel 
plot(lassofit$finalmodel)

#Step 21
coef(lassofit$finalModel)
coef(lassofit$finalModel) [lassofit$bestTune[[1]]+1,]
plot(x=lassofit$results$step, y=lassofit$results$RMSE
     , col="blue" , pch=19, type ="b"
     , main="RMSE vs Lambada (step)" , xlab="Lambda (step)" , y1ab="RMSE")
testYhatLasso <- predict(lassofit,  newdata=test)
TestErrorLasso <- postResample(pred=testYhatLasso,  obs=test$Salary)
TestErrorLasso[[1]]^2

#Step23
finalstats <- rbind(testErrorOLS,  TestErrorRidge,  TestErrorLasso,  TestErrorTree, TestErrorcaretTree)
finalstats 

#Step 24
par(mfrow=c(2,3))
plot(y=test$Salary,  x=exp(testYhatOLS) , pch=19, col="blue"
     ,   xlab="OLS" , ylab="Salary" , main="Actual Salary vs OLS")
abline(0,1,col="red")
plot(y=test$Salary,  x=exp(testYhatRidge) , pch=19, col="blue"
     ,   xlab="Ridge Regression" , ylab="Salary" , main="Actual Salary vs Ridge Regression")
abline(0,1,col="red")
plot(y=test$Salary,  x=exp(testYhatLasso) , pch=19, col="blue"
     ,   xlab="Lasso", ylab="Salary", main="Actual Salary vs Lasso")
abline(0,1,col="red")
plot(y=test$Salary,  x=exp(testYhatTree) , pch=19, col="blue"
     ,   xlab="Tree" , ylab="Salary" , main="Actual Salary vs Regression Tree")
abline(0,1,col="red")
plot(y=test$Salary,  x=exp(caretTreeYhat) , pch=19, col="blue"
     ,     xlab="Tree" , ylab="Salary" , main="Actual Salary vs Regression Tree (caret)")
abline(0,1,col="red")


                                   