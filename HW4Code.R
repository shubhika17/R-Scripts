library(ISLR)
data(Default)
head(Default)
library(caret)
set.seed(2016)
#install.packages("pROC", lib="/my/own/R-packages/")
levels(Default$default) <- make.names(levels(factor(Default$default)))
Default$default <- relevel(Default$default, "Yes")
inTrain <- createDataPartition(Default$default, p=0.7, list = F)
train <- Default[inTrain,]
test <- Default[-inTrain,]
ctrl <- trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary)
logit <- train(default ~ ., data = Default, method = "glm", family = "binomial", trControl = ctrl, metric = "ROC")
summary(logit)
logitProbs <- predict(logit, newdata=test, type="prob")[,1]
logitClasses <- predict(logit, newdata=test)
1-confusionMatrix(data=logitClasses, test$default)$overall["Accuracy"][[1]]

