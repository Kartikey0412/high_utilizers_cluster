#############################################################################################################
#Feature importance
#############################################################################################################
#univariable statistics, univariable logistic, information_gain from CART, gine index from Random forest, lasso, Mutual Information 

library(dplyr)
library(StatMatch)
library(randomForest)
library(rpart)
library(caret)
library(caTools)
library(klaR)
library(dplyr)
library(glmnet)
require(doMC)
library(infotheo)

#importance of each continous varialbe against high utilizer indicator
wilh2ulacon <- lapply(h2ula[,c(198:229,243:259)],function(x) wilcox.test(x~h2ula[,260])$p.value)

#p values for each categorical variable againt high utilizer indicator
chih2ula <- lapply(h2ula[,c(1:197,230:242)],function(x) chisq.test(x,h2ula[,260])$p.value)


#calculating variable importance using gini index

h2ularf <- randomForest(h2ula$hu.y~., data=h2ula)

#Variable importance using information gain using CART trees

h2ular <- rpart(h2ula$hu.y~., data=h2ula)
h2ularvi <-  h2ular$variable.importance
h2ularvid <- as.data.frame(h2ularvi)
h2ularvid <- add_rownames(h2ularvid, "features")


#obtaining p values from univariable logistic regression
h2ulareg <- lapply(h2ula[,1:259], function(x) coef(summary(glm(h2ula[,260]~x,family=binomial, data = h2ula)))[,4])
h2ulareg <- as.data.frame(h2ulareg)


h2ula <- h2ula[,-c(1,244)]
ytrain <- h2ula$hu.y
xtrain <- h2ula
xtrain$hu.y <- NULL
xtrainmod <- model.matrix(~.,xtrain)
xtrainmod<- xtrainmod[,-1]

#lasso with 5 fold cross validation

cv1 <- cv.glmnet(xtrainmod,ytrain,family="binomial", nfold=5, type.measure="deviance", alpha=1)
md1 <- glmnet(xtrainmod, ytrain, family="binomial", lambda=cv1$lambda.1se, alpha=1)

#mutual information
#infotheo
h2ulad <- discretize(h2ula)
i258 <- mutinformation(h2ulad[,258],h2ulad[,260])
i <- lapply(h2ulad[,1:259], function(x) mutinformation(x,h2ulad[,260]))
isort <- sort(unlist(i), decreasing=TRUE)
isortd <- as.data.frame(isort)
