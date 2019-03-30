#Code built for clustering High Utilizers using tSNE embedding 
#loading required libraries
library(lubridate)
library(dplyr)
library(StatMatch)
library(cluster)
library(daisy)
library(Rtsne)
library(fpc)
library(purrr)
library(tidyr)
library(ggplot2)
library(reshape2)

#file for patient conditions and cost data
h<- read.csv("High_utilizer_flags_wcosts_bene-2.csv")
#file containing visits information
i<- read.csv("IP_ED_visits.csv")

#looking at preventable costs
ied <- aggregate(OP_totalcost_vis~BENE_ID,subset(i,preventable==1), sum)

#merging conditions and costs
h1 <- merge(h,ied,all=T)

#converting null's to zeros
h1[is.na(h1)] <- 0

colnames(h1)[312] <- "totaledbill"

#label HU
h1$hu <- ifelse(h1$costgroup>=90,1,0)
#h12 <- h1[1:2,]

#Remove some columns like ageF, age, year, bene_death_dt, bene_birth_dt
h2 <- h1[,-c(4,223, 227,228, 231)]



#Remove costs
h2 <- h2[,-c(122:126,131,134,227,260:293,307)]
write.csv(h2,"h2updated.csv")

#break file

h2u <- read.csv("h2updated.csv")
h2u <- h2u[,-1]

#continous variables and converting to Numeric type
h2ucon <- h2u[,c(6,8,9,10,11,110,113,119,120,121,123,125,126,215,216,217,219,265)]
h2ucon <- apply(h2ucon,2,as.numeric)
h2ucon <- as.data.frame(h2ucon)

#Categorical
h2ucat <- h2u[,c(2,4,5,7,12:109,111,112,114:118,122,124,127:214,218,220:265)]
#removing uniquers
h2uca <- h2ucat[,-c(35,140,141)]
h2uca <- apply(h2uca,2,factor)
h2uca <- as.data.frame(h2uca)




h2ula <- merge(h2uca, h2ucon, by=0)

h2ula <- read.csv("Mergedh2u.csv")
h2ula2 <- h2ula[,-c(1,2,239)]

#kruh2ucon <- apply(h2ucon[,-c(1,16,18)],2,function(x) kruskal.test(x,h2ucon[,18])$p.value)
#kruh2con<- lapply(1:ncol(h2con)-1, function(x) kruskal.test(h2con[,x]~h2con$hu))
#write.csv(h2con,"h2con.csv")
#h2con <- read.csv("h2con.csv")
wilh2ulacon <- lapply(h2ula[,c(198:229,243:259)],function(x) wilcox.test(x~h2ula[,260])$p.value)
#kruh2ulacon <- as.data.frame(kruh2ulacon)
#write.csv(kruh2ulacon, "kruh2ulacon.csv")

chih2ula <- lapply(h2ula[,c(1:197,230:242)],function(x) chisq.test(x,h2ula[,260])$p.value)

#write.csv(h2cat, "h2cat.csv")
#h2cat <- read.csv("h2cat.csv")


# wilh2ulaconsort <- sort(wilh2ulacon)
# h2ulareg2 <- h2ulareg[-1]
# h2ulareg$categc[-1]
# h2ulareg2 <- lapply(h2ulareg, function(x) x[-1])
# h2ulareg2sort <- sort(unlist(h2ulareg2))
# h2ulareg2sortd <- as.data.frame(h2ulareg2sort)
# unih2ula <- append(chih2ula,wilh2ulacon)


#calculating variable importance using gini index
library(randomForest)
h2ularf <- randomForest(h2ula$hu.y~., data=h2ula)

#Variable importance using information gain using CART trees
library(rpart)
h2ular <- rpart(h2ula$hu.y~., data=h2ula)
h2ularvi <-  h2ular$variable.importance
h2ularvid <- as.data.frame(h2ularvi)
h2ularvid <- add_rownames(h2ularvid, "features")

# load the libraries
library(caret)
library(caTools)
library(klaR)

library(caret)
library(klaR)

 
library(dplyr)
#unih2ulasortd <- add_rownames(unih2ulasortd, "features")
#Logistic Regression
#h2ulr <- merge(h2uca, h2ucon, by=0)
#h2ulr <- h2u[,-1]
#h2ulr <- h2ulr[,-c(1,2,100,244)]
#h2ulr <- h2ulr[,-c(2,195:227)]
#h2uca8 <- h2uca[,c(1:8,243)]

#h2uregcat <- apply(h2uca[,-243],2, function(x) coef(summary(glm(h2uca[,243]~x,family=binomial, data = h2uca)))[,4])

#univariable logistic regression
h2ulareg <- lapply(h2ula[,1:259], function(x) coef(summary(glm(h2ula[,260]~x,family=binomial, data = h2ula)))[,4])
h2ulareg <- as.data.frame(h2ulareg)
#write.csv(h2ulaconreg, "h2ulaconreg.csv")
# library(biglm)
# n <- names(h2ulr)
# f <- as.formula(paste("hu.y ~", paste(n[!n %in% "hu.y"], collapse = " + ")))
# h2ulrreg <- bigglm(f,data=h2ulr, family=binomial(), chunksize=30000, maxit=10)

#cross validated- lasso
#h2ula <- merge(h2uca, h2ucon, by=0)
#write.csv(h2ula,"Mergedh2u.csv")
h2ula <- h2ula[,-c(1,244)]
ytrain <- h2ula$hu.y
xtrain <- h2ula
xtrain$hu.y <- NULL
xtrainmod <- model.matrix(~.,xtrain)
xtrainmod<- xtrainmod[,-1]

# pkgs <- list("glmnet","doParallel","foreach","pROC")
# lapply(pkgs, require, character.only=T)
# registerDoParallel(cores=4)


library(glmnet)
require(doMC)
cv1 <- cv.glmnet(xtrainmod,ytrain,family="binomial", nfold=5, type.measure="deviance", alpha=1)
md1 <- glmnet(xtrainmod, ytrain, family="binomial", lambda=cv1$lambda.1se, alpha=1)

#mutual information
#continous
# h2ula <- read.csv("Mergedh2u.csv")
# h2ula <- h2ula[,-1]
# h2ulac <- h2ula[,c(243:259)]
# h2ulac <- apply(h2ulac,2,as.numeric)
# h2ulac <- as.data.frame(h2ulac)
# h2ulacm <- as.matrix(h2ulac)
# 
# 
# h2ula$hu.y <- as.factor(h2ula$hu.y)
# h2ulacd <- as.data.frame(h2ula$hu.y)
# library(mpmi)
# m1 <- mmi(h2ulacm[,1], h2ulacd)

#mrmre
# library(mRMRe)
# mrctarget <- as.numeric(h2ula[,260])
# h2ula <- read.csv("Mergedh2u.csv")
# h2ula <- h2ula[,-1]
# h2ula[,1:197] <- lapply(h2ula[,1:197], as.factor)
# h2ula[,230:242] <- lapply(h2ula[,230:242], as.factor)
# h2ula[,c(198:229,243:259)] <- lapply(h2ula[,c(198:229,243:259)], as.numeric)
# h2ula[,260] <- as.factor(h2ula[,260])
# h2ula[,1:260]<- lapply(h2ula[,1:260], as.numeric)
# h2ulamr <- mRMR.data(h2ula)
# h2ulamre <- mRMR.ensemble(data= h2ulamr, target_indices=260,feature_count = 100, solution_count = 1)

#infotheo
library(infotheo)
h2ulad <- discretize(h2ula)

i258 <- mutinformation(h2ulad[,258],h2ulad[,260])
i <- lapply(h2ulad[,1:259], function(x) mutinformation(x,h2ulad[,260]))
isort <- sort(unlist(i), decreasing=TRUE)
isortd <- as.data.frame(isort)

#h2ula[,198:229]<- lapply(h2ula[,198:229],as.numeric)
#h2ula[,260] <- as.factor(h2ula[,260])
#Predictions
# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- sample.split(h2ula$hu.y, SplitRatio = 0.80)
train = subset(h2ula, trainIndex == TRUE)
test  = subset(h2ula, trainIndex == FALSE)

#only on clinician selected features
train2 <- train[,c(1,2,3,4,6,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,83,84,85,86,87,88,89,90,91,93,97,100,106,241,258)]

# train a CART
h2ularm <- rpart(train$hu.y~., data=train)
h2ularm2 <- rpart(train2$hu.y~., data=train2)
# # make predictions
# x_test <- data_test[,1:4]
# y_test <- data_test[,5]
# predictions <- predict(model, x_test)
# # summarize results
# confusionMatrix(predictions$class, y_test)



#train2 <- train[,c(1,2,3,4,6,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,83,84,85,86,87,88,89,90,91,93,97,100,106,241,258)]

predictrm <- predict(h2ularm, test[,-258], type= "class") #accuracy= 95.069%


predictrm2 <- predict(h2ularm2, test[,-258], type= "class") #accuracy= 91.539
#rm2prediction <- rep("0",nrow(test))
#rm2prediction[predictrm2 >0.5] = "1"
#rm2prediction <- as.numeric(rm2prediction)

#h2ular2 <- rpart(h2ula$hu.y~., data=h2ula, controls = ctree_control(mincriterion = 0.85, minsplit = 0, minbucket = 0))

#Subsetting only high utilizers
h2hu <- subset(h2, h2$hu==1)
write.csv(h2hu, "h2updatedhu.csv")

h2hu <- read.csv("h2updatedhu.csv")
#removing autoincrement column
h2hu <- h2hu[,-1]
h23 <- h2hu[1:5,]

h2g <- data.frame(h2con, h2catfac)
h2g$BENE_ID <- h2$BENE_ID
#Subsetting only high utilizers
h2ghu <- subset(h2g, h2g$hu==1)
#Random sample of 10K patients for Gower dissimilarity calculation
h2ghu2 <- sample_n(h2ghu, 10000)
#emoving hu flag, bene_id, and final cost
h2ghu3 <- h2ghu2[,-c(50,260,261)]

#Gower distance using daiy package

h2ghu3g <- daisy(h2ghu3, metric = "gower")
h2ghu3gm <- as.matrix(h2ghu3g)
h2ghu3gms <- apply(h2ghu3gm,2,sort)
h2ghu3gms6 <- as.data.frame(h2ghu3gms[6,])

write.csv(h2ghu3gms6, "sigmavalhu.csv")  #Saving 6th closest point for each point

#Converting dissimliarity to gaussian similarity measure 
h2ghu3gmsq <- h2ghu3gm^2
h2ghu3gms6 <- read.csv("sigmavalhu.csv")
h2ghu3gms6 <- h2ghu3gms6[,-1]
#dim(h2ghu3gmsq)
#m <- matrix(, nrow=10000, ncol=10000)
# for (r in 1:10000){
#   for(c in 1:10000){
#     m[r,c] <- exp(-h2ghu3gmsq[r,c]/(2*h2ghu3gms6[r,1]*h2ghu3gms6[c,1]))
#   }
# }
h2ghu3gms6 <- as.data.frame(h2ghu3gms6)
for (r in 1:10000){
  for(c in 1:10000){
    m[r,c] <- exp(-h2ghu3gmsq[r,c]/(2*h2ghu3gms6[r,1]*h2ghu3gms6[c,1]))
  }
}

#tSNE embedding
humtsne <- Rtsne(m, is_distance=TRUE)
plot(humtsne$Y)c

#clustering using k means with 3 clusters
humtsnek3 <- kmeans(humtsne$Y,3,nstart=20)
plot(humtsne$Y, color= humtsnek3$cluster)
plotcluster(humtsne$Y, humtsnek3$cluster)


#hu10 <- as.data.frame(humtsne$Y)
#View(hu10)
#View(hu10)
#hu10 <- as.data.frame(humtsnek3$cluster)
#View(hu10)
hu10c <- as.data.frame(humtsnek3$cluster)
hu10 <- as.data.frame(humtsne$Y)
hu10$c <- hu10c
#View(hu10)
h2ghu3$c <- humtsnek3$cluster

h2ghu3con <- h2ghu3[,c(1:17,259)]
kruh2ghu3con <- apply(h2ghu3con[,-18],2,function(x) kruskal.test(x~hu10c$`humtsnek3$cluster`)$p.value)
aggregate(h2ghu3con[-18], by=list(cluster=fit.km$cluster), mean)

h2ghu3con$cluster <- hu10c$`humtsnek3$cluster`

h2gmeancon <- aggregate(.~h2ghu3con$cluster, h2ghu3con, mean)

h2ghu3cat <- h2ghu3[,c(18:258)]
chih2ghu3cat <- apply(h2ghu3cat[,-c(127:129,242)],2,function(x) chisq.test(x,h2ghu3cat[,242])$p.value)
tableh2ghu3cat <- apply(h2ghu3cat[,-c(242)],2,function(x) table(x,h2ghu3cat[,242])) #list of frequency tables

#creating sorted list of feature varaibles based on importance of seperating clusters

tableh2ghu3catn <- lapply(tableh2ghu3cat,function(m) m/colSums(m)[col(m)]) #normalized frequency tables
tableh2ghu3catnt <- lapply(tableh2ghu3catn, function(m) t(m))

h2ghu3catrank <- lapply(tableh2ghu3catn, function(m) (sqrt(sum((m[,1] - m[,2]) ^ 2))+ sqrt(sum((m[,2] - m[,3]) ^ 2))+ sqrt(sum((m[,1] - m[,3]) ^ 2)))/3)

h2ghu3catranks <-  sort(unlist(h2ghu3catrank), decreasing=TRUE) 

#Plotting histograms for contionus features
h2ghu3con%>%
  gather(-c, key="var", value="value") %>% 
  ggplot(aes(x=value, color=c)) +
  geom_density(alpha=0.25)+ facet_wrap(~ var, scales = "free")