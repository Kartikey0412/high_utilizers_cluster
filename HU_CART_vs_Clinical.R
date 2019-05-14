#############################################################################################################
#CART vs Clinical decision high utilizers precicitons
#############################################################################################################
library(dplyr)
library(rpart)
library(caret)

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

predictrm <- predict(h2ularm, test[,-258], type= "class") #accuracy= 95.069%


predictrm2 <- predict(h2ularm2, test[,-258], type= "class") #accuracy= 91.539
