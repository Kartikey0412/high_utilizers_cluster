#loading required libraries
library(lubridate)
library(dplyr)
library(StatMatch)

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
