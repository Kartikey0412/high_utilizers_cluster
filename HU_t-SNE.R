#############################################################################################################
#t-SNE clustering
#############################################################################################################
#here using diasy package to grab gower distance and Rtsne for doing t-SNE and ggplot for efficient visualization

library(dplyr)
library(daisy)
library(Rtsne)
library(fpc)
library(purrr)
library(tidyr)
library(ggplot2)
library(reshape2)

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

#Plotting histograms for contionus features with ggplot library
h2ghu3con%>%
  gather(-c, key="var", value="value") %>% 
  ggplot(aes(x=value, color=c)) +
  geom_density(alpha=0.25)+ facet_wrap(~ var, scales = "free")