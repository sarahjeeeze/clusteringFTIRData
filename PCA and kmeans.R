library(data.table)

par(pin=c(5,4),mfrow=c(1,1),font=2,ps=10,family="sans")
x <- seq(900,2300,2)
#import file
file <- 'C:/ftirdb/R code/alldata.csv'

spectra <- read.csv(file, header = TRUE, row.names=NULL)

#pca using dataframe
pca <- prcomp(spectra,
              center = TRUE,
              scale =TRUE,
              rank. = 10,
)
summary(pca)

plot(pca$x[, 1], pca$x[, 2], pch=10, bg=c("blue","cyan","green","red"), main =
       "PCA", xlab = "PC1", ylab = "PC2") 


#create dataframe with pca
mydata<-data.frame(pca$x[, 1],pca$x[, 2]) 
#calculate sum of squares and plot
withingroupss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:10) withingroupss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:10, withingroupss[1:10], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


#use sum of squares to decide best number of clusters to use 
#create clusters using kmeans
myclusters <- kmeans(spectra,4) 
#label the data
one <- c(rep("Lympho",100),rep("Epithel",100),rep("Erythr",100),rep("Fibro",101))
mynewdata <- data.frame(mydata, myclusters$cluster,one)
View(mynewdata) 
#outputclusters
library(cluster)
#initial plot
clusplot(mynewdata, myclusters$cluster,color=TRUE, shade=TRUE, labels=3, lines=0)

library(ggplot2)
library(plyr)
mydata$cluster <- as.character(myclusters$cluster)
mynewdata <- rename(mydata, c("pca.x...1."="comp1", "pca.x...2."="comp2"))
#make the plot look nicer with Ggplot :) 

p <- ggplot() +
  geom_point(data = mynewdata, 
             mapping = aes(x = comp1, 
                           y = comp2, 
                           colour = one,
                           ))


print(p)

