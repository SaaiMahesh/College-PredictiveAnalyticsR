#Homogeneous groups have similar properties and within the cluster they are same but are different compared to other clusters.
#K-Means algorithm is unsupervised algorithm. It is used to form the cluster.
#The sum of squares of differences of the distance between the centroid and the data point within the cluster gives the K-Mean value of the cluster.
#K-Means is only used for numerical data and not categorical data.

library(MASS)
data("biopsy") #Loading biopsy data
View(biopsy)
str(biopsy) #Checking structure,head and summary
head(biopsy)
summary(biopsy)

table(complete.cases(biopsy))  #Number of NAs - or table(is.na(biopsy))

biopsy2<-na.omit(biopsy) #Omitting the NAs

install.packages("NbClust")
library(NbClust)

biopsy2<-biopsy2[,-c(1,11)] #Removing 1st and 11th columns since they're categorical data and we can do K-Means only with numerical.
View(biopsy2) #Viewing to see if all categorical columns are removed.

biopsyscale<-scale(biopsy2) #Scales and centers the data for the numeric matrix, which is needed for finding the number of clusters.
View(biopsyscale)

#Checking the optimal number of clusters to have
newclust<-NbClust(biopsyscale,distance='euclidean',min.nc = 2,max.nc=15,method = "average") #Always minimum number of clusters are 2 and max can be any. Method = "average" means average of all.
#Hence, the best number of clusters is 3.

#Finding the K-Means for 3 clusters (since optimal number of clusters is 3)
set.seed(8) #Setting seed for fixing a random number for the data rows
kmeansbiopsy<-kmeans(biopsy2,centers=3)
kmeansbiopsy$size #Sizes of each cluster gets generated

install.packages('useful')
library(useful) 
plot(kmeansbiopsy,data=biopsy2) #Plots the clusters from the cleaned data i.e biopsy2

#Finding the K-Means for 3 clusters with a starting point 
set.seed(8) #Setting seed for fixing a random number for the data rows
kmeansbiopsy2<-kmeans(biopsy2,centers=3,nstart=25)
kmeansbiopsy2$size #Sizes of each cluster gets generated

plot(kmeansbiopsy2,data=biopsy2)

#Plotting the clusters with K-Means
library(cluster)
clusplot(biopsy2,kmeansbiopsy$cluster,color=TRUE,shape=TRUE,labels=4,cex=2,main='Biopsy Clustering') #Note: Cex is the size of the plot getting generated

#Output for clusters and cleaned data
biopsy3<-na.omit(biopsy) #Retaining the columns of categories without the NAs.
predictbio<-table(biopsy3$class,kmeansbiopsy$cluster)
predictbio #Shows how many are benign and malignant

install.packages("flexclust")
library(flexclust)
randIndex(predictbio) #ARI shows the accuracy, wherein we get 49% so we need to get better


#Finding better K-Means for higher accuracies by randomly using different centers and nstart
#Finding the K-Means for 2 clusters with a starting point of 25
set.seed(8) #Setting seed for fixing a random number for the data rows
kmeansbiopsy3<-kmeans(biopsy2,centers=2,nstart=25)
kmeansbiopsy3$size #Sizes of each cluster gets generated
plot(kmeansbiopsy2,data=biopsy2)
clusplot(biopsy2,kmeansbiopsy3$cluster,color=TRUE,shape=TRUE,labels=4,cex=2,main='Biopsy Clustering')
predictbio<-table(biopsy3$class,kmeansbiopsy3$cluster)
randIndex(predictbio) #Gives us 84% accuracy, trying for higher again..

#Finding the K-Means for 4 clusters with a starting point of 25
set.seed(8) #Setting seed for fixing a random number for the data rows
kmeansbiopsy4<-kmeans(biopsy2,centers=4,nstart=25)
kmeansbiopsy4$size #Sizes of each cluster gets generated
plot(kmeansbiopsy4,data=biopsy2)
clusplot(biopsy2,kmeansbiopsy4$cluster,color=TRUE,shape=TRUE,labels=4,cex=2,main='Biopsy Clustering')
predictbio<-table(biopsy3$class,kmeansbiopsy4$cluster)
randIndex(predictbio) #Gives us 74% accuracy, trying for higher again..

#Finding the K-Means for 5 clusters with a starting point of 40
set.seed(8) #Setting seed for fixing a random number for the data rows
kmeansbiopsy5<-kmeans(biopsy2,centers=5,nstart=10)
kmeansbiopsy5$size #Sizes of each cluster gets generated
plot(kmeansbiopsy5,data=biopsy2)
clusplot(biopsy2,kmeansbiopsy5$cluster,color=TRUE,shape=TRUE,labels=4,cex=2,main='Biopsy Clustering')
predictbio<-table(biopsy3$class,kmeansbiopsy5$cluster)
randIndex(predictbio) #Gives us 40% accuracy, trying for higher again..

#Finding the K-Means for 3 cluster with a starting point of 10
set.seed(8) #Setting seed for fixing a random number for the data rows
kmeansbiopsy3<-kmeans(biopsy2,centers=3,nstart=10)
kmeansbiopsy3$size #Sizes of each cluster gets generated
plot(kmeansbiopsy3,data=biopsy2)
clusplot(biopsy2,kmeansbiopsy3$cluster,color=TRUE,shape=TRUE,labels=4,cex=2,main='Biopsy Clustering')
predictbio<-table(biopsy3$class,kmeansbiopsy3$cluster)
randIndex(predictbio) #Gives us 78% accuracy, trying for higher again.. 

#Finding the K-Means for 2 cluster with a starting point of 10
set.seed(8) #Setting seed for fixing a random number for the data rows
kmeansbiopsy2<-kmeans(biopsy2,centers=2,nstart=10)
kmeansbiopsy2$size #Sizes of each cluster gets generated
plot(kmeansbiopsy2,data=biopsy2)
clusplot(biopsy2,kmeansbiopsy2$cluster,color=TRUE,shape=TRUE,labels=4,cex=2,main='Biopsy Clustering')
predictbio<-table(biopsy3$class,kmeansbiopsy2$cluster)
randIndex(predictbio)
