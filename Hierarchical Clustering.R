#Note: It is unsupervised because we are not splitting the data into train and test data.
library(cluster.datasets)
data(nutrients.meat.fish.fowl.1959)
orgdata<-nutrients.meat.fish.fowl.1959

View(orgdata)
str(orgdata)
summary(orgdata)

#Cleaning data
sum(is.na(data)) #No NAs present
data<-orgdata[,-1] #Removing categorical column

#Scaling data
datascaled<-scale(data) #Scaling data


#Creating hierarchical clusters
cluster1<-hclust(dist(datascaled),method='centroid') #Creating hierarchical clusters using centroid method - distance between the centroids of two clusters. 

cluster2<-hclust(dist(datascaled),method='average') #Creating hierarchical clusters using average method -  distance between each pair of observations in each cluster are added up and divided by the number of pairs to get an average inter-cluster distance.

cluster3<-hclust(dist(datascaled),method='complete') #Creating hierarchical clusters using complete method - is where distance is measured between the farthest pair of observations in two clusters.

cluster4<-hclust(dist(datascaled),method='single') #Creating hierarchical clusters using single method - is the shortest distance between a pair of observations in two clusters.


#Plotting the dendograms for the hierarchical clusters
plot(cluster1,main='Hierachial Cluster - Centroid') #Additional arguments are labels,cex and hang. Ex: We can do labels=orgdata$name to get names for dendogram

plot(cluster2,main='Hierachial Cluster - Average')

plot(cluster3,main='Hierachial Cluster - Complete')

plot(cluster4,main='Hierachial Cluster - Single')

#Finding the number of optimal clusters
library(NbClust)
numclust<-NbClust(datascaled,distance='euclidean',min.nc=2,max.nc=6,method='centroid') #We put max as 6 since there are max of 6 variables
#We get optimal number of clusters as 5

#Plotting the cut dendogram using the number of optimal clusters
plot(cluster1,main='Hierachial Cluster - Centroid') 
rect.hclust(cluster1,k=5)
#Interpretation: All the values within the red rectangular clusters that are cut, are of similar type when we compare to the original dataset.

#Viewing the number of data values under each cut cluster
cutcluster<-cutree(cluster1,k=5)
table(cutcluster) #This is the number of values for each cut cluster


