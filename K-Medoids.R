#K-Medoids is a method wherein instead of mean being the center of the cluster, is one of the actual observations from the cluster.
#Using the World Bank Data in this exercise

install.package('WDI')
require(WDI) #AKA library(WDI)

#To pull information on the indicators for all the countries in the list, some countries dont have data
indicators<-c("BX.KLT.DINV.WD.GD.ZS","NY.GDP.DEFL.KD.ZG","NY.GDP.MKTP.CD","NY.GDP.MKTP.KD.ZG","NY.GDP.PCAP.CD","NY.GDP.PCAP.KD.ZG","TG.VAL.TOTL.GD.ZS")

#Assigning indicators for countries that dont have data
wbinform<-WDI(country="all",indicator=indicators,start=2011,end=2011,extra=TRUE)

#To remove aggregated information
wbinform<-wbinform[wbinform$region!="Aggregates",]

#To remove countries where values are NA
wbinform<-wbinform[which(rowSums(!is.na(wbinform[,indicators]))>0),]

#To remove rows where ISO is missing
wbinform<-wbinform[!is.na(wbinform$iso2c),]

#Set rownames so as to know the country without using it for clustering
rownames(wbinform)<-wbinform$iso2c

#Refactorize region,income and lending to account
wbinform$region<-factor(wbinform$region)
wbinform$income<-factor(wbinform$income)
wbinform$lending<-factor(wbinform$lending)

#To find columns to keep
keepcols<-which(!names(wbinform)%in%c("iso2c","country","year","capital","iso3c"))

#Fit the clustering
library(cluster)
wpam<-pam(x=wbinform[,keepcols],k=12,keep.diss=TRUE,keep.data=TRUE)

#To see the medoid observations
wpam$medoids

#Making a silhouette plot
plot(wpam,which.plots=2,main='Silhouette Plot') 

#Plot interpretation
#Silhouette plot for country clustering where each line represents an observation and each grouping of lines is a cluster.
#Observations that fit the cluster well have large positive lines and observations that dont fit well have small or negative lines.
