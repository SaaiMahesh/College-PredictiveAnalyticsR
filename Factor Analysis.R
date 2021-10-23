library(MASS)
data()
data("UScrime")
crime<-UScrime
View(crime)

# y is dependent variable
str(crime)
model<-lm(y~.,data=crime)

library(car)
sqrt(vif(model))>2

install.packages("psych")
library(psych)

corUScrime<-cor(crime)

# to determine number of factors
fa.parallel(corUScrime,fa="pc",n.obs=112,show.legend = TRUE,main="Scree Plot")
result<-fa(r=corUScrime,nfactors = 5,rotate = "none",fm="pa")
result

# performing factor analysis with varimax rotation
rotatedfactor<-fa(r=corUScrime,nfactors = 5,rotate = "varimax",cutoff=0.4,fm="pa")
rotatedfactor



walmart<-read.csv("Wallmart_Dataset.csv")
View(walmart)
walmart.pca<-prcomp(walmart[,-1],centre=TRUE,scale=TRUE)
summary(walmart.pca)
# taking till PC5 because there is more variation in Cumulative Proportion and we can take as many Cumulative Proportion as we want
# plot 
plot(walmart.pca,type="l")

# factor analysis
wall.fact<-factanal(walmart[,-1],4,rotation="varimax") # here 4 is the no.of components we want to take
wall.fact

print(wall.fact,digits = 2,cutoff=0.3,sort=TRUE)

wall.fact<-factanal(walmart[,-1],4,rotation="varimax",scores = "regression",cutoff=0.3)
wall.fact$scores

wall<-cbind(walmart,wall.fact$scores)
View(walmart)
