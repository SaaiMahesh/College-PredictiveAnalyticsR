#Building Linear Models
data(PimaIndiansDiabetes)
data<-PimaIndiansDiabetes
View(data)
sum(is.na(data))
sum(is.null(data))

#NOTE: #glm(DV~.) #The dot gives us all the variables within the dataset

#Model building
model1<-glm(data$diabetes~.,data=data,family=binomial()) #Using binomial family to get output in 0s and 1s.
summary(model1)
#Discarding the variables of age,insulin,triceps since the p > 0.05 so it is insignificant to our model.
model1<-glm(data$diabetes~data$pregnant+data$glucose+data$pressure+data$mass+data$pedigree,data=data,family=binomial())
summary(model1) 

#Predicting the model
data$pred<-predict(model1,type='response')
View(data)

#Setting threshold
data$preddiab<-ifelse(data$pred > 0.1,1,0)

#Viewing the confusion matrix to check validity of prediction
table(Actual=data$diabetes,Predicted=data$preddiab)

#Sensitivity (TRUE Positive Rate)
TP<-260
FN<-8
sens<-TP/(TP+FN)
sensitivity<-sens*100
sensitivity #97% positivity rate of diabetes (does have diabetes)

#Specificity (TRUE Negative Rate) 
FP<-361
TN<-139
specif<-TN/(TN+FP) 
specificity<-specif*100
specificity #27% negativity rate of diabetes (don't have diabetes)
