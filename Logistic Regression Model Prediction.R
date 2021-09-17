#In linear regression it was was in the form of y=mx+c.
#If y=0 or 1 i.e the dependent variables are 0 or 1 (binomial) we can't use linear regression hence we use logistic regression.

data<-read.csv("florence.csv")
View(data)
#Dataset description
#M - Monetory value
#Receny - How recent was the purchase
#Florence - Book bought or not
#We can use this dataset to predict if the people have purchased the books using florence column(0,1) through logistic regression.
#GLM (Generalized linear model is inherited for Linear Model as special linear modelling for logistic regression). Here, family used to specify binomial form. We are using Florence and M as dependent and independent respectively.
model1<-glm(Florence~M,data=data,family=binomial()) #Family is binomial so as to get 0s and 1s in output.
summary(model1) #If p < 0.05, it is significant. AIC - Akaikes's Information Criterion. Lower the AIC, better the model since it means there is lesser variation.

#Model prediction
data$pred<-predict(model1,type='response') #Predicting the model. The type="response" option tells R to output probabilities of the form P(Y = 1|X) resulting between 0 and 1 (since its probabilities).
View(data) #To view the newly updated data, wherein pred column was added after prediction.

#Creating threshold to result for the pred column where it shows that if it is >0.1 it is 1 and <0.1 it is 0.
data$purch<-ifelse(data$pred>0.1,1,0)

#Viewing the confusion matrix to check validity of prediction
table(Actual=data$Florence,Predicted=data$purch) #Gets us the confusion matrix for predicted and actual values. Where we defined Actual for actual values of florence and Predicted for predicted values.
#Actual values of 0 shows that people did not buy the book but predicted shows 382 meaning that it is wrong prediction.(A0-P1 should be low, FALSE POSITIVE)
#Actual values of 1 shows that people did buy the book but predicted shows 85 did not meaning that it is wrong prediction.. (A1-P0 should be lower values FALSE NEGATIVE)
#Similarly, A0-P0 should be high (TRUE NEGATIVE), A1-P1 should be high (TRUE POSITIVE)


#Hence, we create another model to get better confusion matrix
model2<-glm(Florence~M+ArtBks,data=data,family=binomial())
summary(model2) #AIC is lesser than model1, so we continue

#Model prediction
data$pred<-predict(model2,type='response')
View(data)

#Defining threshold
data$purch<-ifelse(data$pred>0.1,1,0)
table(Actual=data$Florence,Predicted=data$purch) #This shows that model 2 is best so far.


#Building 3rd model
model3<-glm(Florence~M+ArtBks+CookBks,data=data,family=binomial())
summary(model3) #AIC is lesser than model2, so we continue

#Model Prediction
data$pred<-predict(model3,type='response')
View(data)

#Defining threshold
data$purch<-ifelse(data$pred>0.1,1,0)
table(Actual=data$Florence,Predicted=data$purch) #Showing more errors than previous model so we have to change the predictors for independent variables.


#Building 4th model
model4<-glm(Florence~M+ArtBks+CookBks+ChildBks,data=data,family=binomial())
summary(model4)

#Model prediction
data$pred<-predict(model4,type='response')
View(data)

#Defining threshold
data$purch<-ifelse(data$pred > 0.1,1,0)
table(Actual=data$Florence,Predicted=data$purch) #Showing more errors than previous model so we have to change the predictors for independent variables.


#Sensitivity = TP/FN+TP
#Specificity = TN/TN+FP
#NOTE: #glm(DV~.) #The dot gives us all the variables within the dataset
