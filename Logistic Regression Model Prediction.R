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
model1<-glm(Florence~M,data=data,family=binomial()) 
summary(model1) #If p < 0.05, it is significant. AIC - Aitkens's Information Criterion. Lower the AIC, better the model since it means there is lesser variation.

#Model prediction
data$pred<-predict(model1,type='response') #Predicting the model. The type="response" option tells R to output probabilities of the form P(Y = 1|X) resulting between 0 and 1 (since its probabilities).
View(data) #To view the newly updated data, wherein pred column was added after prediction.

#Creating threshold to result for the pred column where it shows that if it is >0.1 it is 1 and <0.1 it is 0.
data$purch<-ifelse(data$pred>0.1,1,0)
table(FR=data$Florence,PR=data$purch) #Where we defined FR for actual values of florence and PR for predicted values
