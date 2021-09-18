#Model Building using house prices dataset
a<-read.csv("house_prices.csv")
View(a)
boxplot(a$Price~a$Bedrooms)
summary(a)
str(a)

#Anova: To check the relationship between bedroom and price
#H0: There is no relationship between price and bedroom
#H1: There is a relationship between price and bedroom
an<-aov(a$Bedrooms~a$Price)
summary(an)
#P-value<2e-16 which is less than 0.05, hence reject null hypothesis i.e there is relationship between price and bedroom.

#Anova: To check the relationship between price and neighborhood
#H0: There is no relationship between price and neighborhood
#H1: There is a relationship between price and neighborhood
an2<-aov(a$Price~a$Neighborhood)
summary(an2)
#p value<0.05, hence reject null hypothesis i.e there is relationship between price and neighborhood.


#Correlation testing between price and bedroom. (default is Karl-Pearson)
cor(a$Price,a$Bedrooms)
#value=0.52, hence 52 % correlation

cor(a$Price,a$Bathrooms,method="spearman")
#value=0.50 ,hence 50% correlated


#split data to test the models to check if its working or not. We keep a part of the data to test data and train data. More for train data to build model and less for test data to test.
set.seed(1)
b<-sample(80,replace=F) #indicating that we are taking 80 rows without replacing
train_data<-a[b, ] #selecting the train data using sample function
test_data<-a[-b, ] #remaining rows we have put in test_data. Use - symbol to indicate remaining values
View(train_data)
View(test_data)

#Model building on train data and later we build on test. 

model1<-lm(train_data$Price~train_data$SqFt,data=train_data) #dependent~independent
summary(model1)

#R Squared values says that 33 percent is explained by square feet on price. We have to add other factors too since the R squared value is less. 
#Hence, we build another model with more predictors added to independent variables.

model2<-lm(train_data$Price~train_data$Bedrooms+SqFt,data=train_data) #Thus we add bedrooms to sqft.
summary(model2)
#Now,the adjusted R square increases, and starts to decrease the values after keeping on adding the variables.
#This is because - the fundamental point is that when you add predictors to your model, the multiple Rsquared will always increase.
#and the adjusted Rsquared controls against this increase so adjusted Rsquared will get decreased and adds penalties for the number of predictors in the model.

#Adding a predict column to the train data set
train_data$prediction<-predict(model2)
train_data$residual<-residuals(model2)

#Visualizing the model
plot(train_data$prediction,train_data$residual)
hist(train_data$residual)
hist(train_data$prediction)
plot(model2)

#Validate the model (doesn't run rn but syntax is correct)
test_data$pred<-predict(model2,test_data) #Predicting the values of model2 against the test data.
R_Sq<-cor(test_data$Price,test_data$pred)*cor(test_data$Price,test_data$pred) #Checking the accuracy by using correlation * correlation to get R^2. (since correlation is aka R)


#We use dummy variables when the DV (dependent variable) is numerical and IDV are cateogrical. We set the IDV split to dummy variables to be used as our predictors (newly formed IDV).
#Dummy variables - It used to represent categorical data (by mapping numerical/alphabetic data to 0s and 1s) and can be used for modelling. Before the data is split, dummy variables should be added if required for any categorical data that is present in the dataset.
#We create n-1 variables for dummy COLUMN variables where n is number of categories.

#Brick has YES and NO in the house prices dataset, hence put values 1 and 0. Where, 1 is brick's YES and 0 is NO. 
house<-read.csv("house_prices.csv")
#Creating dummy column variables for Bricked (Since Brick has 2 variables so n-1 = 2-1 = 1 we need 1 dummy column variable that is Bricked)
house$Bricked <-ifelse(house$Brick=='yes',1,0) #Adds a new column for Bricked where it shows 1 for yes and 0 for no.

#Create dummy variables for neighborhood. (Neighborhood has 3 variables so we do n-1 i.e 3-1 = 2 dummy column variables are needed)
house$East<-ifelse(house$Neighborhood=='East',1,0) #Adds a new column for East where it shows 1 for East and 0 if not East.
house$West<-ifelse(house$Neighborhood=='West',1,0) #Adds a new column for West where it shows 1 for West and 0 if not West.



