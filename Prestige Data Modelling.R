library(carData)
library(car)
data<-read.csv("C:/Users/punkl/Desktop/ISBR/5th Tri/Predictive Analytics/R/rdataset-master/csv/car/Prestige.csv")
View(data)
summary(data)

#Model 1
#Relationship between average years of education and income
t.test(data$education,data$income)
#H0: There is no relationship between average years of education and income
#H1: There is a relationship between average years of education and income
#Since p < 0.05, we accept H1 (alt hypothesis) for there is a relationship between average years of education and income.

#Building the model
model1<-lm(data$income~data$education,data=data) 

#Assumptions for linearity
outlierTest(model1) #Outlier test (p < 0.05, it has outliers)
shapiro.test(model1) #Shapiro Wilk test (Since p < 0.05, data is not normal)
skewness(data$education) #Data is normal since lies between -1 and +1
skewness(data$income) #Data is skewed since it is greater than 1
kurtosis(data$education) #Data is normal since it lies between -3 and +3
kurtosis(data$income) #Data is skewed since is it is greater than 3

#Assumptions for non linearity
cor.test(data$income,data$education) #Correlation test results in 57% correlation

summary(model1) #Since adjusted R square is ~32%, we need to add more independent variables to the model.

lvif(model1) #Since it contains lesser than 2 terms, we add more independent variables to the model to get VIF value.

#Model 2
#Relationship between 

#Note: If VIF > 10, remove or else keep the model.
