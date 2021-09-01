height <- c(121,164,148,186,178,156,179,163,152,131,138,166,189,123,178,161,149,189,176,159) #Independent variable
weight <- c(53,71,56,79,68,55,67,62,51,48,53,68,83,54,74,58,52,78,67,52) #Dependent variable

#Linear modelling using lm()
model1 <-lm(weight~height) 

#Assumptions for linearity -
#1.Outlier Test ()
outlierTest(model1) #Since p value is lesser than 0.05, hence there is no outliers.

#2.Shaprio-Wilk test
a<-residuals(object=model1)
shapiro.test(x=a) #Since p value is greater than 0.05, the data is normal. Special case for 'p' so, setting the null hypothesis as data is normal and alternative being data is not normal.

#3. Skewness (Acceptable normal range is -1 to +1)
skewness(height)
skewness(weight)

#4. Kurtosis (Acceptable normal range is -3 to +3)
kurtosis(height)
kurtosis(weight)


#Assumptions for non-linearity -
cor.test(weight,height) #Since p is lesser than 0.05. Hence we accept alternative null hypothesis there is linearity between the variable. 
#0.859 (cor) correlation means that there is 85.9% correlation between the variables.

model1
#Hence, weight = -8.295 + 0.4413 * height is the linear line equation.

summary(model1)
#Gives us R^2 and adjusted R^2 values.


#The function lm() establishes a regression equation between the two variables height and weight. Wherein, height is independent variable and weight as dependent variable. The equation formed is weight = -8.295 + 0.4413 * height.
#The summary function of model1 gives us high value of R^2 i.e 0.7389 showing that the model is good and can be used for prediction.


#Now predicting the model
newheight <- data.frame(height=192)
newweight <- predict(model1,newheight)
#The predict function predicts the value for weight depending on the new height value given.
