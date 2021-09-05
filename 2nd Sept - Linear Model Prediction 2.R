mtcars
structure(mtcars)
#Find the dependent and independent variable
#mpg- miles per gallon - dependent , disp - Independent
#gear,cylinder,carb- independent
#horsepower- independent

#Linear model for mpg and disp
mpg<-mtcars$mpg
disp<-mtcars$disp
model1<-lm(mpg~disp)
model1



#Assumptions:
#Normality Test:
library(car)
outlierTest(model1)
#P-value=0.55475 is greater than 0.05, hence there are outliers or residuals
#plot the model to find the outliers and remove the rows
plot(model1,4)
#To remove the row use,
newmtcars1<-mtcars[-25, ]
newmtcars2<-mtcars[-20, ]
newmtcars3<-mtcars[-18, ]
View(newmtcars3)

#model with new dataset
modelNew<-lm(newmtcars3$mpg~newmtcars3$disp)
modelNew
plot(modelNew,4)

outlierTest(modelNew)


View(newmtcars1)
model2<-lm(mpg~)
plot()


#2.Residual
resi<-residuals(model1)
shapiro.test(x=resi)
#H2:Data is#H0:Data is normal
not normal
#P-value=0.03255>0.05, hence accept null hypothesis, hence the data is normal

#3.skewness
library(moments)
skewness(mpg)
#Value=0.6404, which is between -1 to +1 , hence the data is not skewed. Therefore data is normal
skewness(disp)
#value=0.40027, which is between -1 to +1, hence the data is not skewed.The data is normal


#kurtosis
kurtosis(mpg)
#Value=2.7 which is between -3 to +3, hence no kurtosis.The data is normal
kurtosis(disp)
#value=1.910 which is between -3 to +3, hence no kurtosis.The data is normal

#Linearity
#H0:No relationship between mog and displacement
#H1: THere is a relationship between mpg and displacemet
cor.test(mpg,disp)
#p-value=9.38e-10 <0.05, hence reject null hypothesis, there is a relationship between mpg and disp




#Use hp(dependent) and disp(independent)
mtcars
model_new<-lm(mtcars$hp~mtcars$disp)
model_new
newmtcars4<-mtcars[-31, ]
outlierTest(model_new)

plot(model_new,1)

model_new2<-lm(newmtcars4$hp~newmtcars4$disp)
model_new2
plot(model_new2,4)
outlierTest(model_new2)

r<-residuals(model_new2)
shapiro.test(x=r)
#ho:Data is normal
#h1:data is not normal
#p-value=0.06>0.05, hence accept null hypothesis
#Data is normal

skewness(newmtcars4$hp)
#value=0.433 between -1 and +1, hence the data is normal

skewness(newmtcars4$disp)
#value=0.449, between -1 an +1, hence data is normal

kurtosis(newmtcars4$hp)
kurtosis(newmtcars4$disp)
#value between -3 to +3, hence normal