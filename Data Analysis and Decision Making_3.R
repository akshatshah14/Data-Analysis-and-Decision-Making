#Assignment 3

ToyotaPrices <- read.csv("C:/Users/aksha/Downloads/ToyotaPrices.csv")

names(ToyotaPrices)
myData_PKWT <- subset(ToyotaPrices, select = c(Price, KM, Weight, Tow_Bar))
head(myData_PKWT)
#Exercise 1(a)

summary(myData_PKWT)
#from inspection of the median and mean we see that Price and KM are skewed.

#Exercise 1(b)
plot(density(myData_PKWT$Price),xlab = 'Price', main = 'Density plot for Price')
plot(density(myData_PKWT$KM),xlab='KM',main='Density plot for KM')    
qqnorm(myData_PKWT$Price, xlab = 'Price')
qqline(myData_PKWT$Price)
qqnorm(myData_PKWT$KM)
qqline(myData_PKWT$KM)

#Price and KM are skewed.
#KM is normally distributed and Price is not normally distributed


#Exercise 1(c)
myData_PKWT$Tow_Bar = factor(myData_PKWT$Tow_Bar)

levels(myData_PKWT$Tow_Bar) = c('no','Yes')
summary(myData_PKWT$Tow_Bar)

#Exercise 1(d)

boxplot(myData_PKWT$Price ~ myData_PKWT$Tow_Bar)

#The boxplots are different in the terms compared to Price.
#The tow_bar does not appear to predict Price

#Exercise 1(e)

boxplot(myData_PKWT$KM ~ myData_PKWT$Tow_Bar)

#The boxplots are different in terms of outliers.
#There is no prediction that can be made between the two.



#Exercise 1(f)

allt <- par(mfrow=c(1,2))
plot(Price~Tow_Bar, data=myData_PKWT)
plot(KM~Tow_Bar, data=myData_PKWT)

par(allt)

allt <- par(mfrow=c(1,2))
stripchart(Price ~ Tow_Bar, data=myData_PKWT, method = "jitter", vertical = TRUE, xlab="Tow Bar")
stripchart(KM ~ Tow_Bar, data=myData_PKWT, method = "jitter", vertical = TRUE, xlab="Tow Bar")

par(allt)
#If there is a tow_bar in the car the price of the car is less.
#When the car does not have a tow_bar there are many outliers as the car has been driven for many KMs.

#Exercise 2(a)

pairs(~ Price+ KM + Weight, data= myData_PKWT)
fit = lm(Price ~ Weight + KM, data=myData_PKWT)
fit
coef(fit)
summary(fit)
#With increase in KM the price of the car decreases
#THere is no relation between the price and weight
#Clearly there are several outliers
#KM and Weight appear to be redundant

#Exercise 2(b)
pairs(~ Price+ KM + Weight, data= myData_PKWT , col=myData_PKWT$Tow_Bar)
#It appears the relation between Price and KM is the same for cars with 
#& without a tow bar

#there is no clear relationship visible that appear to be different for group of cars
#with or without a tow bar

#Exercise 3
#Question 3(a)
options(show.signif.stars = FALSE)
fit = lm(Price~ KM + Weight + Tow_Bar, data= myData_PKWT)
summary(fit)

#Question 3(b)
summary(fit)


#The residuals appear to be a non-parametric summary of their distribution.
#The residuals appears to be skewed little on the left

#Question 3(c)
coef(fit)
#negative coefficient indicate that they are negatively corelated and inversely proptional to each other
#which indiacte that KM is negatively corelated indicating that with increase in KM there is a  decrease in Price

#question 3(d)
#The signs of slope indicates that if  it is negative it is inversely proportional
#and with positive signs it indicates that they are directly proportional.

#question 3(e)
plot(myData_PKWT$Price,myData_PKWT$KM)
#As the Km increases the Price decreases.
#The Prices go down.

#question 3(f)
plot(myData_PKWT$Price,myData_PKWT$Weight)
#As the weight increases there is no sudden change in the price

#Question 3(g)
#There is not much price difference of automobiles with and without Tow_bar with the same KM and weight. So this values does not make sense 


#Question 3(h)
summary(fit)$r.square
#This value indicates a good fitting model.


#Exercise 4
#Exercise 4(a)
deviance=deviance(fit)
y=myData_PKWT$Price
TotalSS=sum((y-mean(y))^2)
TotalSS
1-deviance/TotalSS
summary(fit)$r.square

#This value is equal to the R^2 value found in the model summary


#Exercise 4(b)
require(ggplot2)
qplot(fitted.values(fit), Price, data= myData_PKWT)+ geom_abline(intercept = 0, slope = 1)

