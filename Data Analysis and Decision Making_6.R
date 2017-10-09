
library("ggplot2")
library("GGally")

library("gridExtra")
#Part B: Residuals and Transforms
#Exercise 1: Residuals Plot
#a - Obtain plot
#Obtain the Residuals vs Fitted Plot of the fitted model with an added horizontal line at 
#y=0
#Do the points look randomly distributed about the line?

ToyotaPrices <- read.csv("C:/Users/aksha/Downloads/ToyotaPrices.csv")
names(ToyotaPrices)
myData_PKWT = subset(ToyotaPrices, select = c(Price, KM, Weight, Tow_Bar))

#Exercise 1
fit = lm(Price~ KM + Weight + Tow_Bar, data= myData_PKWT)
plot(fit$fitted.values,fit$residuals)
abline(h=0,v=NULL)


#yes the points look randomly distributed about the line.

#Exercise 2
# b - Obtain plot using the z-scores of the resiudals
# Repeat the Residuals vs Fitted Plot using z-scores of the residuals. 
# Add empirical rule horizontal lines at 
# +2 and ???2. Use these lines to judge whether or not the residuals are normal or
# there are outliers. Point out any outliers. Point out any floor or ceiling effects. Do you think the residuals are normal?

x=fit$residuals
mean(x)
y=(x-mean(x))/sd(x)
plot(y,fit$fitted.values)
abline(h=NULL,v=-2)
abline(h=NULL,v=2)


mod= fortify(fit)
plot1= qplot(.stdresid, data= mod , geom= "histogram")
plot2= qplot(.stdresid, data= mod , geom= "density")
plot3= qplot(sample=.stdresid, data= mod , geom= "qq") + geom_abline()

grid.arrange(plot1,plot2,plot3,nrow=1)



#The residuals  normal and there are outliers that are present as well.


plot(floor(y),fit$fitted.values)


#If we have empirical rule between +2 and -2 then we have many outliers.


plot(ceiling(y),fit$fitted.values)


#If we have empirical rule betwwen +2 and -2 then we just have less outliers.

#Exercise 2

#2a
#a - The Plot
#Obtain the normal probability QQ-Plot of the residuals.

qqnorm(fit$residuals) 
qqline(fit$residuals) 


#2b - Normality
#Do the residuals look normal?
#the residuals look normal but there are some outliers


#Exercise 3

#3a - Obtain the composite goodness-of-fit plots
#Obtain the composite goodness-of-fit plots for the fitted model. What plots involve the residuals? Do the Residuals vs Fitted Plot and the Normal QQ-Plot look about the same as those obtained earlier?

plot(fit)


#Residuals vs Fitted Plots & Residuals vs leverage Plot involves residuals
#the Residuals vs Fitted Plot and the Normal QQ-PLot are the same as those obtained earlier

#3b-b - Outliers
#We examine the Residuals vs Leverage Plot in the composite goodness-of-fit plots. Outliers points will be identified by their row name. Are there any outliers? If so, what are their row names.
#Yes there are outliers
#602,961,222 are the row names of the outliers