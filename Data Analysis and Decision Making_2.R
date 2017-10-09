##Question 1
ToyotaPrices <- read.csv("C:/Users/aksha/Downloads/ToyotaPrices.csv")

library(e1071) 

summary(ToyotaPrices)
skewness(ToyotaPrices$Id)
skewness(ToyotaPrices$Price)
skewness(ToyotaPrices$Age_08_04)
skewness(ToyotaPrices$cc)
skewness(ToyotaPrices$Mfg_Month)
skewness(ToyotaPrices$Radio)
skewness(ToyotaPrices$Mfg_Year)
skewness(ToyotaPrices$KM)
skewness(ToyotaPrices$HP)
skewness(ToyotaPrices$Automatic)
skewness(ToyotaPrices$Doors)
skewness(ToyotaPrices$Cylinders)
skewness(ToyotaPrices$Gears)
skewness(ToyotaPrices$Quarterly_Tax)
skewness(ToyotaPrices$Weight)
skewness(ToyotaPrices$Mfr_Guarantee)
skewness(ToyotaPrices$BOVAG_Guarantee)
skewness(ToyotaPrices$Guarantee_Period)
skewness(ToyotaPrices$ABS)
skewness(ToyotaPrices$Airbag_1)
skewness(ToyotaPrices$Airbag_2)
skewness(ToyotaPrices$Airco)
skewness(ToyotaPrices$Automatic_airco)
skewness(ToyotaPrices$Boardcomputer)
skewness(ToyotaPrices$CD_Player)
skewness(ToyotaPrices$Central_Lock)
skewness(ToyotaPrices$Powered_Windows)
skewness(ToyotaPrices$Power_Steering)
skewness(ToyotaPrices$Mistlamps)
skewness(ToyotaPrices$Sport_Model)
skewness(ToyotaPrices$Backseat_Divider)
skewness(ToyotaPrices$Metallic_Rim)
skewness(ToyotaPrices$Radio_cassette)
skewness(ToyotaPrices$Tow_Bar)



#Positive Skew  - Id,Price,Mfg_Month, Mfg_Year,KM,HP,Automatic,cc,Gears,Quarterly_Tax,
#Weight,Mfr_Guarantee,Guarantee_Period, Automatic_airco,Boardcomputer,CD_player
#Radio,Mistlamps,Sport_Model,Metallic_Rim,Radio_cassette,Tow_Bar

#Negative Skew - Age_08_04,doors,BOVAG_Guarantee,ABS,Airbag_1,Airbag_2,Airco,Central_Lock,Powered_windows,
#Power_steering, Backseat_Divider.

#no skew - Cylinders

#is.na(ToyotaPrices)   #return TRUE if any values are missing

#There are no values that can be declared missing because all the attributes have values
#and therefore we dont convert the missing values to NA

summary(ToyotaPrices)

##Question 2

#converting categorical variables to factors


ToyotaPrices$Automatic=factor(ToyotaPrices$Automatic)
ToyotaPrices$Doors=factor(ToyotaPrices$Doors)
ToyotaPrices$Cylinders=factor(ToyotaPrices$Cylinders)
ToyotaPrices$Gears=factor(ToyotaPrices$Gears)
ToyotaPrices$Mfr_Guarantee=factor(ToyotaPrices$Mfr_Guarantee)
ToyotaPrices$BOVAG_Guarantee=factor(ToyotaPrices$BOVAG_Guarantee)
ToyotaPrices$Airbag_1=factor(ToyotaPrices$Airbag_1)
ToyotaPrices$Airbag_2=factor(ToyotaPrices$Airbag_2)
ToyotaPrices$Airco=factor(ToyotaPrices$Airco)
ToyotaPrices$Automatic_airco=factor(ToyotaPrices$Automatic_airco)
ToyotaPrices$Boardcomputer=factor(ToyotaPrices$Boardcomputer)
ToyotaPrices$ABS=factor(ToyotaPrices$ABS)
ToyotaPrices$CD_Player=factor(ToyotaPrices$CD_Player)
ToyotaPrices$Powered_Windows=factor(ToyotaPrices$Powered_Windows)
ToyotaPrices$Power_Steering=factor(ToyotaPrices$Power_Steering)
ToyotaPrices$Radio=factor(ToyotaPrices$Radio)
ToyotaPrices$Mistlamps=factor(ToyotaPrices$Mistlamps)
ToyotaPrices$Backseat_Divider=factor(ToyotaPrices$Backseat_Divider)
ToyotaPrices$Metallic_Rim=factor(ToyotaPrices$Metallic_Rim)
ToyotaPrices$Radio_cassette=factor(ToyotaPrices$Radio_cassette)
ToyotaPrices$Tow_Bar=factor(ToyotaPrices$Tow_Bar)
ToyotaPrices$Central_Lock=factor(ToyotaPrices$Central_Lock)
ToyotaPrices$Sport_Model=factor(ToyotaPrices$Sport_Model)



summary(ToyotaPrices)

##Factors with Unbalanced counts
#The factor variables that have unbalanced counts are as follows:
#Automatic
#Mfr_Guarantee
#BOVAG_Guarantee
#ABS
#Airbag_1
#Airbag_2
#Airco
#Automatic_airco
#Boardcomputer
#CD_Player
#Central_Lock
#Powered_Windows
#Power_Steering
#Radio
#Mistlamps
#Sport_Model
#Backseat_Divider
#Metallic_Rim
#Radio_cassette
#Tow_Bar




#question 3


#histogram
hist(ToyotaPrices$Price, xlab='Price')

#Desnity
plot(density(ToyotaPrices$Price), main='Price')

#Sort
plot(sort(ToyotaPrices$Price), ylab = 'Price')

require(ggplot2)

#QQ Plot
qqnorm(ToyotaPrices$Price)

#the variable is normal.
#the variable Price is positively skewed.
#there are clusters that have been formed.

#question 4

#with plot()
plot(ToyotaPrices$Price,ToyotaPrices$KM)

#with qplot()
qplot(ToyotaPrices$Price,ToyotaPrices$KM, xlab='Price', ylab='KM')

#The relation looks like a curve.

#Question 5

#box-whisker Plot
boxplot(ToyotaPrices$Price~ToyotaPrices$ABS, xlab="ABS" , ylab="Price")

#Yes, automobiles with anti-locking breaks tend to have a higher price
#Yes there are outliers for ABS as wekk as Non-ABS.

#Question 6

cor(x = ToyotaPrices$Price, y= ToyotaPrices$KM)

#It is a weak correlation.
#It is negative and and thus it means that as the KM decreases the Price of the automobile increases.

