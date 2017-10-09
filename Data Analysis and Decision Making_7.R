

#Assignment 7
#Exercise 1 

#Create factors, remove NAs
#Process the data to make factors with named levels for race, smsa, and pt.
#Replace the indicator variables ne, mw, so, and we with one factor called region with levels named: ne, mw, so, and we*.
#Remove all NAs.

#load data
#1(a)
library("faraway")

require(car)
data("uswages")
summary(uswages)
uswages$exper[uswages$exper <0] <-NA

#1(b)
# convert race, smsa, and pt to factor variables
uswages$race <- factor(uswages$race)
levels(uswages$race) <- c("White","Black")
uswages$smsa <- factor(uswages$smsa)
levels(uswages$smsa) <- c("No","Yes")
uswages$pt <- factor(uswages$pt)
levels(uswages$pt) <- c("No","Yes")

#1(c)
summary(uswages)

# create region, a factor variable based on the four regions ne, mw, so, we
uswages <- data.frame(uswages,
                      region =
                        1*uswages$ne +
                        2*uswages$mw +
                        3*uswages$so +
                        4*uswages$we)
uswages$region <- factor(uswages$region)
levels(uswages$region) <- c("ne","mw","so","we")

# delete the four regions ne, mw, so, we
uswages <- subset(uswages,select=-c(ne:we))

#1(d)
# Take care of NAs
uswages <- na.omit(uswages)

#Exercise 2
summary(uswages)

#Exercise 3
#Compute OLS fit to model log(wage)~.
#Perform the Shapiro-Wilk Test of Normality for the residuals, what is the conclusion?

g1 = lm(log(wage) ~ ., data = uswages)
summary(g1)

shapiro.test(residuals(g1))

#The Shapiro-Wilk test rejects normality of errors.

#Exercise 4
#Compute WLS fit to model log(wage)~. and weights = 1/(1+educ)
#Perform the Shapiro-Wilk Test of Normality for the residuals, what is the conclusion?
summary(uswages)

g2 = lm(log(wage) ~ ., data = uswages, weight = 1/(1+ educ))
summary(g2)

compareCoefs(g1,g2)

shapiro.test(residuals(g2))

#The Shapiro-Wilk test rejects normality of errors.

#Exercise 5
#Compute Robust fit to model log(wage)~. using Huber, Hampel, Biquare, LTS, and LAD
#Compare coefficients of the above fits using OLS, WLS, Huber, Hampel, Biquare, LTS, and LAD
#Which would you recommend?
#Why?
ols= lm(log(wage)~ . , data = uswages )

wls = lm(log(wage) ~ ., data = uswages, weight = 1/(1+ educ))

#Huber M - estimation
library(MASS)
huber = rlm(log(wage) ~ .,psi= psi.huber, data = uswages )

#Tukey Bisquare M-estimation
bisquare = rlm(log(wage) ~ .,psi= psi.bisquare,init="lts",maxit=100 ,data = uswages )

#Hample M-estimation
hample = rlm(log(wage) ~ .,psi= psi.hampel,init="lts",maxit=100 ,data = uswages )

#Least Trimmed Squares(LTS)
#install.packages("robustbase")
library(robustbase)
lts = ltsReg(log(wage) ~ .,data = uswages )

lts_exact =ltsReg(log(wage) ~ .,data = uswages ,
                  nsamp = "exact")

#Least Absolute Deviation
library(quantreg)
lad = rq(log(wage) ~ .,data = uswages )


coefs = compareCoefs(ols,wls,huber,bisquare,hample,lts,lts_exact,lad,se = FALSE)

colnames(coefs) = c("OLS","WLS","Huber", "Bisquare", "Hample", "LTS", "LTS-exact", "LAD")
coefs

#We see that LTS and LTS-exact appear to agree with each other and both are very different different from OLS.
#All three M-estimation methods, Huber, Bisquare, and Hample are different from each other, and different from OLS and both LTS's.
#LAD is similar to OLS.
#LTS is recommended since it has the best breakdown.