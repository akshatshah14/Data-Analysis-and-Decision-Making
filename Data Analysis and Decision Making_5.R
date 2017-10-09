
#Question 1

#1. Exercise
#For the prostate data infaraway, fit a model with lpsa as the response and the other variables as predictors.
#Compute 90% and 95% CIs for the parameter associated with age

require(faraway)
g=lm(lpsa ~ .,prostate)
summary(g)
confint(g,level=0.9)
confint(g,level=0.95)
#Question 2

#2. Exercise

#Compute and display a 95% joint confidence region for 
#the parameters associated with age and lbph. 
#Plot the origin and report the outcome of the appropriate hypotheses test. Affirm 
#this conclusion with an appropriate partial F-test.


require(ellipse)
plot(ellipse(g,c("age","lbph")),
     type="l",
     main="Joint Confidence Region")
points(0,0)
points(coef(g)["age"],coef(g)["lbph"],pch=18)
abline(v=confint(g)["age",],lty=2)
abline(h=confint(g)["lbph",],lty=2)


#We do not reject the null hyotheses as the origin lies inside the ellipse
#The 95% CR is equivalent to testing the full model.

g2=lm(lpsa ~ age + lbph, prostate)
anova(g2,g)
# based on the partial F-test we get to know that F value is very big and 
#p-value =2.2*10^-16 which is very small thus we do not reject the null hypotheses 
#and consider age and lbph

#Question 3
#Predict lpsa (95%) for a new patient with lcavol = 1.22692, lweight = 3.62301, age = 65, lbph = -0.3001, svi = 0.0, 
#lcp = -0.79851, gleason = 7.0, pgg45 = 15.0.
#Do this again for the mean response. Using the exp() function, obtain the new prediction and mean response for psa.


x0= data.frame(lcavol=1.22692,
               lweight=3.62301,
               age=65,
               lbph=-0.3001,
               svi=0.0,
               lcp=-0.79851,
               gleason=7.0,
               pgg45=15.0)

predict(g,
        x0,
        interval = "prediction",
        level=.95)
exp(predict(g,x0,interval = "prediction",
            level = .95))
#Question 4
#Repeat the above exercise with new patient age = 20

x0= data.frame(lcavol=1.22692,
               lweight=3.62301,
               age=20,
               lbph=-0.3001,
               svi=0.0,
               lcp=-0.79851,
               gleason=7.0,
               pgg45=15.0)

predict(g,
        x0,
        interval = "prediction",
        level=.95)
exp(predict(g,x0,interval = "prediction",
            level = .95))
#question 5

#For the model in exercise 1, remove all the predictors that are not significant at the 5% level.
#Recompute the predictions for exercises 3 and 4. Compare CIs. On the psa scale, which CIs do you prefer?

fm.1 <- lm(lpsa ~ lcavol+lweight+age+svi,prostate)
summary(fm.1)
newP.data0 <- data.frame(lcavol = 1.22692, lweight = 3.62031, age = 65,svi=0)
predict(fm.1,newP.data0,interval= "prediction",level=0.95)
exp(predict(fm.1,newP.data0,interval = "prediction",
            level = .95))
newP.data1 <- data.frame(lcavol = 1.22692, lweight = 3.62031, age = 20,svi=0)
predict(fm.1,newP.data1,interval= "prediction",level=0.95)
exp(predict(fm.1,newP.data1,interval = "prediction",
            level = .95))
#The length of the second prediction interval is longer than the first.This is also because of age
#The prediction intervals from the second model should be narrower than those from the original model theoretically because all significant values have been removed
#Therefore,the second one should explain the response more accurately than the first model
#The narrower prediction intervals are preferred

#Excercise 6

#Test the "small" model in exercise 5 against the "big"" model in
#exercise 1 at probability type I error ??=0.05 Which model is preferred?
fit1<- lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,prostate)
fit2 = lm(lpsa~lcavol+lweight+svi, data=prostate)
anova(fit1, fit2)
#Since the p-value for the F-stat is 0.2167 and is larger than the significance level 0.05, we accept the reduced model
#Thus, the smaller model is preferred because it is simpler and still valid.