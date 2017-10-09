require("faraway")
head("uswages")



require("faraway")
head("uswages")
summary(uswages)
uswages$exper[uswages$exper <0] <-NA

# convert race, smsa, and pt to factor variables
uswages$race <- factor(uswages$race)
levels(uswages$race) <- c("White","Black")
uswages$smsa <- factor(uswages$smsa)
levels(uswages$smsa) <- c("No","Yes")
uswages$pt <- factor(uswages$pt)
levels(uswages$pt) <- c("No","Yes")

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


# Take care of NAs
uswages <- na.omit(uswages)

# Variable names
names(uswages)
#Question 1
# Run a model with region as predictor of wages. Show that the number of coefficients associated with region is 3.
g=lm(wage ~ region,data= uswages)
coef(g)
# We see that there are 3 coefficients namely regionmw(midwest region)=-48.027300, regionso(south region)=-56.902861, and regionwe(western region)=9.514236.

#Question 2
#Apply the aggregate(wage ~ region, data = uswages, mean) function in R to obtain the mean wages by region
#Show that the average wage in the northeast is b0.
#Show that the average wage in the midwest is b0+b1 dollars.
#Show that the average wage in the south is b0+b2 dollars.
#Show that the average wage in the west is b0+b3 dollars.

g=lm(wage ~ region,data= uswages)

coef(g)
aggregate(wage ~ region,data=uswages, mean)
#Average wage in northeast(ne) = 641.7178(b0)
#Average wage in midwest(mw) = b0 + b1 = 641.7178 - 48.027300 = 593.6905
#Average wage in south(so) = b0 + b2= 641.7178 - 56.902861 =584.8150
#Average wage in west(we)= b0+b3=641.7178 + 9.514236= 651.2320

#Question 3


#Compare the two models:
# Model 1: wage ~ region
# Model 2: wage ~ region + educ + exper
# Show that the F-Ratio is 152.397 with p-value 3.02510^{-62}.
# What is the conclusion - Model 1 or Model 2 is better?
# So does education and experience matter?

m1 = lm(wage ~ region, data= uswages)

m2 = lm(wage ~region + educ + exper , data= uswages)
summary(m2)
sse.sm = deviance(m1)
df.sm = df.residual(m1)


sse.bg = deviance(m2)
df.bg = df.residual(m2)

mse.prt = (sse.sm - sse.bg)/(df.sm - df.bg)
mse.bg =sse.bg/df.bg

f.ratio_model2 = mse.prt/mse.bg
f.ratio_model2
# The F- ratio equals 152.397

p.value = 1-pf(f.ratio_model2,df.sm - df.bg,df.bg)
p.value
p.value1=pf(f.ratio_model2,df.sm-df.bg,df.bg,lower.tail = FALSE)
p.value1
#the p.value1 3.02510^{-62} mathches the one in the question
# p - value is very less compared to 0.05 thus we reject the null hypothesis giving us Model 2 is better.

#Education and experience do matter



#Question 4
#Compare the two models:
#  Model 1: wage ~ educ + exper
#Model 2: wage ~ region + educ + exper
#Show that the F-ratio is 2.404 with p-value equal to 0.066.
#Using level of significance ??=0.05, what is the conclusion: Model 1 or Model 2 is better?
#So does education and experience determine wage regardless of the region of the United States you live in, or does region still matter?

m1 = lm(wage ~ educ + exper, data= uswages)


m2 = lm(wage ~region + educ + exper , data= uswages)
sse.sm = deviance(m1)
df.sm = df.residual(m1)


sse.bg = deviance(m2)
df.bg = df.residual(m2)

mse.prt = (sse.sm - sse.bg)/(df.sm - df.bg)
mse.bg =sse.bg/df.bg

f.ratio_model2 = mse.prt/mse.bg
f.ratio_model2

# The F- ratio of model 2 equals 2.404

p.value = 1-pf(f.ratio_model2,df.sm - df.bg,df.bg)
p.value
p.value1=pf(f.ratio_model2,df.sm-df.bg,df.bg,lower.tail = FALSE)
p.value1

#the p-value equals 0.066
#The F-Ratio 2.404 is big, & p-value is 0.066 which is greater than 0.05
#therfore based on this Model 1 is better. 

#education and experience determine wage regardless of the region of the United States you live in.

#Question 5
#Repeat exercise #4 using log(wage) for the outcome variable.

#Compare the two models:
#  Model 1: log(wage) ~ educ + exper
#Model 2: log(wage) ~ region + educ + exper
#Show that the F-ratio is 1.289 with p-value equal to 0.276.
#Using level of significance ??=0.05, what is the conclusion: Model 1 or Model 2 is better?
#So does education and experience determine wage regardless of the region of the United States you live in, or does region still matter?

m1 = lm(log(wage) ~ educ + exper, data= uswages)


m2 = lm(log(wage) ~region + educ + exper , data= uswages)
sse.sm = deviance(m1)
df.sm = df.residual(m1)


sse.bg = deviance(m2)
df.bg = df.residual(m2)

mse.prt = (sse.sm - sse.bg)/(df.sm - df.bg)
mse.bg =sse.bg/df.bg

f.ratio_model2 = mse.prt/mse.bg
f.ratio_model2
# The F- ratio of model 2 equals 1.289

p.value = 1-pf(f.ratio_model2,df.sm - df.bg,df.bg)
p.value
p.value1=pf(f.ratio_model2,df.sm-df.bg,df.bg,lower.tail = FALSE)
p.value1
#p-value is 0.276 which is greater than 0.05 based on this we therfore have  Model 1 is better. 

#education and experience determine wage regardless of the region of the United States you live in.