

#LabExercise_2
library("faraway")
data(uswages)
head(uswages)
summary(uswages)
#Manipulate data. We see that experience has negative values.
uswages$exper[uswages$exper < 0] = NA

#Convert race, smsa, and pt to factor variables
uswages$race = factor(uswages$race)
levels(uswages$race) = c("White","Black")
uswages$smsa = factor(uswages$smsa)
levels(uswages$smsa) = c("No","Yes")
uswages$pt = factor(uswages$pt)
levels(uswages$pt) = c("No","Yes")

#Create region, a factor variable based on the four regions ne, mw, so, we
uswages = data.frame(uswages,
                     region =
                       1*uswages$ne +
                       2*uswages$mw +
                       3*uswages$so +
                       4*uswages$we)
uswages$region = factor(uswages$region)
levels(uswages$region) = c("ne","mw","so","we")

#Take care of NAs
uswages <- na.omit(uswages)

#Column names
names(uswages)
#Exercise 1
g = lm(log(wage) ~ educ + exper + race + smsa + pt + region, data = uswages)
confint(g, level = 0.95)
#Exercise 2
g1 = lm(log(wage) ~ educ + exper + race + smsa + pt, data = uswages)
confint(g1)
tab1 = anova(g1,g)
tab1
#Exercise 3
g.bg = lm(log(wage) ~ educ + exper + race + smsa + pt + region, data = uswages)
g.sm = lm(log(wage) ~ educ + exper + race + smsa + pt, data = uswages)
sse.sm = deviance(g.sm)
df.sm = df.residual(g.sm)
sse.bg = deviance(g.bg)
df.bg = df.residual(g.bg)
mse.prt = (sse.sm-sse.bg)/(df.sm-df.bg)
mse.bg = sse.bg/df.bg
f.ratio = mse.prt/mse.bg
f.ratio
p.value = pf(f.ratio, df.sm-df.bg, df.bg, lower.tail=FALSE)
p.value
#Analysis:
#The P-value is 0.634 which is greater than 0.05. 
#Therefore, model 2 is better.


#Exercise 4
install.packages("ellipse", repos = "http://cran.us.r-project.org", type = "source")
library(ellipse)
plot(ellipse(g1, c("educ", "exper")), type = "l", main = "Joint Confidence Region")
points(0,0)
points(coef(g1)["educ"], coef(g)["exper"])


#Exercise 5
g2 <- lm(log(wage) ~ race + smsa + pt, data = uswages)
plot(ellipse(g1, c("educ", "exper")), type = "l", main = "Joint Confidence Region")
points(0,0)
points(coef(g)["educ"], coef(g)["exper"], pch=18)
abline(v=confint(g)["educ",], lty=2)
abline(h=confint(g)["exper",], lty=2)


compareg2g1 <- anova(g2, g1)
compareg2g1
#Analysis:
#If the p-value is less than or equal to the alpha (p < .05), then we reject the null hypothesis, and we say the result is statistically significant. 
#If the p-value is greater than alpha (p > .05), then we fail to reject the null hypothesis, and we say that the result is statistically nonsignificant (n.s.).
#The F-Ratio 238.9 is big and since the p-value 2.2e-16 is much less than 0.05, we reject the null hypothesis H0 : ??educ = ??exper = 0.


#Exercise 6
g1 = lm(log(wage) ~ educ + exper + race + smsa + pt, data = uswages)
x0 = data.frame(educ = 12, exper = 5, race = "White", smsa = "Yes", pt = "No", stringsAsFactors = FALSE)
predict(g1, x0, level = 0.95, interval = "confidence")
#Exercise 7
x0 <- rbind(x0, data.frame(educ = 12, exper = 5, race = "Black", smsa = "Yes", pt = "No"))
predict(g1, x0, level = 0.95, interval = "confidence")
33