#Exercise 1(a)

set.seed(123)
n= 100
x <- 1:n/(n+1)
head(x)
tail(x)
beta0 <- 10
beta1 <- 2
expy <- beta0 + beta1*x
mu <- 0.0
sigma <- 2.7
err  <- rnorm(n, mu, sigma)
head(err)
tail(err)
yobs <-  expy + err
require(ggplot2)
p <- qplot(x, yobs) 
p <- p + geom_abline(intercept = beta0, slope = beta1)
p
cor(x, yobs)
my_data <- data.frame(x, yobs)
my_data

#Exercise 1(b)

sigma2 <- 0.7
err2  <- rnorm(n, mu, sigma2)
yobs2 <- expy + err2
p2 <- qplot(x, yobs2)
p2 <- p2 + geom_abline(intercept = beta0, slope = beta1)
p2

cor(x,yobs2)
my_data <- data.frame(x, yobs2)
my_data

#Exercise 1(c)

sigma3 <- 0.27
err3  <- rnorm(n, mu, sigma3)
yobs3 <- expy + err3
p3 <- qplot(x, yobs3)
p3 <- p3 + geom_abline(intercept = beta0, slope = beta1)
p3

cor(x,yobs3)
my_data <- data.frame(x, yobs3)
my_data




 
#Observation Exercise 1
# cor(x, yobs) =  0.2994951
# cor(x,yobs2) =  0.6884403
# cor(x,yobs3) =  0.9215917

# The 1st observation in exercise 1 is that as soon as we change the sigma the distriburion of the points also gets changed.
# Exercise 1(a)--> cor(x, yobs) =  0.2994951
# When sigma change is equal to 2.7 we see the points are scattered.
# Exercise 1(b)--> cor(x,yobs2) =  0.6884403
#output values are seen closer to the value line when sigma changes from 2.7 to 0.7.
# Exercise 1(c)--> cor(x,yobs3) =  0.9215917
#We see the points are more closer to the line when it changes further from 0.7 to 0.27.

## Thus as the sigma changes we see there is a change in correlation measure and Y gets  gets more and more closer to the expected line as sigma changes from 2.7 to 0.7 to 0.27 .
## Further as correlation gets closer to 1,the points are seen closer to the expected line.

#Exercise 2(d)

n <- 10:80
x=n
head(x)
tail(x)
beta0 <- 10
beta1 <- 2
expy <- beta0 + beta1*x
mu <- 0.0
sigma <-2.7
err  <- rnorm(x, mu, sigma)
head(err)
tail(err)
yobs4 <-  expy + err
require(ggplot2)
p4 <- qplot(x, yobs4, xlab = 'x4')
p4 <- p4 + geom_abline(intercept = beta0, slope = beta1)
p4

cor(x,yobs4)

my_data <- data.frame(x, yobs4)
my_data


#Observation Exercise 2
# cor(x, yobs) =  0.2994951
# cor(x,yobs2) =  0.6884403
# cor(x,yobs3) =  0.9215917
# cor(x,yobs4) =  0.9979845

# The 1st observation in exercise 1 is that as soon as we change the sigma the distriburion of the points also gets changed.
# Exercise 1(a)--> cor(x, yobs) =  0.2994951
# When sigma change is equal to 2.7 we see the points are scattered.
# Exercise 1(b)--> cor(x,yobs2) =  0.6884403
# output values are seen closer to the value line when sigma changes from 2.7 to 0.7.
# Exercise 1(c)--> cor(x,yobs3) =  0.9215917
# We see the points are more closer to the line when it changes further from 0.7 to 0.27.
# Exercise 1(c)--> cor(x,yobs4) =  0.9979845
# We see the points are more closer to the line when it changes further from 0.7 to 2.7 and as the input data range for x with minimum 10 and maximum 80.


## Thus as the sigma changes we see there is a change in correlation measure and Y gets  gets more and more closer to the expected line. 
## It gets even closer as we almost see points ploted on the expected line when the sigma changes to 2.7 again and there is a input data range for x with minimum 10 and maximum 80. 

