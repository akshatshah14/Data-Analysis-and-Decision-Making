

# load data
require(faraway)
require(car)
data(fpe)


species <- gala$Species
endemics <- gala$Endemics
area <- gala$Area
elevation <- gala$Elevation
nearest <- gala$Nearest
scruz <- gala$Scruz
adjacent <- gala$Adjacent

poisson_gala <- glm(
  species 
  ~ endemics 
  + area
  + elevation
  + nearest
  + scruz
  + adjacent,
  family = poisson(link = "log"))

summary(poisson_gala)


g <- lm(species ~ endemics + area + elevation + nearest + scruz + adjacent, data = gala)
lambda <- powerTransform(g)
lam <- lambda$lambda
boxcox_gala <- lm(species^lam ~ endemics + area + elevation + nearest + scruz + adjacent, data = gala)

summary(boxcox_gala)
compareCoefs(poisson_gala, boxcox_gala, se = FALSE)


ei <- fpe$EI
a <- fpe$A
b <- fpe$B
c <- fpe$C
d <- fpe$D
e <- fpe$E
f <- fpe$F
g <- fpe$G
h <- fpe$H
j <- fpe$J
k <- fpe$K
a2 <- fpe$A2
b2 <- fpe$B2
n <- fpe$N

poisson_fpe <- glm(
  ei 
  ~ a 
  + b
  + c
  + d
  + e
  + f
  + g
  + h
  + j
  + k
  + a2
  + b2
  + n,
  family = poisson(link = "log"))

summary(poisson_fpe)

WLS_fpe1 <- lm(ei ~., fpe, weight = 1 / n)
WLS_fpe2 <- lm(ei ~., fpe, weight = 1 / (n^2))

summary(WLS_fpe1)
summary(WLS_fpe2)
compareCoefs(poisson_fpe, WLS_fpe1, WLS_fpe2)
