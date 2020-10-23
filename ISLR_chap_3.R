## load libraries
library(ISLR)
library(MASS)
library(Carseats)
library(glue)

## question 8
# (a) lm <- simple linear regression with mpg as response and horsepower as predictor
# load dataset
fix(Auto)
attach(Auto)

lm.fit = lm(mpg ~ horsepower)
summary(lm.fit)

# i. looks like there's a relationship between response and predictor variable
# ii. For one unit change in horsepower, mpg drops by ~0.16 units
# iii. the relationship is negative
# iv. 
predict (lm.fit ,data.frame(horsepower=c(98) ),
         interval ="prediction")

confint(lm.fit)

# b. plot
plot(horsepower, mpg)
abline(lm.fit, lwd=3, col='red')

# c. 
par(mfrow =c(2,2))
plot(lm.fit)

# residuals seem to indicate a non-linear relationship

##
# Question 9
##
# a.
pairs(Auto)

# b.
cor(Auto[sapply(Auto, is.numeric)])

# c
lm.fit9 = lm(mpg~.-name, data=Auto)
summary(lm.fit9)

# i. F-statistic is well above 1 and p-value is very close to 0 therefore there is evidence of relationship between
# response variable and predictor
# ii. displacement, weight, year, and origin
# iii. an increment of 1 in year increases mpg by 0.75 units

# d
plot(lm.fit9)

# there are outliers in the dataset as evidenced in residual plot, so does leverage point to high leverage points

# e
car::vif(lm.fit9)

# because collinearity selecting variables
# 1. horsepower
# 2. acceleration
# 3. year
# 4. origin

model1 = lm(mpg~horsepower+acceleration+year+origin)
summary(model1)

# F statistic 302.6 and p-value very close to 0
# R_sq 0.7577

# check for interaction effect
# plot
plot(model1)

# add interaction term between horsepower and acceleration
model2 = lm(mpg~horsepower*acceleration+year+origin, data=Auto)
summary(model2)
# r_sq improved to 0.8066, coefficients are also signicant as suggested by t-values
# p-value suggests interaction between acceleration and horsepower is significant

# f
# trying horsepower^2
model3 = lm(mpg~horsepower*acceleration+year+origin+ I(horsepower^2), data=Auto)
summary(model3)

# the interaction p value is high now
model4 = lm(mpg~horsepower+acceleration+year+origin+ I(horsepower^2), data=Auto)
summary(model4)

# looks good
plot(model4)

# 10
# a
model_cars = lm(Sales~Price+Urban+US, data=Carseats)
summary(model_cars)

# b. The p value is significant for Price and US. As US is a binary variable the sales increase by approximately 1.2
# in the US. Further, as expected the relationship is negative with Price, however the scale surprised me.

# c. Sales = Beta_0 + Beta_1*Price + Beta_2 * Urban + Beta_3 * US (model equation)
# d. Reject Null hypothesis for US and Price

# e.
model_cars_2 = lm(Sales ~ Price + US, data=Carseats)
summary(model_cars_2)

# f. The R_sq in both (a) and (e) are low 0.2393, doesn't suggest a good fit

# g.
confint(model_cars_2)

# h.
par(mfrow=c(2,2))
plot(model_cars_2)
plot(resid(model_cars_2), rstudent(model_cars_2), pch=23, bg='blue', cex=3)

# evidence of high leverage but not of outliers

# 11.
# a.
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)

simple_model = lm(y~x+0)
summary(simple_model)

# coeff 1.9939 
# t value 18.73
# p values <2e-16
# as p value is less than 0.05, we can reject the null hypothesis

# b
simple_model2 = lm(x~y+0)
summary(simple_model2)

# t value 18.73
# coeff 0.39111
# p value 2e-16
# t statistic same as before

# f
sm_3 = lm(y~x)
sm_4 = lm(x~y)

summary(sm_3)
summary(sm_4)

# 12
# b different

x = rnorm(100)
y = 1.1*x + rnorm(100)

m1 = lm(y~x+0)
m2 = lm(x~y+0)

coeff1 = summary(m1)$coefficients
coeff2 = summary(m2)$coefficients
val = glue("Coefficients are equal: {coeff1[1]==coeff2[1]}")
val

# c same
x = rnorm(100)
y = rnorm(100)
m1 = lm(y~x+0)
m2 = lm(x~y+0)

coeff1 = summary(m1)$coefficients
coeff2 = summary(m2)$coefficients
val = glue("Coefficients are : {round(coeff1[1], 2)}, {round(coeff2[1], 2)}")
val

# 13

set.seed(1)
X = rnorm(100)
eps = rnorm(100, sd=0.25)
Y = -1 + 0.5*X + eps
par(mfrow=c(1,1))
plot(X, Y)
lm.fit = lm(Y~X)
summary(lm.fit)

# B0_hat = -1.009, B1_hat = 0.49973 
# t statistic = 18.56 for B1_hat and p-value 2e-16
# indeed coefficients are close to original values and significant
abline(lm.fit, col='red', lty=2)
legend(-2,0, legend=c('points','fit'), col=c("black", "red"), lty=1:2)

lm.fit2 = lm(Y~X+I(X^2))
summary(lm.fit2)

# p value of X^2 is 0.164, therefore can't reject the null hypothesis
# no evidence quadratic term improves the fit

eps = rnorm(100, sd=0.01)
Y = -1 + 0.5*X + eps
par(mfrow=c(1,1))
plot(X, Y)
lm.fit3 = lm(Y~X)
summary(lm.fit3)

# B0_hat = -0.999, B1_hat = 0.500 
# t statistic = 430.1 for B1_hat and p-value 2e-16
# indeed coefficients are close to original values and significant
abline(lm.fit3, col='red', lty=2)
legend(-2,0, legend=c('points','fit'), col=c("black", "red"), lty=1:2)

eps = rnorm(100, sd=0.75)
Y = -1 + 0.5*X + eps
par(mfrow=c(1,1))
plot(X, Y)
lm.fit4 = lm(Y~X)
summary(lm.fit4)

# B0_hat = -0.9567, B1_hat = 0.4582 
# t statistic = 5.485 for B1_hat and p-value 3.23e-07
# indeed coefficients are close to original values and significant
abline(lm.fit4, col='red', lty=2)
legend(-2,0, legend=c('points','fit'), col=c("black", "red"), lty=1:2)

# confidence intervals
confint(lm.fit)
confint(lm.fit3)
confint(lm.fit4)

# noisier samples have wider confidence interval

# 14
set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)

cor(x1, x2)
plot(x1, x2)

lm.fit = lm(y~x1+x2)
summary(lm.fit)

# p value of B2_hat is 0.375 therefore we can not reject the null hypotheis
# B0_hat = 2.13, B1_hat = 1.44, B2_hat = 1.01 
# the values do not estimate the original function properly
# can reject B1 null hypothesis for p = 0.05
# can reject B0 null hypothesis for p = 0.05

lm.fit2 = lm(y~x1)
summary(lm.fit2)

# results far better than before
# Yes can reject null hypothesis for B0 and B1 at p = 0.05

lm.fit3 = lm(y~x2)
summary(lm.fit3)

# results are good

x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

lm.fit = lm(y~x1+x2)
summary(lm.fit)

# p value of B1_hat is 0.36458 therefore we can not reject the null hypotheis
# B0_hat = 2.226, B1_hat = 0.539, B2_hat = 2.515 
# the values do not estimate the original function properly
# can reject B2 null hypothesis for p = 0.05
# can reject B0 null hypothesis for p = 0.05

lm.fit2 = lm(y~x1)
summary(lm.fit2)

# results far better than before
# Yes can reject null hypothesis for B0 and B1 at p = 0.05

lm.fit3 = lm(y~x2)
summary(lm.fit3)

# the estimates don't look so good
par(mfrow=c(2,2))
plot(lm.fit)
plot(lm.fit2)
plot(lm.fit3)

# there is evidence of high leverage

# 15

# function to extract p-value
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# save all significant variables
significant = list()

# save all non-significant variables
non_significant = list()

# simple regression with all predictor variables
# extract p value
for (col in names(Boston)[-1]){
  x = c('crim', col)
  model = lm(Boston[, x])
  p = lmp(model)
  if (p > 0.05){
    non_significant[[col]] = p
  }
  else {
    significant[[col]] = p
  }
}

# only chas is non-significant

lm.fit = lm(crim~., data=Boston)
summary(lm.fit)

# can reject null hyposthesis for 
data.frame(summary(lm.fit)$coef[summary(lm.fit)$coef[,4] <= .05, 4])

# univariate coefficients
coeff.uni = list()
for (col in names(Boston)[-1]) {
  x = c('crim', col)
  coeff = coefficients(lm(Boston[, x]))[col]
  attributes(coeff) = NULL
  coeff.uni[[col]] = coeff
}

coeff.multi = coefficients(lm.fit)[-1]
par(mfrow = c(1,1))
plot(coeff.uni, coeff.multi)

df = data.frame(value=numeric())
for (col in names(Boston)[c(-1, -4)]){
  form = as.formula(glue('crim~{col}+I({col}^2)+I({col}^3)'))
  model = lm(form, data=Boston)
  model.summary = summary(model)
  frame = data.frame(model.summary$coef[model.summary$coef[,4] <= .05, 4][-1])
  colnames(frame)[1] = 'value'
  df = rbind(df, frame)
}

# Yes, there is evidence of non-linear relationship for multiple variables
