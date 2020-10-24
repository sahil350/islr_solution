# imports

library(ISLR)
library(boot)
library(MASS)
library(glue)

# 5
# part a
glm.fit = glm(default~income+balance, data = Default, family = binomial)

# part b

validation = function(x){
  set.seed(x)
  train = sample(10000, 7000)
  glm.fit2 = glm(default~income+balance, data=Default, subset=train, family=binomial) 
  glm.proba = predict(glm.fit2, Default, type='response')[-train]
  glm.pred = rep('No', 3000)
  glm.pred[glm.proba>0.5]='Yes'
  mean(glm.pred != Default[-train])
}
# error = 4.14%
error = validation(1)
error

# part c
error.2 = validation(12)
error.3 = validation(21)
error.2
error.3

# As expected there is strong variation in error depending on the sample split

# part d
validation_student = function(x){
  set.seed(x)
  train = sample(10000, 7000)
  glm.fit2 = glm(default~income+balance+student, data=Default, subset=train, family=binomial) 
  glm.proba = predict(glm.fit2, Default, type='response')[-train]
  glm.pred = rep('No', 3000)
  glm.pred[glm.proba>0.5]='Yes'
  mean(glm.pred != Default[-train]) 
}

error.student = validation_student(1)
error.student

error.student.2 = validation_student(12)
error.student.2

error.student.3 = validation_student(21)
error.student.3

# error is lower for split with seed 1, 12, but higher with 21.

# 6
# part a
summary(glm.fit)

# std error income = 4.985e-06
# std error balance = 2.274e-04

# part b
boot.fn = function(dataset, index){
  coefficients(glm(default~income+balance, data=dataset, subset=index, family=binomial))
}

# part c
boot(Default, boot.fn, 1000)
# std error income = 4.784590e-06
# std error balance = 290665e-04

# part d
# Std errors are different from the summary function. This is because the errors from summary function assume that the
# logistic model is correct, more specifically the variance depends on the correctness of the model
# whereas std erro from the bootstrap approach are non parametric and thus more likely to be correct

# 7
# part a
glm.fit = glm(Direction~Lag1+Lag2, data=Weekly, family=binomial)

# part b
glm.fit2 = glm(Direction~Lag1+Lag2, data=Weekly[-1, ], family=binomial)

# part c
proba = predict(glm.fit2, Weekly[1, ], type='response')
prediction = ifelse(proba>0.5, 'Up', 'Down')
prediction == Weekly[1, 'Direction']
# prediction is 'Up', it is not correct

# part d
results = rep(NA, 1089)
for (i in (1:1089)){
  data = Weekly[-i, ]
  dir = Weekly[i, 'Direction']
  model = glm(Direction~Lag1+Lag2, data=data, family=binomial)
  proba = predict(model, Weekly[i, ], type='response')
  prediction = ifelse(proba>0.5, 'Up', 'Down')
  results[i] = ifelse(prediction==dir, 0, 1)
}

# part e
mean(results)
# 0.45

# we would report the test error to be 45%

# 8
# part a
set.seed(1)
X = rnorm(100)
Y = X - 2*X^2 + rnorm(100) 
# n = 100, p = 2

# part b
plot(X, Y)
# relationship between X, and Y appears to be quadratic

# part c
set.seed(1)
# 1. Y = Beta_0 + Beta_1 * X + eps
data = data.frame(Y, X, X^2, X^3, X^4)
model.1 = glm(Y~X, data = data)
error.1 = cv.glm(data, model.1)

# 2. Y = Beta_0 + Beta_1 * X + Beta_2 * X^2 + eps
model.2 = glm(Y~X+X.2, data = data)
error.2 = cv.glm(data, model.2)

# 3. Y = Beta_0 + Beta_1 * X + Beta_2 * X^2 + Beta_3*X^3 + eps
model.3 = glm(Y~X+X.2+X.3, data = data)
error.3 = cv.glm(data, model.3)

# 4. Y = Beta_0 + Beta_1 * X + Beta_2 * X^2 + Beta_3*X^3 + Beta_4*X^4 + eps
model.4 = glm(Y~X+X.2+X.3+X.4, data = data)
error.4 = cv.glm(data, model.4)

# part d
set.seed(2)
# 1. Y = Beta_0 + Beta_1 * X + eps
data = data.frame(Y, X, X^2, X^3, X^4)
model.1 = glm(Y~X, data = data)
error.1 = cv.glm(data, model.1)

# 2. Y = Beta_0 + Beta_1 * X + Beta_2 * X^2 + eps
model.2 = glm(Y~X+X.2, data = data)
error.2 = cv.glm(data, model.2)

# 3. Y = Beta_0 + Beta_1 * X + Beta_2 * X^2 + Beta_3*X^3 + eps
model.3 = glm(Y~X+X.2+X.3, data = data)
error.3 = cv.glm(data, model.3)

# 4. Y = Beta_0 + Beta_1 * X + Beta_2 * X^2 + Beta_3*X^3 + Beta_4*X^4 + eps
model.4 = glm(Y~X+X.2+X.3+X.4, data = data)
error.4 = cv.glm(data, model.4)

# Errors are same as before as expected because errors from LOOCV are not expected to vary

# part e
# minimum error is from model.2, which was expected as the original relationship is quadratic

# part f
model.1$coefficients
# coeff X = 0.24
model.2$coefficients
# coef of X = 0.9, X^2 = -1.87. These are close to the original relationship but not exact
model.3$coefficients
# coef of X = 0.96, X^2 = -1.85, X^3 = -0.025. These are also close
model.4$coefficients
# coef of X= 0.91, X^2 = -1.72, X^3 0.007, X^4=-0.038. Model3 and model2 are clearly better than model4
# Results are consistent with LOOCV error conclusion

# 9
# part a
mean(Boston$medv)
# 22.53

# part b
variance = var(Boston$medv)
stdev = variance^0.5
std.error = stdev/(length(Boston$medv)^0.5)
# 0.41

# part c
error.fn = function(data, index){
  mean(data[index])
}

boot(Boston$medv, error.fn, 1000)
# std.error = 0.4118375, it is slightly higher as compared with part b

# part d
CI = c(22.53281-2*0.4118375, 22.53281+2*0.4118375)
t.test(Boston$medv)
# approximately same

# part e
median(Boston$medv)
# 21.2

# part f
median.fn = function(data, index){
  median(data[index])
}

boot(Boston$medv, median.fn, 1000)
# std error 0.38

# part g
quantile(Boston$medv, 0.1)
# 12.75

# part h
mu.10.fn = function(data, index){
  quantile(data[index], 0.1)
}

boot(Boston$medv, mu.10.fn, 1000)
# std error 0.52