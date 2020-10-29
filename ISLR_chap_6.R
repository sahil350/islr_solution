# imports
library(ISLR)
library(MASS)
library(glue)
library(leaps)
library(glmnet)
library(pls)

# 8
# part a
X = rnorm(100)
eps = rnorm(100)

# part b
Y = -10 + 2 * X + 5 * X^2 + 0.01 * X^3 + eps

# part c

data = data.frame(Y, X)
for (i in (2:10)){
  data = data.frame(data, X^i)
}

regfit.full = regsubsets(Y~., data)
reg.summary = summary(regfit.full)

par(mfrow = c(2,2))
plot(reg.summary$adjr2, xlab='Number of variables', ylab='Adjusted R^2', type='l')
points(2, reg.summary$adjr2[2], col='red', cex=2, pch=20)

plot(reg.summary$cp, xlab='Number of variables', ylab='Cp', type='l')
points(2, reg.summary$cp[2], col='red', cex=2, pch=20)

plot(reg.summary$bic, xlab='Number of variables', ylab='BIC', type='l')
points(2, reg.summary$bic[2], col='red', cex=2, pch=20)

# selected X and X^2

# part d
regfit.for = regsubsets(Y~., data=data, method = 'forward')
reg.summary = summary(regfit.for)

plot.subsets = function(reg.summary){
  par(mfrow = c(2,2))
  plot(reg.summary$adjr2, xlab='Number of variables', ylab='Adjusted R^2', type='l')
  x = which.max(reg.summary$adjr2)
  points(x, reg.summary$adjr2[x], col='red', cex=2, pch=20)
  x = which.min(reg.summary$cp)
  plot(reg.summary$cp, xlab='Number of variables', ylab='Cp', type='l')
  points(x, reg.summary$cp[x], col='red', cex=2, pch=20)
  x = which.min(reg.summary$bic)
  plot(reg.summary$bic, xlab='Number of variables', ylab='BIC', type='l')
  points(x, reg.summary$bic[x], col='red', cex=2, pch=20)
  
}

plot.subsets(reg.summary)

regfit.back = regsubsets(Y~., data=data, method='backward')
reg.summary = summary(regfit.back)
plot.subsets(reg.summary)

# part e
set.seed(1)
X = model.matrix(Y~., data = data)[, -1]
y = data$Y
lasso.out = cv.glmnet(X, y, alpha=1)
plot(lasso.out)
lasso.out$lambda.min
# 0.0722
lasso.mod = glmnet(X, y, alpha=1, lambda = lasso.out$lambda.min)
summary(lasso.mod)
coef(lasso.mod)
# The results obtained are really close to the original coefficients of f
# intercept = -10.026
# coeff(X) = 2.109
# coeff(X^2) = 4.93
# coeff(X^3) = 0
# coeff(X^4) = 0.012

# part f
Y = -10 + 23*data$X.i.6 + eps
data$Y = Y

regfit.full = regsubsets(Y~., data)
reg.summary = summary(regfit.full)
plot.subsets(reg.summary)

# lasso
x = model.matrix(Y~., data)[, -1]
y = data$Y
lasso.out = cv.glmnet(x, y, alpha=1)
bestlam = lasso.out$lambda.min
lasso.mod = glmnet(x, y, alpha=1, lambda = bestlam)
coef(lasso.mod)
# best subset gave better results here

# 9
# part a
set.seed(1)
train = sample(1:nrow(College), as.integer(0.7*round(nrow(College))))
test = (-train)

# part b
lm.fit = lm(Apps~., data = College, subset = train)
y_hat = predict(lm.fit, College[test, ])

error = mean((College[test, 'Apps'] - y_hat)^2)
error

# part c
X = model.matrix(Apps~., data = College)[, -1]
y = College$Apps
ridge.out = cv.glmnet(X[train, ], y[train], alpha=0)
bestlam = ridge.out$lambda.min
ridge.fit = glmnet(X[train, ], y[train], alpha = 0, lambda = bestlam)
ridge.pred = predict(ridge.fit, X[test, ], exact = T)
ridge.error = mean((y[test] - ridge.pred)^2)
ridge.error

# part d
lasso.out = cv.glmnet(X[train, ], y[train], aplha=1)
bestlam = lasso.out$lambda.min
lasso.fit = glmnet(X[train, ], y[train], alpha = 1, lambda = bestlam)
lasso.pred = predict(lasso.fit, X[test, ], exact = T)
lasso.error = mean((y[test]-lasso.pred)^2)
lasso.error

# part e
set.seed(1)
pcr.out = pcr(Apps~., data=College, subset=train, scale=T, validation='CV')
summary(pcr.out)
validationplot(pcr.out, val.type="MSEP")
pcr.fit = pcr(Apps~., data=College, subset=train, scale=T, ncom=17)
pcr.pred = predict(pcr.fit, X[test, ], ncomp = 17)
pcr.error = mean((y[test]-pcr.pred)^2)
pcr.error

# part f
pls.out = plsr(Apps~., data=College, subset=train, scale=T, validation='CV')
summary(pls.out)
validationplot(pls.out, val.type = "MSEP")
pls.fit = plsr(Apps~., data=College, subset=train, scale=T, ncomp=10)
pls.pred = predict(pls.fit, X[test, ])
pls.error = mean((y[test]-pls.pred)^2)
pls.error

# part g
all.error = c(error, ridge.error, lasso.error, pcr.error, pls.error)
all.labels = c('Least Sq', 'Ridge reg', 'Lasso reg', 'PCR', 'PLSR')
names(all.error) = all.labels
barplot(all.error)

# Ridge regression performed the best

# 10
# part a
set.seed(1)

# Xs
data = data.frame(rep(NA, 1000))
for (i in (1:20)) {
  data = data.frame(x = rnorm(1000), data)
}

data = data[, -21]

# Betas
beta = rnorm(20)
beta[sample(1:20, 6)] = 0
names(beta) = names(data)[-1]
# rename Xs
res = 'x.1'
for (i in 2:20){
  res = c(res, glue('x.{i}'))
}
names(data) = res

# calculate Y
data$Y = rowSums(mapply(`*`, data, beta)) + rnorm(1000)

# reorder columns in data frame Y first
data = data[,c('Y', setdiff(names(data), 'Y'))]

# part b
train = sample(1:1000, 900)
test = (-train)

# part c
reg.full = regsubsets(Y~., data[train, ], nvmax = 20)
reg.summary = summary(reg.full)

predict.regsubsets =function (object ,newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix (form ,newdata )
  coefi =coef(object ,id=id)
  xvars =names (coefi )
  mat[,xvars ]%*% coefi
}

plot.errors = function(sub, title='Training MSE'){
  errors = rep(NA, 20)
  for (i in (1:20)){
    y_hat = predict(reg.full, data[sub, ], i)
    y = data[sub, 'Y']
    errors[i] = mean((y - y_hat)^2)
  }
  # plot
  plot(errors, type='l', main = title)
  min.error = which.min(errors)
  points(min.error, errors[min.error], col='red', cex=2, pch=20)
}

par(mfrow=c(2, 2))
plot.errors(train)

# part d
plot.errors(test, 'Testing MSE')

# part e
# min test mse occurs for model with 14 features as expected

# part f
best.coef = coef(reg.full, 14)
beta
# the model is close to the original
errors = rep(NA, 20)
for (i in (1:20)){
  coefs = coef(reg.full, i)[-1]
  error.helper = 0
  for (n in names(beta)){
    err = ifelse(n %in% names(coefs), (coefs[n] - beta[n])^2, beta[n]^2)
    error.helper = error.helper + err 
  }
  errors[i] = (sum(error.helper))^(0.5)
}
min.error.index = which.min(errors)
plot(errors, main='Beta Errors', type='l')
points(min.error.index, errors[min.error.index], col='red', cex=2, pch=20)

# both have minimum at same number of features

# 11
# part a
set.seed(1)
train = sample(1:nrow(Boston), as.integer(0.75*nrow(Boston)))
test = (-train)
reg.full = regsubsets(crim~., Boston[train, ], nvmax = length(names(Boston))-1)

plot.errors = function(sub, data, target='Y', title='Training MSE', numf = 20 ){
  errors = rep(NA, numf)
  for (i in (1:numf)){
    y_hat = predict(reg.full, data[sub, ], i)
    y = data[sub, target]
    errors[i] = mean((y - y_hat)^2)
  }
  # plot
  plot(errors, type='l', main = title)
  min.error = which.min(errors)
  points(min.error, errors[min.error], col='red', cex=2, pch=20)
  return(min.error)
}
numf = length(names(Boston))-1
par(mfrow=c(1,1))
min.error = plot.errors(test, Boston, 'crim', 'Boston Test MSE', numf)
# all 14 features together give min MSE
reg.summary = summary(reg.full)
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

# training on all 14 vars
lm.fit = lm(crim~., Boston, subset = train)
lm.pred = predict(lm.fit, Boston[test, ])
lm.error = mean((y[test] - lm.pred)^2)
# lasso
X = model.matrix(crim~., data = Boston)[, -1]
y = Boston$crim
lasso.out = cv.glmnet(X[train, ], y[train], alpha=1)
bestlam = lasso.out$lambda.min
lasso.fit = glmnet(X[train, ], y[train], alpha = 1, lambda = bestlam)
lasso.pred = predict(lasso.fit, X[test, ], exact = T)
lasso.error = mean((y[test] - lasso.pred)^2)

# ridge
ridge.out = cv.glmnet(X[train, ], y[train], alpha=0)
bestlam = ridge.out$lambda.min
ridge.fit = glmnet(X[train, ], y[train], alpha=0, lambda = bestlam)
ridge.pred = predict(ridge.fit, X[test, ], exact = T)
ridge.error = mean((y[test]-ridge.pred)^2)

# pcr
pcr.out = pcr(crim~., data=Boston[train, ], scale=T, validation='CV')
summary(pcr.out)
validationplot(pcr.out, val.type = "MSEP")
pcr.fit = pcr(crim~., data=Boston[train, ], scale=T, ncomp=14)
pcr.pred = predict(pcr.fit, X[test, ])
pcr.error = mean((y[test]-pcr.pred)^2)
# part b
all.errors = c(lm.error, lasso.error, ridge.error, pcr.error)
names(all.errors) = c('Best Subset', 'Lasso', 'Ridge', 'PCR')

barplot(all.errors)

# all test MSE are roughly same, best subset gave the best performance

# part c
# in terms of performance best subset is better than all others, but it uses all the features. An observation is that
# the best subset is only marginally better than Lasso (which is not using all the features), therefore we can reduce
# the model complexity by choosing Lasso.


