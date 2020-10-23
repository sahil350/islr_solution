# imports
library(ISLR)
library(MASS)
library(class)
library(glue)
# 10
# part 1
summary(Weekly)
pairs(Weekly)

# part b
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly, family = binomial)
summary(glm.fit)
# at p-value 0.05 Lag2 is a significant variable

# part c
glm.proba = predict(glm.fit, type='response')
glm.pred = rep('Down', 1250)
glm.pred[glm.proba > 0.5] = 'Up'

# confusion matrix
table(glm.pred, Direction)

# accuracy 52.56%
# type 1 (false positives) error 4.08%
# type 2 (false negatives) error 43.36%

# part d
train = (Weekly$Year <= 2008)
glm.fit2 = glm(Direction~Lag2, data=Weekly[train, ], family = binomial)

# test confusion matrix
X.test = Weekly[!train, ]
glm.proba2 = predict(glm.fit2, X.test, type='response')
glm.pred2 = rep('Down', length(X.test$Direction))
glm.pred2[glm.proba2>0.5] = 'Up'
table(glm.pred2, X.test$Direction)
# accuracy
mean(glm.pred2==X.test$Direction)

# part e
lda.fit = lda(Direction~Lag2, data=Weekly[train, ])
lda.pred = predict(lda.fit, X.test)
lda.class = lda.pred$class
table(lda.class, X.test$Direction)
mean(lda.class==X.test$Direction)

# part f
qda.fit = qda(Direction~Lag2, data=Weekly[train, ])
qda.pred = predict(qda.fit, X.test)
qda.class = qda.pred$class
table(qda.class, X.test$Direction)
mean(qda.class==X.test$Direction)

# part g
X.train = cbind(Weekly[train, ]$Lag2)
X.test = cbind(Weekly[!train, ]$Lag2)
Y.train = Weekly[train, ]$Direction
Y.test = Weekly[!train, ]$Direction

set.seed(1)
knn.pred = knn(X.train, X.test, Y.train, k=1)
table(knn.pred, Y.test)
mean(knn.pred == Y.test)

# part h
# best logistic and LDA

# part i
knn.pred5 = knn(X.train, X.test, Y.train, k=5)
table(knn.pred5, Y.test)
mean(knn.pred5 == Y.test)

knn_expt = function(k=5){
  knn.expt = knn(X.train, X.test, Y.train, k=k)
  print(table(knn.expt, Y.test))
  mean(knn.expt==Y.test)
}

knn_expt(10)
knn_expt(15)
knn_expt(20)
knn_expt(25)

# k = 20 is the best wit 56.7% accuracy

# improve lda with non linear features
X.test = Weekly[!train, ]
lda.fit2 = lda(Direction~Lag2+I(Lag2^2), data=Weekly[train, ])
lda.pred2 = predict(lda.fit2, X.test)
lda.class2 = lda.pred2$class
table(lda.class2, X.test$Direction)
mean(lda.class2==X.test$Direction)

# acuracy is going down

# experimenting with logistic regression
glm.fit3 = glm(Direction~Lag2+I(Lag2^2), data = Weekly[train, ], family = binomial)
glm.proba3 = predict(glm.fit3, X.test, type='response')
glm.pred3 = rep('Down', length(X.test$Direction))
glm.pred3[glm.proba3>0.5] = 'Up'
table(glm.pred3, X.test$Direction)
# accuracy
mean(glm.pred3==X.test$Direction)



# 11
# part a
Auto$mpg01 = ifelse(Auto$mpg > median(Auto$mpg), 1, 0)

# part b
pairs(Auto)
boxplot(displacement~mpg01, data=Auto)
boxplot(weight~mpg01, data=Auto)
boxplot(horsepower~mpg01, data=Auto)
boxplot(acceleration~mpg01, data=Auto)

# part c
smp_size = floor(0.75 * nrow(Auto))
set.seed(1)
train_ind = sample(seq_len(nrow(Auto)), size = smp_size)
train = Auto[train_ind, ]
test = Auto[-train_ind, ]


# part d
# using acceleration, displacement, weight, and horsepower
lda.fit = lda(mpg01~acceleration+displacement+weight+horsepower, data = Auto)
lda.pred = predict(lda.fit, test)
lda.class = lda.pred$class
mean(lda.class!=test$mpg01)   ## test error = 0.07

# part e
qda.fit = qda(mpg01~acceleration+displacement+weight+horsepower, data = Auto)
qda.pred = predict(qda.fit, test)
qda.class = qda.pred$class
mean(qda.class!=test$mpg01)   ## test error = 0.08

# part f
# logistic regression
glm.fit = glm(mpg01~acceleration+displacement+weight+horsepower, data = Auto, family = binomial)
glm.proba = predict(glm.fit, test, type='response')
glm.pred = rep(0, length(test$mpg01))
glm.pred[glm.proba > 0.5] = 1
mean(glm.pred != test$mpg01) # 0.07

# part g
knn_model = function(k=1){
  X.train = train[c("acceleration", "displacement", "weight", "horsepower")]
  X.test = test[c("acceleration", "displacement", "weight", "horsepower")]
  Y.train = train$mpg01
  Y.test = test$mpg01
  pred = knn(X.train, X.test, Y.train, k=k)
  print(table(pred, Y.test))
  mean(pred==Y.test)
}

knn_model() # k = 1
knn_model(5) # k = 5
knn_model(10) # k = 10

# plot to visualize best k
accuracy = list()
for (k in (1:25)){
  score = knn_model(k)
  accuracy = c(accuracy, score)
}
plot(unlist(accuracy), type='l', xlab="K", ylab="Accuracy", main="Accuracy vs K")

# taking k as 5
knn_model(5)
## accuracy 93.88%

# 12

# part a
Power = function(){
  print(2^3)
}

Power()

# part b
Power2 = function(x, a){
  print(x^a)
}

Power2(3,8)

# part c
Power2(10, 3)
Power2(8, 17)
Power2(131, 3)

# part d
Power3 = function(x, a){
  return (x^a)
}

# part e
result = list()
for (i in (1:10)){
  result = c(result, Power3(i, 2))
}

plot(unlist(result), type='l', xlab='x', ylab='x^2', log='x', main='f(x)=x^2')

# part f
PlotPower =function(iter, a){
  result = list()
  for (i in iter){
    result = c(result, Power3(i, a))
  }
  plot(unlist(result), type='l', xlab='x', ylab=glue('x^{a}'), log='x', main=glue('f(x)=x^{a}'))
}

PlotPower(1:10, 3)
PlotPower(1:10, 6)

# 13
crim01 = ifelse(Boston$crim > median(Boston$crim), 1, 0)
Boston = data.frame(Boston, crim01)

# train test split
smp_size = floor(0.75 * nrow(Boston))
set.seed(1)
train_ind = sample(seq_len(nrow(Boston)), size = smp_size)
train = Boston[train_ind, ]
test = Boston[-train_ind, ]


## logistic regression
glm.fit = glm(crim01~.-crim, data=train, family = binomial)
summary(glm.fit)

df = data.frame(value=numeric())
for (col in names(Boston)[c(-1, -4, -15)]){
  form = as.formula(glue('crim01~{col}+I({col}^2)+I({col}^3)'))
  model = glm(form, data=train, family=binomial)
  model.summary = summary(model)
  frame = data.frame(model.summary$coef[model.summary$coef[,4] <= .05, 4][-1])
  colnames(frame)[1] = 'value'
  df = rbind(df, frame)
}

# using features I(indus^2), I(indus^3), nox, I(nox^2), I(nox^3), rad, I(rad^2), I(rad^3), I(lstat^3), medv
glm.fit2 = glm(crim01~I(indus^2)+I(indus^3)+nox+I(nox^2)+I(nox^3)+rad+I(rad^2)+I(rad^3)+I(lstat^3)+medv,
               data=Boston,family=binomial)
summary(glm.fit2)

# now using I(indus^2), I(indus^3), rad, I(rad^2), I(rad^3)
glm.fit3 = glm(crim01~I(indus^2)+I(indus^3)+rad+I(rad^2)+I(rad^3), data=train, family=binomial)
summary(glm.fit3)
glm.proba = predict(glm.fit3, test, type='response')
glm.pred = rep(0, length(test$crim01))
glm.pred[glm.proba>0.5]=1
table(glm.pred, test$crim01)
mean(glm.pred==test$crim01)
# accuracy 83.46%

# trying lda
lda.fit = lda(crim01~I(indus^2)+I(indus^3)+rad+I(rad^2)+I(rad^3), data=train)
lda.pred = predict(lda.fit, test)
lda.class = lda.pred$class
table(lda.class, test$crim01)
mean(lda.class==test$crim01)
# accuracy 78.74%

# now trying qda
qda.fit = qda(crim01~I(indus^2)+I(indus^3)+rad+I(rad^2)+I(rad^3), data=train)
qda.pred = predict(qda.fit, test)
qda.class = qda.pred$class
table(qda.class, test$crim01)
mean(qda.class==test$crim01)
# accuracy 78.74%

# trying knn
knn_model = function(k=1){
  X.train = train[c("zn", "nox", "dis", "rad", "ptratio", "medv")]
  X.test = test[c("zn", "nox", "dis", "rad", "ptratio", "medv")]
  Y.train = train$crim01
  Y.test = test$crim01
  pred = knn(X.train, X.test, Y.train, k=k)
  print(table(pred, Y.test))
  mean(pred==Y.test)
}

knn_model() # k = 1
knn_model(5) # k = 5
knn_model(10) # k = 10

# plot to visualize best k
accuracy = list()
for (k in (1:25)){
  score = knn_model(k)
  accuracy = c(accuracy, score)
}
plot(unlist(accuracy), type='l', xlab="K", ylab="Accuracy", main="Accuracy vs K")

# selecting k=1
knn_model()
# accuracy 94.49%