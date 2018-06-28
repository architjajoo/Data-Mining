rm(list=ls()); gc()
setwd("/Users/architjajoo/Desktop/d/CSUF classes/Semester 2/ISDS 574/HW2")

dat = read.csv('toyota_clean2.csv', stringsAsFactors=T, head=T)
View(dat)

# b). - Partition the data into training (60%) and validation (40%).
set.seed(1)
id.train = sample(1:nrow(dat), nrow(dat)*.6) 
id.test = setdiff(1:nrow(dat), id.train) 

obj = lm(Price ~ ., data = dat[id.train, ])
summary(obj)

# c). - For the outcome variable “Price”, fit a multiple linear regression with
# forward selection, regression-based kNN with k selected by the validation set, 
# and regression tree. Use all variables in these three methods and PCA analysis
# is not needed.

yhat = predict(obj, newdata = dat[id.test, ])
length(yhat)
length(id.test)
plot(dat[id.test, 'Price'], yhat, xlab='Actual y', ylab='Fitted y')

require(hydroGOF)
rmse(dat[id.test, 'Price'], yhat)

# Forward
library(MASS)
obj1 = step(obj, direction='forward')
summary(obj1)

# kNN
install.packages("FNN")
library('FNN')

knn.reg = function(train, test, y.train, y.test, k.max = 20) {
  pe = rep(NA, k.max)
  for (ii in 1:k.max) {
    y.hat = knn(train, test, y.train, k = ii, prob=F)
    pe[ii] = sum(y.hat != y.test)
  }
  list(k.optimal = which.min(pe), pe.min = min(pe))
}

knn.reg(dat[id.train,1:2], dat[id.test,1:2], dat[id.train,3], dat[id.test,3])

# regression tree
library(rpart)
rfit = rpart(Price ~ ., method="anova", data=cu.summary)
printcp(rfit) 
plotcp(rfit)  
summary(rfit)

par(mfrow=c(1,2)) 
rsq.rpart(rfit)

plot(rfit, uniform=T, main="Regression Tree for Price ")
text(rfit, use.n=T, all=T, cex=.8)

# a). Dichotomize the variable “Price” at its 75th percentile and call this new
# variable “HighPrice”. Please note that we will not replace the “Price”
# variable but create a new variable “HighPrice”.

par(mfrow=c(1,1))
yhat1 = predict(obj, newdata = dat.test, type='response')
hist(yhat1)
dat.train = dat[id.train,]
dat.test = dat[id.test,]

HighPrice = function(yhat1, cutoff=.75) {
  out = rep(0, length(yhat1))
  out[yhat1 > cutoff] = 1
  out
}

yhat.class = HighPrice(yhat1)

sum(yhat.class != dat.test$Price)/length(id.test)

#forward
min.model = glm(Price ~ 1, data = dat.train, family = 'gaussian')
max.model = glm(Price ~ ., data = dat.train, family = 'gaussian')
max.formula = formula(max.model)
HPobj = step(min.model, direction='forward', scope=max.formula)
summary(HPobj)

#kNN
require(class)
kNNobj = knn(dat[id.train,1:2], dat[id.test,1:2], dat[id.train,3], k = 3, prob=TRUE)
names(kNNobj)
class(kNNobj)

knn.bestKHP = function(train, test, y.train, y.test, k.max = 20) {
  pe = rep(NA, k.max)
  for (ii in 1:k.max) {
    y.hat = knn(train, test, y.train, k = ii, prob=F)
    pe[ii] = sum(y.hat != y.test)
  }
  list(k.optimal = which.min(pe), pe.min = min(pe))
}
knn.bestKHP(dat[id.train,1:2], dat[id.test,1:2], dat[id.train,3], dat[id.test,3])

# classification tree
fit = rpart(Price ~ ., method="class", data=dat, cp = 1e-2, minsplit=5)
printcp(fit) 
plotcp(fit)  
summary(fit)

plot(fit, uniform=T, main="Classification Tree for Toyota Corolla")
text(fit, use.n=T, all=TRUE, cex=.8)
which.min(fit$cptable[, "xerror"])

pfit = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
plot(pfit, uniform=T, main="Pruned Classification Tree for Toyota Corolla")
text(pfit, use.n=T, all=T, cex=.8)
names(fit)
fit$frame
fit$where
fit$cptable
fit$numresp
fit$y




