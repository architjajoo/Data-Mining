rm(list=ls())
gc()

setwd("/Users/architjajoo/Desktop/d/CSUF classes/Semester 2/ISDS 574/HW 1")

dat = read.csv("toyota_2.csv", head=T, stringsAsFactors = F, na.strings = '')
View(dat)
dim(dat)
colnames(dat)
head(dat)

summary(dat)

cor(dat[, c(8:30)])
prcomp(dat[, c(8:30)], scale=T) 
obj = prcomp(dat[, c(8:30)], scale=T) 
class(obj) 
names(obj)

obj$sdev
obj$rotation
obj$center
obj$scale
obj$x

summary(obj)
#to capture atleast 80% of total variance we get 13 Principal Components.
