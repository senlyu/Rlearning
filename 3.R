library(vcd)
library(car)
library(MASS)

View(whiteside)
hist(whiteside$Gas)
plot(density(whiteside$Gas))

polygon(density(whiteside$Gas),col="green")
View(Salaries)

# how to use symbols
symbols(Salaries$yrs.service,Salaries$salary,
        circles = Salaries$yrs.since.phd,inches = 0.1,bg = "red")
View(UScereal)

# change the table to a matrix, and the symbols could deal with matrix
y = as.matrix(UScereal[,3:4])
symbols(UScereal$sugars,UScereal$calories,
        rectangles = y1,inches = 0.4, bg = "red")

# random function
x1 = rnorm(10000)
y1 = rnorm(10000)
plot(x1,y1)

# better for density observation
smoothScatter(x1,y1)

# for uniform distribution
x2 = runif(10000)
y2 = runif(10000)
smoothScatter(x2,y2)

# could use to see where the points are occur the most
View(Boston)
sunflowerplot(Boston$ptratio,Boston$tax)


install.packages("rgl")
library(rgl)
# just fun, not very useful
plot3d(Boston$crim,Boston$indus,Boston$age,col = "red", size = 4)

#
install.packages("plotrix")
library(plotrix)
l1 = c("US","UK","G","F")
v1 = c(43,124,134,180)
pie(v1,l1,col = rainbow(4))

fan.plot(x = v1, labels = l1, col = rainbow(4))

install.packages("vioplot")
library(vioplot)
vioplot(whiteside$Gas)

install.packages("corrgram")
library(corrgram)
cor(Boston)
corrgram(Boston,order = T,upper.panel = panel.pie)


# programming functions

Numer<- read.csv("K:\\OneDrive\\Git\\Rlearning\\NumericPred.csv")
View(Numer)
colnames(Numer)=c("target","model1","model2")
a = Numer$target
m = Numer$model1

metrics = c(MAD=0,MSE=0,MAPE=0,MPSE=0,R2=0,TMAD=0)
metrics["MAD"] = mean(abs(a-m))
metrics["MSE"] = mean((a-m)^2)
metrics["MAPE"] = mean(abs((a-m)/a))
metrics["MPSE"] = mean(((a-m)/a)^2)
metrics["TMAD"] = mean(abs(a-m),trim = 0.05)
# TODO: find a function to get only 95% good TMAD
tmad = abs(a-m)
metrics["TMAD"] = mean(tmad[tmad<quantile(abs(a-m),probs=0.95)])
metrics["R2"] = 1-sum((a-m)^2)/sum((a-mean(a))^2)

metrics

# function
f1 = function(a,m)
{
  metrics = c(MAD=0,MSE=0,MAPE=0,MPSE=0,R2=0,TMAD=0)
  metrics["MAD"] = mean(abs(a-m))
  metrics["MSE"] = mean((a-m)^2)
  metrics["MAPE"] = mean(abs((a-m)/a))
  metrics["MPSE"] = mean(((a-m)/a)^2)
  #metrics["TMAD"] = mean(abs(a-m),trim = 0.05)
  # TODO: find a function to get only 95% good TMAD
  tmad = abs(a-m)
  metrics["TMAD"] = mean(tmad[tmad<quantile(abs(a-m),probs=0.95)])
  metrics["R2"] = 1-sum((a-m)^2)/sum((a-mean(a))^2)
  return(metrics)
}

asd = f1(Numer$target,Numer$model2)
asd

# store
getwd()
setwd("K:\\OneDrive\\Git\\Rlearning")
dump('f1',file = 'myfunctions.R')

# read the function
source(file.choose())
source("K:\\OneDrive\\Git\\Rlearning\\myfunctions.R")

args(f1)


