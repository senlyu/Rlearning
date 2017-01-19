#1
log(769)
6^5
#get 10 random numbers
rnorm(10)
help("rnorm")
#get the working dirctory
getwd()
#change the working dirctory
setwd("K:/OneDrive - University of Connecticut/Git/Rlearning")
getwd()
#show the librarys
library()
#case sensetive
library(MASS)
#can see the package details
library(help=MASS)
#want to gey some information in the package dataset
help("whiteside")

# basic data structures in R
# 1 Vectors(must be same type)
# 2 Matrices(array,must be same)
# 3 Dataframes(data table)
# 4 Lists(dont be identity)
# 5 Factors()

# create a voctor(60,72,57,90,95,72) called weight
weight=c(60,72,57,90,95,72)
height=c(1.75,1.8,1.65,1.9,1.74,1.91)
# 
mean(weight)
sd(height)
a=median(weight)
quantile(height,probs = 0.75)
quantile(height,probs=c(0.3,0.5))

length(height)
range(height)
t.test(weight,mu=80)
