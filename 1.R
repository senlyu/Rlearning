#class 1
#
#
#
#
#
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
mean(height)
sd(height)
a=median(weight)
quantile(height,probs = 0.75)
quantile(height,probs=c(0.3,0.5))

length(height)
range(height)
t.test(weight,mu=80)


bmi=weight/height^2
bmi
# graph
plot(height,weight,pch=3,
     main="first graph",col="salmon4",ylim=c(0,100),xlim = c(1,3))
colors()
#voctor slice
weight=c(weight,86)
height=c(height,weight[1])
height=height[1:6]
height=c(height,NA)
weight[5]
weight[c(2,6)]
weight[1:4]
weight[weight>73]
height[height>1.7&is.na(height)==F]
# this is useful
f=gender=="F"



#mean args
mean(height,na.rm = T)
# voctor diff date types
gender=c("M","F","M","F","F","M","F")
names(gender)=c("1","2","4","3","5","7","9")
# because gender is caterary data, so need to change to factor
plot(factor(gender),weight)

# seqence
x=seq(1,100,by=7)
# repeat
y=rep(7,100)
z=rep(c("A","B","C"),100)
zz=rep(c("A","B"),c(4,5))

#dataframe
ghw=data.frame(gender,height,weight)

ghw$height
plot(ghw$height,ghw$weight)
#can only read
edit(ghw)
#can read and write
fix(ghw)
ghw$gender[4]="F"
#open new window
View(ghw)

ghw[c(2,4,5),]
ghw[ghw$gender=="M",]

plot(whiteside$Insul,whiteside$Gas)
# datafram structure, auto change to factor!
str(whiteside)
# dimensions of data structure
dim(whiteside)

summary(whiteside)
class(whiteside)
attributes(whiteside)


edit(Cars93)
#show all the datasets
data()
