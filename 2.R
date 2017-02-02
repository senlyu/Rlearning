

# read csv files
car1 <- read_csv("K:/Google Drive/classes/R/Session 2/Data/car insurance.csv")
fix(car1)
# change colnames
colnames(car1)[5] = "amount"
fix(car1)
# write out cvs
write.csv(car1,"car2.cvs")
#
mean(car1$veh_value)
mean(veh_value)
# acttach like import numpy, so you can use all the cols directly
attach(car1)
mean(veh_value)
# detach means un-acttach, attach close
detach(car1)

# summarizing data

# install packages
install.packages(c("vcd", "car", "dplyr"))
library(vcd)
library(car)
library(dplyr)
library(MASS)
View(whiteside)
# subset the data according to Insul
s1 = whiteside[whiteside$Insul=="After",]
View(s1)
mean(s1$Temp)
View(Arthritis)
a = Arthritis[((Arthritis$Sex=="Male") & (Arthritis$Treatment=="Treated")),]
mean(a$Age)
# or
mean(Arthritis[((Arthritis$Sex=="Male") & (Arthritis$Treatment=="Treated")),]$Age)

# check
x = table(Arthritis$Treatment,Arthritis$Improved)
prop.table(x)
# by row
prop.table(x,1)
# by col
prop.table(x,2)

xx = table(Arthritis$Sex,Arthritis$Treatment,Arthritis$Improved)
# format table
ftable(xx)
# range
range(Arthritis$Age)
# group the numbers
agegroup = cut(Arthritis$Age,breaks = c(-Inf,40,60,74))
agegroup
table(agegroup)
# group more
table(agegroup,Arthritis$Improved)

# table apply
list(Arthritis$Sex,Arthritis$Treatment)
tapply(Arthritis$Age,list(Arthritis$Sex),mean)
tapply(Arthritis$Age,list(Arthritis$Sex,Arthritis$Treatment),mean)
xxx = aggregate(Arthritis$Age,list(Arthritis$Sex,Arthritis$Treatment),mean)
class(xxx)
View(xxx)

View(Salaries)
# aggregate different function has similiar use
aggregate(Salaries$salary,list(Salaries$sex),mean)
aggregate(Salaries$yrs.service,list(Salaries$sex),mean)

#dplyr
attach(Salaries)
formula1 = (sex=="Male" & rank=="Prof")|salary>100000 
f1 = filter(Salaries,formula1) 
View(f1)
xxxx = arrange(Salaries,sex,rank)
View(xxxx)

summarize(xxxx,mean(salary),mean(yrs.service),median(yrs.since.phd))


#visualization
attach(whiteside)
plot(Insul)
plot(Insul,Gas)
plot(Temp,Gas)
plot(whiteside)
# type is the data type showing in the screen
# las is the Temp length
# cex is the circle size
plot(Temp,Gas,type="p",las=2,cex=2)
# lwd is the wide of the line
abline(h=mean(Gas),col="orange",lwd=3)
abline(v=mean(Temp),col="magenta",lwd=3)
l1 = paste("Mean Gas = ",round(mean(Gas),2))
text(0,4,l1)
l2 = paste("Mean Temp = ",mean(Temp))
text(5,5,l2,srt=90)


















