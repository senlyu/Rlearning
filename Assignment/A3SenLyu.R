# Assignment 3 Sen Lyu
# 2/4/2017

#Read the file contribution.csv
df<-read.csv("K:\\OneDrive\\Git\\Rlearning\\contribution.csv",header = T)

#Create a frequency table between Gender and Marital Status. 
table1 = table(df$Gender,df$Marital.Status)
table2 = prop.table(table1)
#table2 is the frequency table

#What percentage of divorcees (denoted as D) are male?
#1 use margin =2 to get the percentage between male and female of each column.
pt1 = prop.table(table1,margin = 2)
#2 get the data of male from divorcees
answer1 = paste(pt1["M","D"]*100,"%")
# the answer for percentage of divorcees (denoted as D) are male is in answer1

#What percentage of the total are single (denoted as S) females?
#1 get the total percentage
pt2 = prop.table(table1)
#2 get the single and female
answer2 = paste(pt2["F","S"]*100,"%")
# the answer for percentage of the total are single (denoted as S) females is in answer2

#What is the most common marital status of females?
pt3 = table1
#find the max column name of the most common marital status of females
#http://stackoverflow.com/questions/10290801/getting-column-name-which-holds-a-max-value-within-a-row-of-a-matrix-holding-a-s
answer3 = names(which.max(table1["F",]))
# the answer for most common marital status of females is in answer3

#Compute the median value of FY04Giving based on Gender and Marital Status.
fy4t = aggregate(df$FY04Giving,list(df$Marital.Status,df$Gender),median)
#Which two groups had the highest median giving?
sortedfy4t = fy4t[order(fy4t$x,decreasing = T),]
answer4 = head(sortedfy4t,2)
# the answer for two groups had the highest median giving is in answer4

#Cut 'Class Year' into 2 groups with 3 break points -Inf, 1980, Inf.
#Save it as cyear.
cyear = cut(df$Class.Year,c(-Inf,1980,Inf))
#Calculate the average FY03Giving grouping by cyear and Gender.
fy3t = aggregate(df$FY03Giving,list(cyear,df$Gender),mean)
#the answer for
sortedfy3t = fy3t[order(fy3t$x),]
answer5 = head(sortedfy3t,1)
# the answer for group gave the lowest is in answer5

#Install and explore package dplyr.
install.packages("dplyr")
library(dplyr)
#Create a subset with individuals whose Next Degree is either MS or PHD.
#Save it as S1.
s1 = df[df$Next.Degree=="MS"|df$Next.Degree=="PHD",]
#or use dplyr
s1 = filter(df,Next.Degree=="MS"|Next.Degree=="PHD")
View(s1)
#Sort the subset S1 on Gender and Next Degree.
#Save it as S2.
s2 = s1[order(s1$Gender,s1$Next.Degree),]
View(s2)
#or use dplyr
s2 = arrange(s1,Gender,Next.Degree)

#Using the dataset quakes in the datasets library to complete the following 2 tasks
dsq = quakes
#Split the plotting region into 1 row and 3 columns with grey background and blue color
opar = par()
par(bg="grey",mfrow=c(1,3),col="blue")
#and then plot the following graphs: lat and long, depth and mag, stations and mag.
plot(dsq$lat,dsq$long)
plot(dsq$depth,dsq$mag)
plot(dsq$stations,dsq$mag)
par(opar)
#Draw a histogram for mag with density on the y-axis.
hist(dsq$mag,prob= T)
#Add a vertical line to indicate the mean of mag in red with line width 3.
abline(v=mean(dsq$mag),col="red",lwd=3)
#Give the line an appropriate label.
text(4.7,0.2,paste("Avg Mag = ",mean(dsq$mag)),srt=90)

