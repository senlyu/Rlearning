# Assignment 5
# Sen Lyu

# use survey in MASS
library(MASS)
View(survey)
summary(survey)
x = survey$Age
y = survey$Pulse
# plot it with small dots, cex =0.5
plot(x,y,pch=16, xlab = "survey$Age", ylab = "survey$Pulse",cex=0.5)
title("Assignment 5")
# choose the female whose age is more than 21
f1 = survey[survey$Sex == "Female" & survey$Age>21,]
# show them as blue dots
points(f1$Age,f1$Pulse,pch=16,col="red")
# choose the male who exer freq
f2 = survey[survey$Sex == "Male" & survey$Exer == "Freq",]
# show them as red dots
points(f2$Age,f2$Pulse,pch=16,col="blue")




