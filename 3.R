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



