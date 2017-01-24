# Assignment1
# Sen Lyu

# Create a vector called x with a sequence of numbers from -10 to 10 incrementing by 0.1.
# seq(a,b,by=c) means the sequence will start from a to b and the increasement will be c
x=seq(-10,10,by = 0.1)
# Create another vector called y which computes the sine (which is a trigonometric function; the syntax is sin()) of the values of x.
y=sin(x)
# Create a plot of x and y. Use pch value of 19, add labels to the x and y axis - "x value" and "sine", add the title "Assignment Graph". Plot the points in color orange.
plot(x,y,pch=19,xlab = "x value",ylab = "sine",main="Assignment Graph",col="Orange")
# And the title could be added seperately in this way.
title(main="Assignment Graph")
# Write the expression in R to repeat x, y, and z as follows
# a.Repeat 7 times as follows
#"x" "y" "z" "x" "y" "z" "x" "y" "z" "x" "y" "z" "x" "y" "z" "x" "y" "z" "x" "y" "z"
# rep(a,b) means repeat a, b times.
rep(c("x","y","z"),7)
# b.Repeat x, y 4 times and z twice as follows.
#"x" "y" "x" "y" "x" "y" "x" "y" "z" "z"
# So seperate the vector to "xyxyxyxy" and "zz", first rep(c("x","y"),c(4), to get xyxyxyxy, then rep("z",2) to get zz, at last conbine them.
c(rep(c("x","y"),c(4)),rep("z",2))

