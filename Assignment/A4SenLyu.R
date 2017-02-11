# Assignment 4 
# sen lyu

# create a vector as x
# place the values 10 to 1000.
x = c(10:1000)

# create a vector as y
# y = square root of log of x
y = sqrt(log(x))

# create a vector z
# z =50/x
z = 50/x

# create a plot to show both y and z
plot(x,y,col = "blue",ylim=c(0,10),ylab = "y and z" )
points(x,z,col = "red",pch = 16)

# create expression()
y_t = expression(y==sqrt(log(x)))

# frac will be better than just use /
# http://vis.supstat.com/2013/04/mathematical-annotation-in-r/
z_t = expression(z==frac(50,x))

y_t1 = expression(y==alpha^2+gamma+sqrt(beta))

#text(400,3,y_t)
#text(400,1,z_t)
text(400,5,y_t1)


title("Assignment 4")

