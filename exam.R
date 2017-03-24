# sen lyu
# exam 1
# 1
gap = read.csv('gapminder.csv')
mean(gap[gap$year==1957,]$lifeExp)
mean(gap[gap$year==2007,]$lifeExp)
View(gap)

a = aggregate(gap$lifeExp,list(gap$year),mean)

plot(a, xlab = 'Year', ylab = 'AverageLifeExpectancy')
meana = mean(a$x)
abline(h = meana,col='red')

# 2

f1 = function()
{
  x1 = runif(n = 1,min = 3,max = 15)
  x2 = runif(n = 1,min = 3,max = 15)
  return(min(x1,x2))
}
f1()
sdist = replicate(10000,f1())

plot(density(sdist))
s = sdist[sdist<5]
p1 = length(s)/length(sdist)
p1
s2 = sdist[sdist>10]
p2 = length(s2)/length(sdist)
p2

# 3
swap = function(z)
{
  a = z[1]
  z[1] = z[length(z)]
  z[length(z)] = a
  print(z)
  return(z)
  
}

# test
a= c("A","B","c","D","E","f","g")
b = swap(a)
b
# test end

# 4
ad = read.csv("admission.csv")
GMAT = ad$GMAT
stnorm = rnorm(10000,mean=0,sd=1)
q1 = quantile(stnorm,probs = 0.25)
q2 = quantile(stnorm,probs = 0.75)
aa = q2-q1
b = 110/aa
sd1 = b
mean1 = mean(GMAT)

x11 = quantile(GMAT,probs = 0.75)
x22 = quantile(GMAT,probs = 0.25)

tstat = x11-x22
f1 = function()
{
  x1 = rnorm(length(GMAT),mean=mean1,sd=sd1)
  x2 = quantile(x1,probs = 0.25)
  x3 = quantile(x1,probs = 0.75)
  return(x3-x2)
}
sdist = replicate(10000,f1())
plot(density(sdist))
abline(v=tstat,col="blue")
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap,col="red")
abline(v=mean(sdist)+gap,col="red")
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue

