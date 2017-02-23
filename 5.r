# sampling error
x = rnorm(20, mean=12, sd=6)
mean(x)
sd(x)


# sampling distribution

f1 = function(){
  # flip a fair coin 10 times
  v = c(0,1)
  p = c(0.5,0.5)
  x = sample(size = 10, x = v, prob = p, replace = T)
  sum(x)
}

# sdist
rep(f1(),10000)
# rep() only take the result rep it
sdist = replicate(n = 10000,f1())
prop.table(table(sdist))
plot(table(sdist),type="h")

# 
getwd()
ad = read.csv("admission.csv")
GMAT = ad$GMAT
# compute the test statistic
tstat = mean(GMAT)
# descibe the population based on the hypothesis
# draw a synthetic sample and compute the metric
f1 = function()
{
  x = rnorm(length(GMAT),mean = 510,sd = sd(GMAT))
  return(mean(x))
}
f1()
# create the sampling distribution
sdist = replicate(100000,f1())
# draw a sampling distribution and compute p-value
plot(density(sdist))
abline(v=tstat,col="blue")
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap,col="red")
abline(v=mean(sdist)+gap,col="red")
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue

# sd
tstat = sd(GMAT)
f1 = function()
{
  x = rnorm(length(GMAT),mean = mean(GMAT),sd = 78)
  return(sd(x))
}
sdist = replicate(100000,f1())
plot(density(sdist))
abline(v=tstat,col="blue")
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap,col="red")
abline(v=mean(sdist)+gap,col="red")
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue

# median
tstat = median(GMAT)
f1 = function()
{
  x = rnorm(length(GMAT),mean = 500,sd = sd(GMAT))
  return(median(x))
}
sdist = replicate(100000,f1())
plot(density(sdist))
abline(v=tstat,col="blue")
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap,col="red")
abline(v=mean(sdist)+gap,col="red")
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue

# 75%
tstat = quantile(GMAT,probs = 0.75)
tstat = 600
a=qnorm(0.75,mean=0,sd=1)
# x - u = qnorm() * sd()
# x = tstat
# u and sd() both unknown, so could choose randomly
# use this to calcluate the actual value of the point
f1 = function()
{
  x = rnorm(length(GMAT),mean = 545,sd = 81.5)
  return(quantile(x,probs = 0.75))
}
f1()
sdist = replicate(100000,f1())
plot(density(sdist))
abline(v=tstat,col="blue")
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap,col="red")
abline(v=mean(sdist)+gap,col="red")
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
s1
pvalue = length(s1)/length(sdist)
pvalue

# ad
prop.table(table(ad$De))
tstat = prop.table(table(ad$De))[1]
f1 = function()
{
  sampl = sample(size = length(GMAT),x = c("1","2"),prob = c(0.4,0.6),replace = T)
  y = length(sampl[sampl=="1"])/length(GMAT)
  x = prop.table(table(sampl))[1]
  return(x)
}
f1()
sdist = replicate(100000,f1())
plot(density(sdist))
abline(v=tstat,col="blue")
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap,col="red")
abline(v=mean(sdist)+gap,col="red")
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue





















