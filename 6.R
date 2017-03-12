# 6
ad = read.csv("admission.csv")
GMAT = ad$GMAT
GPA = ad$GPA

# test of correlation
# whether GMAT and GPA are correlated


# compute the test statistic
tstat = cor(GMAT,GPA)

# hyo0 = no correlation between GMAT & GPA
# distribution GMAT = n(), GPA = n(), parameter to be in the sample
# get many samples and do the test


f1 = function()
{
  x1 = rnorm(length(GMAT),mean = mean(GMAT),sd = sd(GMAT))
  x2 = rnorm(length(GPA),mean = mean(GPA),sd = sd(GPA))
  x = cor(x1,x2)
  return(x)
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
# p is low so h0 is rejected

# shape test
plot(density(GMAT))
plot(density(GPA))

# standise
GMATs = (GMAT - mean(GMAT))/sd(GMAT)
GPAs = ((GPA - mean(GPA))/sd(GPA))
plot(density(GPAs))
lines(density(GMATs),col="blue")

# h0 they are the same shape
# compute the test statisctic
q = c(0.1,0.2,0.3,0.67,0.96)
q1 = quantile(GMATs,probs = q)
q2 = quantile(GPAs,probs = q)
tstat = sum(abs(q1-q2))

f1 = function()
{
  x1 = rnorm(length(GMAT))
  x2 = rnorm(length(GPA))
  q1 = quantile(x1,probs = q)
  q2 = quantile(x2,probs = q)
  return(sum(abs(q1-q2)))
}
sdist = replicate(100000,f1())
plot(density(sdist))
abline(v=tstat,col="blue")
s1 = sdist[sdist>tstat]
pvalue = length(s1)/length(sdist)
pvalue
# cannot reject

# for mean
# confidence intervals
f1 = function()
{
  x = rnorm(length(GMAT),mean = mean(GMAT),sd = sd(GMAT))
  return(mean(x))
}

# create the sampling distribution
sdist = replicate(10000,f1())

# compute confidence interval
plot(density(sdist))
quantile(sdist,probs = c(0.025,1-0.025))

# for median
f1 = function()
{
  x = rnorm(length(GMAT),mean = mean(GMAT),sd = sd(GMAT))
  return(median(x))
}
# create the sampling distribution
sdist = replicate(10000,f1())

# compute confidence interval
plot(density(sdist))
quantile(sdist,probs = c(0.025,1-0.025))

# for 75 quantile
f1 = function()
{
  x = rnorm(length(GMAT),mean = mean(GMAT),sd = sd(GMAT))
  return(quantile(x,probs = 0.75))
}
# create the sampling distribution
sdist = replicate(10000,f1())

# compute confidence interval
plot(density(sdist))
quantile(sdist,probs = c(0.025,1-0.025))


# nonparameter test
# the good is no parameter no assumption
# the bad is not use all the information it has
# use median to do the model

tstat = sum(ifelse(GMAT>500,1,0))

f1 = function()
{
  v = c(0,1)
  p = c(0.5,0.5)
  x = sample(size = length(GMAT),x = v,prob = p,replace = T)
  return(sum(x))
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

# nonparameter with assumption mirror
# think about how no mirror data will effect with this sign test
# no sign will not be "in the middle"


# bootstrapping
# 1 use the samples to make a population
# use the population to do the sample

# hard to use to do test. the test need to be set to be like the h0, however bootstrapping cannot make
# the model in the way of h0



# for test the shape of the GMAT and GPA
# standize the data

# bootstrapping

# draw to samples twice and calculate the diff
# the tstat is the diff between GMAt and GPA




# test the dataset sleep and money is related
Sleep_and_Money <- read_csv("~/Sleep and Money.csv")
sleep = Sleep_and_Money$sleep
money = Sleep_and_Money$money

tstat = cor(sleep,money)

f1 = function()
{
  x1 = rnorm(length(sleep),mean = mean(sleep),sd = sd(sleep))
  x2 = rnorm(length(money),mean = mean(money),sd = sd(money))
  x = cor(x1,x2)
  return(x)
}
f1()
# create the sampling distribution
sdist = replicate(10000,f1())
# draw a sampling distribution and compute p-value
plot(density(sdist))
abline(v=tstat,col="blue")
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap,col="red")
abline(v=mean(sdist)+gap,col="red")
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue



