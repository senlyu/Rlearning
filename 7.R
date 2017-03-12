twosample = read.csv("~/twosample.csv")
View(twosample)
treatment = twosample[twosample$group=="Treatment",2]
control = twosample[twosample$group=="Control",2]
treatment
control
nt = length(treatment)
nc = length(control)

# tstat
tstat = mean(treatment)-mean(control)

# describe the population and create synthetic samples
# this two is the same

f1 = function()
{
  x = c(treatment,control)
  x = sample(x)
  
  m1 = mean(x[1:nt])
  m2 = mean(x[(nt+1):(nt+nc)])
  return(m1-m2)
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
# not big enough to show the h0 is true, so the answer is false

# now we have a A and B has a correlation=0.6 test this

ad = read.csv("admission.csv")
GMAT = ad$GMAT
GPA = ad$GPA

install.packages("mvtnorm")
library(mvtnorm)

M = c(mean(10),mean(6))
S = matrix(c(4,2,2,5),nrow = 2, ncol = 2)
rmvnorm(n = 50, mean = M, sigma = S)

# tstat
tstat = cor(GPA,GMAT)

f1 = function()
{
  M = c(mean(GPA),mean(GMAT))
  S = matrix(c(var(GPA),0.6*sd(GPA)*sd(GMAT),0.6*sd(GPA)*sd(GMAT),var(GMAT)),
             nrow = 2, ncol = 2)
  x = rmvnorm(length(GMAT), mean = M, sigma = S)
  return(cor(x[,1],x[,2]))
}
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


# statistical estimation
data1 = read.csv("data1.csv")
x1 = data1$x1

dnorm(8.527,5,2)

#likelihood the probility to see the whole sample

m = 6
sum(dnorm(x1, mean = m, sd =2, log= T))

mseq = seq(0,10,by = 0.05)

mseq

f1 = function(m)
{
  LL = sum(dnorm(x1, mean = m, sd =2, log= T))
  return(LL)
}

LLres = sapply(mseq,f1)
i = which.max(LLres)
mseq[i]




























