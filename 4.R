


getwd()
Bi = read.csv("/Users/senlyu/Desktop/Session\ 3/Data/BinaryPred.csv")

a = Bi$Target
m = Bi$Model1


# calcluate summary to see which model is better
k = 10
metrics = c(LL=0,AIC=0,BIC=0,R2=0)
metrics["LL"] = sum(ifelse(a==1,log(m),log(1-m)))
metrics["AIC"] = -2*metrics["LL"]+2*k
metrics["BIC"] = -2*metrics["LL"]+2*k*log(length(a))
sst = sum((a-mean(a))^2)
sse = sum((a-m)^2)
metrics["R2"] = 1 - (sse/sst)
metrics

bin = function2(a,m,k=10)
{
  metrics = c(LL=0,AIC=0,BIC=0,R2=0)
  metrics["LL"] = sum(ifelse(a==1,log(m),log(1-m)))
  metrics["AIC"] = -2*metrics["LL"]+2*k
  metrics["BIC"] = -2*metrics["LL"]+2*k*log(length(a))
  sst = sum((a-mean(a))^2)
  sse = sum((a-m)^2)
  metrics["R2"] = 1 - (sse/sst)
  return(metrics)
}

bin(a=Bi$Target,m=Bi$Model1)
bin(a=Bi$Target,m=Bi$Model2)

# save that
setwd("/Users/senlyu/classnote-Rlearning/")
dump('bin',file = 'myfunctions.R',append = TRUE)

# predict the data using threthoud 0.7
p = 0.7
mnew = ifelse(m>p,1,0)
# test for ifelse
ifelse(-1,1,2)
ifelse(1,1,2)

# try to make a confusion matrics
x = table(factor(mnew),factor(a))
x

vec = as.vector(x)
vec
# TN FP FN TP

# contarin P in the output
vec = c(p,vec)
vec

# loop function to put all p and the results into a dataframe
incre = 0.05
cutoff = seq(min(m),max(m),by = incre)

mresult = data.frame()
for (p in cutoff)
{
  mnew = ifelse(m-p>0,1,0)
  x = table(factor(mnew),factor(a))
  vec = as.vector(x)
  vec = c(p,vec)
  mresult = rbind(mresult,vec)
}
colnames(mresult) = c("cutoff","TN","FP","FN","TP")
View(mresult)

# creeate a even bigger function
xyz = function3(a,m,incre=0.05)
{
  cutoff = seq(min(m),max(m),by = incre)
  mresult = data.frame()
  for (p in cutoff)
  {
    mnew = ifelse(m-p>0,1,0)
    x = table(factor(mnew),factor(a))
    vec = as.vector(x)
    vec = c(p,vec)
    mresult = rbind(mresult,vec)
  }
  colnames(mresult) = c("cutoff","TN","FP","FN","TP")
  return(mresult)
}
setwd("/Users/senlyu/classnote-Rlearning/")
dump('xyz',file = 'myfunctions.R',append = TRUE)

x = xyz(a=Bi$Target,m=Bi$Model2)
x

# 
x1 = x$FP/max(x$FP)
y1 = x$TP/max(x$TP)
plot(x1,y1,type="l")
AUC = mean(y1)


# homework
length(cutoff)
x1=9
x2=10
cutoff[x1:x2]

a = sample(1:6,2,replace = TRUE)
ifelse(a[1] == a[2],"your win","your lose")
a = c(1:10,1:10)
length(unique(a))



# statistics from Ground up
v = c("H","T")
p = c(0.5,0.5)
sample(size = 6,x = v,prob = p,replace = TRUE)

v = seq(1,6)
p = rep(1/6,6)
sample(size = 3,x = v,prob = p,replace = T)


# dnorm, pnorm, qnorm, rnorm
dnorm(x = 50,mean = 67,sd = 6)
pnorm(71,mean = 67,sd = 6)
qnorm(p = 0.75,mean = 67,sd = 6)
rnorm(n = 100,mean = 67,sd = 6)

# convolutions
r1 = rnorm(3000)
r2 = rnorm(3000)
u1 = runif(3000)
u2 = runif(3000)
u3 = runif(3000)
u4 = runif(3000)
u5 = runif(3000)
u6 = runif(3000)

plot(density(x))
x = u1 + u2 + u3 + u4 + u5 + u6
a = shapiro.test(x)
a[2]

# get the normal dis
x = 0
n = 0
a = 0
while (a<0.05)
{
  u = runif(3000);
  x = x + u;
  n = n + 1;
  a1 = shapiro.test(x);
  a = a1[2];
}
a
n

# ok, this is use subset to analysis the data
t1 = rnorm(100000,mean = 5,sd = 3)
t2 = runif(100000,min = 3,max = 7)
t = t1 + t2
t
s1 = t[t<10]
length(s1)
length(s1)/100000

t = ifelse(t1>t2,t1,t2)
plot(density(t))
s1 = t[t<10]
length(s1)/100000

# sampling error
x = rnorm(20,mean = 12,sd = 6)
mean(x)
sd(x)
