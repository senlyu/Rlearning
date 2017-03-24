library(readr)
data1 <- read_csv("data1.csv")
View(data1)
x = data1$x1
p1 = dnorm(3.327,mean = 5, sd = 2)
p2 =dnorm(3.814,5,2)
p3 =dnorm(5.502,5,2)

#Likelyhood = p1 + p2 + p3 ...
#LL = logLikelyhood = log(likelyhood)=log(sum(p1,p2,p3...))
# p = density value of ... 
?dnorm
install.packages("bbmle")
install.packages("stats4")
library(bbmle,help = T)

#DGP - N(M,2)
f1 <- function(M){
  LLsum = sum(dnorm(x, mean = M, sd = 2, log = T))
  return(-1*LLsum)
}
f1(5)
res = mle2(minuslogl = f1, start = list(M = 5))
summary(res)

#DGP - N(M,s)

f1 = function(M,s){
  LLsum = sum(dnorm(x,mean = M, sd = s, log = T))
  return(-1*LLsum)
}

f1(1,10)
res = mle2(minuslogl = f1, start = list(M = 5, s = 10))
summary(res)

res = mle2(minuslogl = f1, start =list(M=1, s=1), method = "L-BFGS-B",
           lower = c(s = 0))
summary(res)


library(readr)
data2 <- read_csv("data2.csv")
x = data2$x1
#DGP - Pois(1)
#Poisson distribution (only one unknown variable: lambda)
f1 = function(l){
  LLsum = sum(rpois(x,l))
  return(-1*LLsum)
}
f1(1)
res = mle2(minuslogl = f1, start =list(l=1))
summary(res)


# start
x = data2$x2
f1 = function(p,l)
{
  L = ifelse(x==0,p+(1-p)*dpois(0,l),(1-p)*dpois(x,l))
  LLsum = sum(log(L))
  return(-1*LLsum)
}
f1(0.5,1)
res = mle2(minuslogl = f1,start = list(p=0.5,l=1))
summary(res)

