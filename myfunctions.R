f1 <-
function(a,m)
{
  metrics = c(MAD=0,MSE=0,MAPE=0,MPSE=0,R2=0,TMAD=0)
  metrics["MAD"] = mean(abs(a-m))
  metrics["MSE"] = mean((a-m)^2)
  metrics["MAPE"] = mean(abs((a-m)/a))
  metrics["MPSE"] = mean(((a-m)/a)^2)
  #metrics["TMAD"] = mean(abs(a-m),trim = 0.05)
  # TODO: find a function to get only 95% good TMAD
  tmad = abs(a-m)
  metrics["TMAD"] = mean(tmad[tmad<quantile(abs(a-m),probs=0.95)])
  metrics["R2"] = 1-sum((a-m)^2)/sum((a-mean(a))^2)
  return(metrics)
}
bin <-
function(a,m,k=10)
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
xyz <-
function(a,m,incre=0.05)
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
