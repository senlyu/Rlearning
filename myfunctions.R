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
