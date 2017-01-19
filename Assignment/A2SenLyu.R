# load the library, so we load the dataset
library(ISwR)
# get the mean of the age from dataframe stroke
# stroke$age get the age from stroke
mean(stroke$age)
# get the median
median(stroke$age)
# get the 75% percentitle
# quantitle(x,probs) could get the percentitle from x
# probs is numeric vector of probabilities with values in [0,1].
quantile(stroke$age,probs = 0.75)
#  Conduct a t-test to evaluate the hypothesis for the population age of 71 for age variable.
# t.test could use to conduct a t-test
t.test(stroke$age,mu=71)
