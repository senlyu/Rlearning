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
# t.test(x,mu) could use to conduct a t-test, and the hypothesis that the mean is mu.
t.test(stroke$age,mu=71)
# Create a subset data frame called s1 of all patients who are not dead.  
# use stroke$dead to call the dead colum. Use "," to get all the rest.
# http://stackoverflow.com/questions/19205806/undefined-columns-selected-when-subsetting-data-frame
s1=stroke[stroke$dead==F,]
# Draw a plot of sex and age for these individuals.
plot(stroke$sex, stroke$age)
# What is your takeaway from the visual?
# 1. male not dead has a bigger maximum and a smaller minimum.
# 2. there are both outliers
# 3. female has a bigger median, and a small IQR

# What is the mean age of all patients who have diabetes and are not dead?
# stroke$age[stroke$diab=="Yes" & stroke$dead==F] could call all the rows who is disable and not dead.
# And because there are NA in the results, so if we need to calculate the mean, we need get rid of NA values
# So we use na.rm = T
mean(stroke$age[stroke$diab=="Yes" & stroke$dead==F],na.rm = T)

# What is the mean age of all patients who have diabetes and are dead?
mean(stroke$age[stroke$diab=="Yes" & stroke$dead==T],na.rm = T)


