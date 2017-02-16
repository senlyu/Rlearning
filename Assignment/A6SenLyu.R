# Assignment 6
# SenLyu 

# all the input 

# question1
# the function takes a input vector and output a vector with the mean
# since the input vector is numeric, the vector cannot be Null
f1 = function(inputv)
{
  # get the length
  seqs = seq(length(inputv))
  a = NULL
  # for each get the mean from a[1] to a[i]
  for (i in seqs)
  {
    a = c(a,mean(inputv[1:i]))
  }
  return(a)
}

# test sample
test11 = c(1:10)
test12 = seq(100,1000,2)
resulttest11 = f1(test11)
resulttest12 = f1(test12)
resulttest11
resulttest12

# question2
# function takes a vector and piy put a dataframe

f2 = function(inputv, alpha = 0.8)
{
  n = length(inputv)
  # since the input vector is numeric, the vector cannot be Null
  a = inputv[1]
  i = 1
  while (i<n)
  {
    a[i+1] = a[i] + alpha * (inputv[i] -a[i])
    i = i + 1
  }
  result = data.frame(inputv,a)
  colnames(result) = c("actual", "predicted")
  return(result)
}
test21 = c(1:10)
test22 = seq(100,150,2)
resulttest21 = f2(test21)
resulttest22 = f2(test22)
resulttest21
resulttest22

# question 3
# use package schoolmath to find prim
install.packages("schoolmath")
# f3 function needs package schoolmath
f3 = function(a=1,b=1)
{
  library(schoolmath)
  seqs = a:b
  countss = 0
  for (i in seqs)
  {
    # I find that the result of is.prim(1) is true, that is not right
    if(is.prim(i) & i != 1) 
    {
      countss = countss + 1
    }
  }
  return(countss)
}

# for q3 test
f3(4,4)
f3(1,100)
f3(100,1)
f3(2,3)


# question 4
# if we just think it like the actual what happens
f4 = function()
{
  # straight forward, from 1:6, get it twice, and replace
  a = sample(1:6,2,replace = TRUE)
  # if the dices are same
  return(ifelse(a[1] == a[2],"You Win","You Lose"))
}

# actually we can calculate the probobility and make the model even simple
# since there are only 6 situations that we will have the same dices and the total cases are 36 cases
# so the model can be simplify to 1/6 chance we will win
f4 = function()
{
  # 6 cases, 1 is win, 5 lose
  seq = rep(c("You Win","You Lose"),c(1,5))
  a = sample(seq,1)
  return(a)
}

# for q4 test
for (i in 1:6) 
{
  print(f4())
}
 

# question 5
# consider that missing value also is a unique value
Missing = function(inputdf)
{
  # get all the colnames
  coln = colnames(inputdf)
  # get the length of the colnames
  n = length(coln)
  # set the vectors to be NULL
  mvc = NULL
  mvpc = NULL
  uvc = NULL
  # for each col do calculate
  for (i in 1:n)
  {
    # find the numbers of the missing value
    mv = sum(is.na(inputdf[,i]))
    # get the total numbers of values
    totalv = length(inputdf[,i])
    # * 100 for percentage
    # get the percentages
    mvp = mv / totalv * 100
    # get the unique value
    uv = length(unique(inputdf[,i]))
    # if the NaN is not a unique value
    # if (mv>0) uv = uv -1
    mvc = c(mvc,mv)
    mvpc = c(mvpc,mvp)
    uvc = c(uvc,uv)
  }
  result = data.frame(coln,mvc,mvpc,uvc)
  # change the colnames
  colnames(result) = c("Column Name","# Missing Values","% Missing Values","# Unique Values")
  return(result)
}
# for q5 test
testdf1v1 = rep(c(NaN,1,2,3),300)
testdf1v2 = rep(c(NaN,1,2),400)
testdf1v3 = rep(c(NaN,1),600)
testdf1 = data.frame(testdf1v1,testdf1v2,testdf1v3)

testdf2v1 = rep(c(NaN,1:99),100) 
testdf2v2 = rep(c(NaN,rep(1,999)),10)
testdf2v3 = rep(c(NaN,rep(1,9999)),1)
testdf2v4 = rep(1:10000)
testdf2v5 = rep(NaN,10000)
testdf2 = data.frame(testdf2v1,testdf2v2,testdf2v3,testdf2v4,testdf2v5)

resulttest51 = Missing(testdf1)
resulttest51
resulttest52 = Missing(testdf2)
resulttest52




