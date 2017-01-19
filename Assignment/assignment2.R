# Install the ISwR package. There is a data set called stroke. The information is the follows (use help to get information about each of the variables in the data):
# All cases of stroke in Tartu, Estonia, during the period 1991-1993, with follow-up until January 1, 1996.
# Create a new script file called A2yourname. Add commands to do the following. Make sure to include ample comments to describe your code. Upload your script to submit the assignment.
#1.     Mean, median and 75th percentile values of age
#2.     Conduct a t-test to evaluate the hypothesis for the population age of 71 for age variable.
#3.     Create a subset data frame called s1 of all patients who are not dead. Draw a plot of sex and age for these individuals. What is your takeaway from the visual?
#4.     What is the mean age of all patients who have diabetes and are not dead?
#5.     What is the mean age of all patients who have diabetes and are dead?


# Install the ISwR package.
help("INSTALL")
install.packages("ISwR")
# check the depends
library(help=ISwR)
# shows that dont need other packages
# also we could use Rstudio to install packages. Tools-->Install Packages


# Using help to find the dataset called stroke.
library(help=ISwR)
# check the structure
str(stroke)


