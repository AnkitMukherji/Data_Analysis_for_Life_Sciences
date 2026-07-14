setwd("Statistics and R/Week2")

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
if (!file.exists(filename)) {download(url, destfile=filename)}
x <- unlist( read.csv(filename) )
# Here x represents the weights for the entire population.

############################# Random Variables Exercises #############################

## Random Variables Exercises #1 ##
# What is the average of these weights?
mean(x)
# 23.89338


## Random Variables Exercises #2 ##
# Take a random sample of size 5. 
# What is the absolute value (use abs()) of the difference between 
# the average of the sample and the average of all the values?
set.seed(1)
X = sample(x, 5)
abs(mean(X) - mean(x))
# 0.3293778

## Random Variables Exercises #3 ##
# After setting the seed at 5, set.seed(5), take a random sample of size 5. 
# What is the absolute value of the difference between the average of the sample and 
# the average of all the values?
set.seed(5)
X = sample(x, 5)
abs(mean(X) - mean(x))
# 0.3813778

## Random Variables Exercises #4 ##
# Why are the answers from 2 and 3 different?
# Because the average of the samples is a random variable.

############################# Null Distributions Exercises #############################

## Null Distributions Exercises #1 ##
# Set the seed at 1, then using a for-loop take a random sample of 5 mice 1,000 times. 
# Save these averages.
# What proportion of these 1,000 averages are more than 1 gram away from the average of x ? 
# Proportions are written as numbers between zero and one.
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}
hist(averages5) ##take a look
mean( abs( averages5 - mean(x) ) > 1)
# 0.503

## Null Distributions Exercises #2 ##
# We are now going to increase the number of times we redo the sample from 1,000 to 10,000. 
# Set the seed at 1, then using a for-loop take a random sample of 5 mice 10,000 times. 
# Save these averages.
# What proportion of these 10,000 averages are more than 1 gram away from the average of x ?
set.seed(1)
n <- 10000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}
hist(averages5) ##take a look
mean( abs( averages5 - mean(x) ) > 1)
# 0.5084

############################# Probability Distributions Exercises #############################

