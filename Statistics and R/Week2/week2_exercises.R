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
install.packages("gapminder_1.0.1.tar.gz", repos = NULL, type = "source")
library(gapminder)
data(gapminder)
head(gapminder)

# Create a vector x of the life expectancies of each country for the year 1952. 
# Plot a histogram of these life expectancies to see the spread of the different countries.
library(tidyverse)
library(ggplot2)
df <- gapminder
x <- df |> 
  filter(year == "1952")

ggplot(x, aes(x = lifeExp, y = reorder(country, lifeExp))) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  labs(title = "Life Expectancy by Country (2007)",
       x = "Life Expectancy (Years)",
       y = "Country") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

## Probability Distributions Exercises #1
# In statistics, the empirical cumulative distribution function 
# (or empirical cdf or empirical distribution function) is the function F(a) for any a, 
# which tells you the proportion of the values which are less than or equal to a.

# We can compute F in two ways: the simplest way is to type mean(x <= a). 
# This calculates the number of values in x which are less than or equal to a, 
# divided by the total number of values in x, in other words the proportion of values 
# less than or equal to a.

# The second way, which is a bit more complex for beginners, is to use the ecdf() function. 
# This is a bit complicated because this is a function that doesn't return a value, 
# but a function.

# Let's continue, using the simpler mean() function.
# What is the proportion of countries in 1952 that have a life expectancy 
# less than or equal to 40?
mean(x$lifeExp <= 40)
# 0.2887324

## sapply() on a custom function
y <- x$lifeExp
qs = seq(from=min(y), to=max(y), length=20)
props = sapply(qs, function(q) mean(y <= q))
plot(qs, props)
# or
plot(ecdf(y))

############################# Normal Distributions Exercises #############################
x <- unlist( read.csv("femaleControlsPopulation.csv") )
# make averages5
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}

# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}

## Normal Distribution Exercises #1
# Use a histogram to "look" at the distribution of averages we get with a sample size of 5 
# and a sample size of 50. How would you say they differ?
library(rafalib)
mypar(1,2)
hist(averages5, xlim=c(18,30))
hist(averages50, xlim=c(18,30))
# They both look roughly normal, but with a sample size of 50 the spread is smaller.

## Normal Distribution Exercises #2
# For the last set of averages, the ones obtained from a sample size of 50, 
# what proportion are between 23 and 25?
mean(averages50 <= 25) - mean(averages50 <= 23)
# or
mean( averages50 < 25 & averages50 > 23)
# 0.982

## Normal Distribution Exercises #3
# Note that you can use the function pnorm() to find the proportion of observations 
# below a cutoff x given a normal distribution with mean mu and standard deviation sigma 
# with pnorm(x, mu, sigma) or pnorm( (x-mu)/sigma ).

# What is the proportion of observations between 23 and 25 in a normal distribution 
# with average 23.9 and standard deviation 0.43?
# Hint: Use pnorm() twice.
pnorm(25, 23.9, 0.43) - pnorm(23, 23.9, 0.43)
# 0.9765648
# The answers to 2 and 3 were very similar. 
# This is because we can approximate the distribution of the sample average with a 
# normal distribution.