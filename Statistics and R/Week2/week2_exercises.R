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

############################# Population, Samples, and Estimates Exercises #############################
dat <- read.csv("mice_pheno.csv")
dat <- na.omit(dat)
head(dat)

## Population, Samples, and Estimates Exercises #1
# Use dplyr to create a vector x with the body weight of all males on the control (chow) diet.
# What is this population's average?
library(tidyverse)
x <- dat |> 
  filter(Sex == "M" & Diet == "chow") |> 
  pull(Bodyweight)
mean(x)
# 30.96381

## Population, Samples, and Estimates Exercises #2
# Now use the rafalib package and use the popsd() function to compute the population standard deviation.
library(rafalib)
popsd(x)
# 4.420501

## Population, Samples, and Estimates Exercises #3
# Set the seed at 1. Take a random sample X of size 25 from x.
# What is the sample average?
set.seed(1)
X <- sample(x, 25)
mean(X)
# 30.5196

## Population, Samples, and Estimates Exercises #4
# Use dplyr to create a vector y with the body weight of all males on the high fat hf) diet.
# What is this population's average?
y <- dat |> 
  filter(Sex == "M" & Diet == "hf") |> 
  pull(Bodyweight)
mean(y)
# 34.84791

## Population, Samples, and Estimates Exercises #5
# Now use the rafalib package and use the popsd() function to compute the population standard deviation.
popsd(y)
# 5.574609

## Population, Samples, and Estimates Exercises #6
# Set the seed at 1. Take a random sample  of size 25 from y.
# What is the sample average?
set.seed(1)
Y <- sample(y, 25)
mean(Y)
# 35.8036

## Population, Samples, and Estimates Exercises #7
# What is the difference in absolute value between y(bar) - x(bar) and Y(bar) - X(bar)?
abs((mean(y) - mean(x)) - (mean(Y) - mean(X)))
# 1.399884

## Population, Samples, and Estimates Exercises #8
# Repeat the above for females, this time setting the seed to 2.
# What is the difference in absolute value between y(bar) - x(bar) and Y(bar) - X(bar)?
# Make sure to set the seed to 2 before each sample() call. This function should be called twice.
x <- dat |> 
  filter(Sex == "F" & Diet == "chow") |> 
  pull(Bodyweight)

set.seed(2)
X <- sample(x, 25)

y <- dat |> 
  filter(Sex == "F" & Diet == "hf") |> 
  pull(Bodyweight)

set.seed(2)
Y <- sample(y, 25)

abs((mean(y) - mean(x)) - (mean(Y) - mean(X)))
# 0.3647172

############################# Central Limit Theorem Exercises #############################
dat <- na.omit(read.csv("mice_pheno.csv"))
head(dat)

## Central Limit Theorem Exercises #1
# If a list of numbers has a distribution that is well approximated by the normal distribution, 
# what proportion of these numbers are within one standard deviation away from the list's average?
# Use the pnorm() function. You can look up more information with ?pnorm.
pnorm(1) - pnorm(-1)
# 0

## Central Limit Theorem Exercises #2
# What proportion of these numbers are within two standard deviations away from the list's average?
pnorm(2) - pnorm(-2)
# 0.9544997

## Central Limit Theorem Exercises #3
# What proportion of these numbers are within three standard deviations away from the list's average?
pnorm(3) - pnorm(-3)
# 0.9973002

## Central Limit Theorem Exercises #4
# Define y to be the weights of males on the control diet.

# What proportion of the mice are within one standard deviation away from the average weight?
# Remember to use popsd() from rafalib for the population standard deviation.
library(tidyverse)
library(rafalib)

y <- dat |> 
  filter(Sex == "M" & Diet == "chow") |> 
  pull(Bodyweight)

z <- (y - mean(y)) / popsd(y)
mean(abs(z) <= 1)
# 0.6950673

## Central Limit Theorem Exercises #5
# What proportion of these numbers are within two standard deviations away from the list's average?
mean(abs(z) <= 2)
# 0.9461883

## Central Limit Theorem Exercises #6
# What proportion of these numbers are within three standard deviations away from the list's average?
mean(abs(z) <= 3)
# 0.9910314

## Central Limit Theorem Exercised #7
# Note that the numbers for the normal distribution and our weights are relatively close. 
# Also, notice that we are indirectly comparing quantiles of the normal distribution to quantiles 
# of the mouse weight distribution. We can actually compare all quantiles using a qqplot.
qqnorm(z)
abline(0,1)
# Which of the following best describes the qq-plot comparing mouse weights to the normal distribution?

# The mouse weights are well approximated by the normal distribution, 
# although the larger values (right tail) are larger than predicted by the normal. 
# This is consistent with the differences seen between question 3 and 6.

## Central Limit Theorem Exercises #8
# Here we are going to use the function replicate() to learn about the distribution of random variables. 
# All the above exercises relate to the normal distribution as an approximation of the distribution 
# of a fixed list of numbers or a population. We have not yet discussed probability in these exercises. 
# If the distribution of a list of numbers is approximately normal, then if we pick a number at random 
# from this distribution, it will follow a normal distribution. However, it is important to remember 
# that stating that some quantity has a distribution does not necessarily imply this quantity is random. 
# Also, keep in mind that this is not related to the central limit theorem. 
# The central limit applies to averages of random variables. Let's explore this concept.

# We will now take a sample of size 25 from the population of males on the chow diet. 
# The average of this sample is our random variable. 
# We will use the replicate() function to observe 10,000 realizations of this random variable. 
# Set the seed at 1, then generate these 10,000 averages. 
# Make a histogram and qq-plot of these 10,000 numbers against the normal distribution.

# We can see that, as predicted by the CLT, the distribution of the random variable is very well 
# approximated by the normal distribution.
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

# What is the average of the distribution of the sample average?
mean(avgs)
# 30.96856

## Central Limit Theorem Exercises #9
# What is the standard deviation of the distribution of sample averages (use popsd())?
popsd(avgs)
# 0.827082