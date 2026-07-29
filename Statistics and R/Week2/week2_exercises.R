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

## Probability Distributions Exercises #1 ##
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

## Normal Distribution Exercises #1 ##
# Use a histogram to "look" at the distribution of averages we get with a sample size of 5 
# and a sample size of 50. How would you say they differ?
library(rafalib)
mypar(1,2)
hist(averages5, xlim=c(18,30))
hist(averages50, xlim=c(18,30))
# They both look roughly normal, but with a sample size of 50 the spread is smaller.

## Normal Distribution Exercises #2 ##
# For the last set of averages, the ones obtained from a sample size of 50, 
# what proportion are between 23 and 25?
mean(averages50 <= 25) - mean(averages50 <= 23)
# or
mean( averages50 < 25 & averages50 > 23)
# 0.982

## Normal Distribution Exercises #3 ##
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

## Population, Samples, and Estimates Exercises #1 ##
# Use dplyr to create a vector x with the body weight of all males on the control (chow) diet.
# What is this population's average?
library(tidyverse)
x <- dat |> 
  filter(Sex == "M" & Diet == "chow") |> 
  pull(Bodyweight)
mean(x)
# 30.96381

## Population, Samples, and Estimates Exercises #2 ##
# Now use the rafalib package and use the popsd() function to compute the population standard deviation.
library(rafalib)
popsd(x)
# 4.420501

## Population, Samples, and Estimates Exercises #3 ##
# Set the seed at 1. Take a random sample X of size 25 from x.
# What is the sample average?
set.seed(1)
X <- sample(x, 25)
mean(X)
# 30.5196

## Population, Samples, and Estimates Exercises #4 ##
# Use dplyr to create a vector y with the body weight of all males on the high fat hf) diet.
# What is this population's average?
y <- dat |> 
  filter(Sex == "M" & Diet == "hf") |> 
  pull(Bodyweight)
mean(y)
# 34.84791

## Population, Samples, and Estimates Exercises #5 ##
# Now use the rafalib package and use the popsd() function to compute the population standard deviation.
popsd(y)
# 5.574609

## Population, Samples, and Estimates Exercises #6 ##
# Set the seed at 1. Take a random sample  of size 25 from y.
# What is the sample average?
set.seed(1)
Y <- sample(y, 25)
mean(Y)
# 35.8036

## Population, Samples, and Estimates Exercises #7 ##
# What is the difference in absolute value between y(bar) - x(bar) and Y(bar) - X(bar)?
abs((mean(y) - mean(x)) - (mean(Y) - mean(X)))
# 1.399884

## Population, Samples, and Estimates Exercises #8 ##
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

## Central Limit Theorem Exercises #1 ##
# If a list of numbers has a distribution that is well approximated by the normal distribution, 
# what proportion of these numbers are within one standard deviation away from the list's average?
# Use the pnorm() function. You can look up more information with ?pnorm.
pnorm(1) - pnorm(-1)
# 0

## Central Limit Theorem Exercises #2 ##
# What proportion of these numbers are within two standard deviations away from the list's average?
pnorm(2) - pnorm(-2)
# 0.9544997

## Central Limit Theorem Exercises #3 ##
# What proportion of these numbers are within three standard deviations away from the list's average?
pnorm(3) - pnorm(-3)
# 0.9973002

## Central Limit Theorem Exercises #4 ##
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

## Central Limit Theorem Exercises #5 ##
# What proportion of these numbers are within two standard deviations away from the list's average?
mean(abs(z) <= 2)
# 0.9461883

## Central Limit Theorem Exercises #6 ##
# What proportion of these numbers are within three standard deviations away from the list's average?
mean(abs(z) <= 3)
# 0.9910314

## Central Limit Theorem Exercised #7 ##
# Note that the numbers for the normal distribution and our weights are relatively close. 
# Also, notice that we are indirectly comparing quantiles of the normal distribution to quantiles 
# of the mouse weight distribution. We can actually compare all quantiles using a qqplot.
qqnorm(z)
abline(0,1)
# Which of the following best describes the qq-plot comparing mouse weights to the normal distribution?

# The mouse weights are well approximated by the normal distribution, 
# although the larger values (right tail) are larger than predicted by the normal. 
# This is consistent with the differences seen between question 3 and 6.

## Central Limit Theorem Exercises #8 ##
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

## Central Limit Theorem Exercises #9 ##
# What is the standard deviation of the distribution of sample averages (use popsd())?
popsd(avgs)
# 0.827082

############################# CLT and t-distribution in Practice Exercises #############################
dat <- read.csv("femaleMiceWeights.csv")
head(dat)

## CLT and t-distribution in Practice Exercises #1 ##
# The CLT is a result from probability theory. Much of probability theory was originally inspired by gambling. 
# This theory is still used in practice by casinos. For example, they can estimate how many people 
# need to play slots for there to be a 99.9999% probability of earning enough money to cover expenses. 
# Let's try a simple example related to gambling.

# Suppose we are interested in the proportion of times we see a 6 when rolling n=100 dice. 
# This is a random variable which we can simulate with x=sample(1:6, n, replace=TRUE) and 
# the proportion we are interested in can be expressed as an average: mean(x==6). 
# Because the die rolls are independent, the CLT applies.

# We want to roll n dice 10,000 times and keep these proportions. This random variable (proportion of 6s) 
# has mean p=1/6 and variance p*(1-p)/n. So according to the CLT, 
# z = (mean(x==6) - p) / sqrt(p*(1-p)/n) should be normal with mean 0 and SD 1.

# Set the seed to 1, then use replicate() to perform the simulation, and 
# report what proportion of times z was larger than 2 in absolute value (CLT says it should be about 0.05).
set.seed(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(zs) > 2)
# 0.0431

## CLT and t-distribution in Practice Exercises #2 ##
# For the last simulation you can make a qqplot to confirm the normal approximation. 
# Now, the CLT is an asymptotic result, meaning it is closer and closer to being a perfect approximation 
# as the sample size increases. In practice, however, we need to decide if it is appropriate for 
# actual sample sizes. Is 10 enough? 15? 30?

# In the example used in exercise 1, the original data is binary (either 6 or not). 
# In this case, the success probability also affects the appropriateness of the CLT. 
# With very low probabilities, we need larger sample sizes for the CLT to "kick in".

# Run the simulation from exercise 1, but for different values of p and n. 
# For which of the following is the normal approximation best?
ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
  	x <- sample(1:sides,n,replace=TRUE)
  	(mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}
mypar()
# Normal approximation is best for p=0.5 and n=30

## CLT and t-distribution in Practice Exercises #3 ##
# As we have already seen, the CLT also applies to averages of quantitative data. 
# A major difference with binary data, for which we know the variance is p(1-p), 
# is that with quantitative data we need to estimate the population standard deviation.

# In several previous exercises we have illustrated statistical concepts with the 
# unrealistic situation of having access to the entire population. 
# In practice, we do *not* have access to entire populations. 
# Instead, we obtain one random sample and need to reach conclusions analyzing that data. 
# dat is an example of a typical simple dataset representing just one sample. 
# We have 12 measurements for each of two populations:
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

# We think of X as a random sample from the population of all mice in the control diet and 
# Y as a random sample from the population of all mice in the high fat diet.

# Define the parameter mu(X) as the average of the control population. 
# We estimate this parameter with the sample average X(bar). What is the sample average?
mean(X) # X(bar)

## CLT and t-distribution in Practice Exercises #4 ##
# We don't know  mu(X), but want to use X(bar) to understand mu(X). 
# Which of the following uses CLT to understand how well X(bar) approximates mu(X) ?

# X(bar) follows a normal distribution with mean mu(X) and standard deviation sigma(X)/sqroot(12) 
# where sigma(X) is the population standard deviation.


## CLT and t-distribution in Practice Exercises #5 ##
# The result above tells us the distribution of the following random variable: 
# Z=(X(bar) - mu(X)) / (sigma(X) / sqroot(12)). 
# What does the CLT tell us is the mean of Z (you don't need code)?
0

## CLT and t-distribution in Practice Exercises #6 ##
# The result of 4 and 5 tell us that we know the distribution of the difference between our estimate and 
# what we want to estimate, but don't know. However, the equation involves the 
# population standard deviation sigma(X), which we don't know.

# Given what we discussed, what is your estimate of sigma(X)?
# Hint: While the popsd() function from rafalib calculates population standard deviations, 
# the sd() function in base R calculates sample standard deviations.
sd(X)
# 3.022541

## CLT and t-distribution in Practice Exercises #7 ##
# Use the CLT to approximate the probability that our estimate X(bar) is off by more than 2 grams from mu(X).

# Null hypothesis = The difference is 2 gm
# Alternate hypothesis = The difference is not equal to 2 gm
# We need to prove that p < 0.05 to disprove the null hypothesis
2 * (1 - pnorm(2 / (sd(X) / sqrt(length(X)))))
# 0.02189533

## CLT and t-distribution in Practice Exercises #8 ##
# Now we introduce the concept of a null hypothesis. We don't know mu(X) nor mu(Y). 
# We want to quantify what the data say about the possibility that the diet has no effect: mu(X) = mu(Y). 
# If we use CLT, then we approximate the distribution of X(bar) as normal with 
# mean mu(X) and standard deviation sigma(X)/sqrt(M) and the distribution of Y(bar) as normal with 
# mean mu(Y) and standard deviation sigma(Y)/sqrt(N), with M and N the sample sizes for X and Y respectively, 
# in this case 12. This implies that the difference Y(bar)-X(bar) has mean 0. 
# We described that the standard deviation of this statistic (the standard error) is 
# SE (X(bar) - Y(bar)) = sqrt((sigma(X)**2 / 12) + (sigma(Y)**2 / 12))
# and that we estimate the population standard deviations sigma(X) and sigma(Y) with the sample estimates.

# What is the estimate of SE (X(bar) - Y(bar)) = sqrt((sigma(X)**2 / 12) + (sigma(Y)**2 / 12)) ?
sqrt( sd(X)^2/12 + sd(Y)^2/12 )
# or 
sqrt( var(X)/12 + var(Y)/12)

## CLT and t-distribution in Practice Exercises #9 ##
# So now we can compute Y(bar) - X(bar) as well as an estimate of this standard error and 
# construct a t-statistic. What number is this t-statistic?
(mean(X) - mean(Y)) / sqrt( sd(X)^2/12 + sd(Y)^2/12 )
# or
t.test(X, Y)$stat
# -2.055174

## CLT and t-distribution in Practice Exercises #10 ##
# The t-distribution is centered at 0 and has one parameter: the degrees of freedom, 
# that control the size of the tails. You will notice that if X follows a t-distribution 
# the probability that X is smaller than an extreme value such as 3 SDs away from the mean 
# grows with the degrees of freedom. For example, notice the difference between:
1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pnorm(3)
# If we apply the CLT, what is the distribution of this t-statisic?
# Normal with mean 0 and standard deviation 1

## CLT and t-distribution in Practice Exercises #11 ##
# Now we are ready to compute a p-value using the CLT. 
# What is the probability of observing a quantity as large as what we computed in 9, 
# when the null distribution is true?
Z <- (mean(Y) - mean(X)) / sqrt( sd(X)^2/12 + sd(Y)^2/12 )
2 * (1 - pnorm(Z))
# 0.0398622

## CLT and t-distribution in Practice Exercises #12 ##
# CLT provides an approximation for cases in which the sample size is large. 
# In practice, we can't check the assumption because we only get to see 1 outcome (which you computed above). 
# As a result, if this approximation is off, so is our p-value. 
# As described earlier, there is another approach that does not require a large sample size, 
# but rather that the distribution of the population is approximately normal. 
# We don't get to see this distribution so it is again an assumption, 
# although we can look at the distribution of the sample with qqnorm(X) and qqnorm(Y). 
# If we are willing to assume this, then it follows that the t-statistic follows the t-distribution.

# What is the p-value under the t-distribution approximation?
# Hint: use the t.test() function.
t.test(X,Y)$p.value
# 0.05299888

## CLT and t-distribution in Practice Exercises #13 ##
# With the CLT distribution, we obtained a p-value smaller than 0.05 and with the t-distribution, 
# one that is larger. They can't both be right. What best describes the difference?

# These are two different assumptions. 
# The t-distribution accounts for the variability introduced by the estimation of the standard error and thus, 
# under the null, large values are more probable under the null distribution.