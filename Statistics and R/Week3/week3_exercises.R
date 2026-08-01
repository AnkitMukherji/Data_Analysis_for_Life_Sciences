setwd("Statistics and R/Week3/")

##################################### T-test Exercises #########################################
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

# This is a large dataset (1,236 cases), and we will pretend that it contains the entire population 
# in which we are interested. We will study the differences in birth weight between babies born to 
# smoking and non-smoking mothers.

# First, let's split this into two birth weight datasets: one of birth weights to non-smoking mothers 
# and the other of birth weights to smoking mothers.
library(tidyverse)
bwt.nonsmoke <- filter(babies, smoke==0) |> select(bwt) |> unlist()
bwt.smoke <- filter(babies, smoke==1) |> select(bwt) |> unlist()

# Now, we can look for the true population difference in means between smoking and non-smoking birth weights.
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

# The population difference of mean birth weights is about 8.9 ounces. 
# The standard deviations of the nonsmoking and smoking groups are about 17.4 and 18.1 ounces, respectively.

# As we did with the mouse weight data, this assessment interactively reviews inference concepts 
# using simulations in R. We will treat the babies dataset as the full population and draw samples 
# from it to simulate individual experiments. We will then ask whether somebody who only received 
# the random samples would be able to draw correct conclusions about the population.

# We are interested in testing whether the birth weights of babies born to non-smoking mothers are 
# significantly different from the birth weights of babies born to smoking mothers.

## T-test Exercises #1 ##

# Set the seed at 1 and obtain samples from the non-smoking mothers (dat.ns) of size N=25. 
# Then, without resetting the seed, take a sample of the same size from smoking mothers (dat.s). 
# Compute the t-statistic (call it tval).
# What is the absolute value of the t-statistic?
set.seed(1)
dat.ns <- sample(bwt.nonsmoke, 25)
dat.s <- sample(bwt.smoke, 25)
test <- t.test(dat.ns, dat.s)
tval <- test$statistic
# or
N = 25
X.ns <- mean(dat.ns)
sd.ns <- sd(dat.ns)

X.s <- mean(dat.s)
sd.s <- sd(dat.s)

sd.diff <- sqrt(sd.ns^2/N+sd.s^2/N)
tval <- (X.s - X.ns)/sd.diff
abs(tval)
# 1.659325

## T-test Exercises #2 ##
# Recall that we summarize our data using a t-statistics because we know that in situations 
# where the null hypothesis is true (what we mean when we say "under the null") and 
# the sample size is relatively large, this t-value will have an approximate standard normal distribution. 
# Because we know the distribution of the t-value under the null, we can quantitatively determine how 
# unusual the observed t-value would be if the null hypothesis were true.

# The standard procedure is to examine the probability a t-statistic that actually does follow the 
# null hypothesis would have larger absolute value than the absolute value of the t-value we just 
# observed -- this is called a two-sided test.

# We have computed these by taking one minus the area under the standard normal curve between -abs(tval) 
# and abs(tval). In R, we can do this by using the pnorm() function, which computes the area under a 
# normal curve from negative infinity up to the value given as its first argument:
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))

# What is the estimated p-value?
0.09705034
# By reporting only p-values, many scientific publications provide an incomplete story of their findings. 
# As we have mentioned, with very large sample sizes, scientifically insignificant differences between 
# two groups can lead to small p-values. Confidence intervals are more informative as they include the 
# estimate itself. We will learn how to compute these in the next module.