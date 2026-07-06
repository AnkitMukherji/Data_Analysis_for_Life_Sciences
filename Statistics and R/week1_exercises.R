## Exercise #1 ##
# What version of R are you using?
version
# 4.5.2 (Answer is old in the course. Given: 4.2.0)

## Exercise #2 ##
# Create a numeric vector containing the numbers 2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23.
# What is the average of these numbers?

v <- c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)
mean(v)
# 7.794286

## Exercise #3 ##
# Use a for loop to determine the value of sum(i=1 to i=25) i**2
s = 0
for (i in seq(1, 25)) {
  s = s + i ** 2
}
print(s)
# 5525

## Exercise #4 ##
# The cars dataset is available in base R. You can type cars to see it. Use the class() function to determine what type of object is cars.
class(cars)
# data.frame

## Exercise #5 ##
# How many rows does the cars object have?
nrow(cars)
# 50

## Exercise #6 ##
# What is the name of the second column of cars?
colnames(cars)[2]
# or
names(cars)[2]
# dist

## Exercise #7 ##
# The simplest way to extract the columns of a matrix or data.frame is using [. For example you can access the second column with cars[,2].

# What is the average distance traveled in this dataset?
mean(cars[,2])
# 42.98

## Exercise #8 ##
# Familiarize yourself with the which() function. Which row of cars has a a distance of 85?
which(cars[,2] == 85)
# 50

## Getting Started Exercises #1 ##
# Read in the file femaleMiceWeights.csv and report the exact name of the column containing the weights.
dat <- read.csv("femaleMiceWeights.csv")
head(dat)
names(dat)[2]
# Bodyweight

## Getting Started Exercises #2 ##
# The [ and ] symbols can be used to extract specific rows and specific columns of the table.
# What is the entry in the 12th row and second column?
dat[12, 2]
# 26.25

## Getting Started Exercises #3 ##
# You should have learned how to use the $ character to extract a column from a table and return it as a vector. 
# Use $ to extract the weight column and report the weight of the mouse in the 11th row.
dat$Bodyweight[11]
# 26.91

## Getting Started Exercises #4 ##
# The length() function returns the number of elements in a vector.
# How many mice are included in our dataset?
nrow(dat)
length(dat$Bodyweight)
# 24

## Getting Started Exercises #5 ##
# To create a vector with the numbers 3 to 7, we can use seq(3,7) or, because they are consecutive, 3:7. 
# View the data and determine what rows are associated with the high fat or hf diet. 
# Then use the mean() function to compute the average weight of these mice.

# What is the average weight of mice on the high fat diet?
unique(dat$Diet)
mean(dat$Bodyweight[dat$Diet == "hf"])
# 26.83

## Getting Started Exercises #6 ##
# One of the functions we will be using often is sample(). Read the help file for sample() using ?sample. 
# Now take a random sample of size 1 from the numbers 13 to 24 and report back the weight of the mouse represented by that row. 
# Make sure to type set.seed(1) to ensure that everybody gets the same answer.
set.seed(1)
dat$Bodyweight[sample(13:24, 1)]
# 34.02

###################################### dplyr Exercises #####################################
library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)

## dplyr Exercises #1 ##
# Read in the msleep_ggplot2.csv file with the function read.csv() and 
# use the function class() to determine what type of object is returned.
df <- read.csv("msleep_ggplot2.csv")
class(df)
# data.frame

## dplyr Exercises #2 ##
# Now use the filter() function to select only the primates.
# How many animals in the table are primates?
df |> filter(order == "Primates") |> nrow()
# 12

## dplyr Exercises #3 ##
# What is the class of the object you obtain after subsetting the table to only include primates?
class(df |> filter(order == "Primates"))
# data.frame

## dplyr Exercises #4 ##
# Now use the select() function to extract the sleep (total) for the primates.
# What class is this object?
class(df |> filter(order == "Primates") |> select(sleep_total))
# data.frame

## dplyr Exercises #5 ##
# Now we want to calculate the average amount of sleep for primates 
# (the average of the numbers computed above). 
# One challenge is that the mean() function requires a vector so, 
# if we simply apply it to the output above, we get an error. 
# Look at the help file for unlist() and use it to compute the desired average.

# What is the average amount of sleep for primates?
df |> filter(order == "Primates") |> select(sleep_total) |> unlist() |> mean()
# 10.5

## dplyr Exercises #6 ##
# For the last exercise, we could also use the dplyr summarize() function. 
# We have not introduced this function, but you can read the help file and repeat exercise 5, 
# this time using just filter() and summarize() to get the answer.

# What is the average amount of sleep for primates calculated by summarize()
df |> filter(order == "Primates") |> summarise(mean(sleep_total))

###################################### QQ-plot Exercises ###################################

url <- "https://courses.edx.org/c4x/HarvardX/PH525.1x/asset/skew.RData"
filename <- basename(url)
download(url, filename)

load("skew.RData")
dim(dat)

# Using QQ-plots, compare the distribution of each column of the matrix to a normal.
par(mfrow = c(3,3))
for (i in 1:9) {
  qqnorm(dat[,i])
  qqline(dat[,i])
}
par(mfrow=c(1,1))

# Identify the two columns which are skewed.
# 4 = Positive skew (looks like an up-shaping curve)
# 9 = Negative skew (looks like a down-shaping curve)

# Examine each of these two columns using a histogram.
par(mfrow=c(1,2))
for (i in c(4, 9)) {
  hist(dat[,i])
}
par(mfrow=c(1,1))

# Positive skew = Histogram shows a long tail to the right (towards larger values)
# Negative skew = Histogram shows a long tail to the left (towards smaller values)

## QQ-plot Exercises #1 ##
# Which column has positive skew (a long tail to the right)?
# 4

## QQ-plot Exercises #2 ##
# Which column has negative skew (a long tail to the left)?
# 9

######################################## Boxplot Exercises ##################################
head(InsectSprays)

# Boxplot using split
boxplot(split(InsectSprays$count, InsectSprays$spray))

# Boxplot using formula
boxplot(InsectSprays$count ~ InsectSprays$spray)

## Boxplot Exercises #3 ##
# Let's consider a random sample of finishers from the New York City Marathon in 2002. 
# This dataset can be found in the UsingR package. 
# Load the library and then load the nym.2002 dataset.
library(dplyr)
data(nym.2002, package="UsingR")

# Use boxplots and histograms to compare the finishing times of males and females.
mypar(1,3)
males <- filter(nym.2002, gender=="Male") %>% select(time) %>% unlist
females <- filter(nym.2002, gender=="Female") %>% select(time) %>% unlist
boxplot(females, males)
hist(females,xlim=c(range( nym.2002$time)))
hist(males,xlim=c(range( nym.2002$time)))
mypar(1,1)
# Male and females have similar right skewed distributions with the former, 
# 20 minutes shifted to the left.
