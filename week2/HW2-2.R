# Install the gapminder data set using the devtools R-package.
library(devtools)
install_github("jennybc/gapminder")
# Load the downloaded library 
library(gapminder)
library(dplyr)
data(gapminder)
head(gapminder)

# What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?
lifeExp <- gapminder[gapminder$year==1952,][,4] 
mean(lifeExp <= 40)
# What is the proportion of countries in 1952 that have a life expectancy between 40 and 60 years?
mean(lifeExp <= 60 & lifeExp >= 40)

# Create a vector which gives the population sizes of the countries in 1952. Examine the histogram of
# these population sizes (it might help to increase the number of 'breaks').
popSize <- gapminder[gapminder$year==1952,][,"pop"] 
hist(popSize, breaks=30)
# Now examine the histogram of the log10 of these population sizes. We will try to find parameters for
# a Normal distribution which match the logarithm (base 10) of population sizes.
hist(log10(popSize), breaks=30)
# What is the standard deviation of the log10 of population size of the countries in 1952?
sd(log10(popSize))


# We want to further explore the log10 of the 1952 population sizes, and compare to a Normal distribution.
# Create a vector 'x' of the log10 of the 1952 population sizes.

x <- log10(popSize)
# Examine a Q-Q plot of 'z' against the Normal distribution using qqnorm(). You can add a diagonal 
# "identity line" with abline(0,1).
qqnorm(z)
abline(0,1)
# What is the z-score of the country with the largest population size? (you can use tail(sort(z),1) to 
# see the very last and largest value).
z <- (max(x)-mean(x))/sd(log10(popSize))


# Creata a function that returns a proportion of countries
F <- function(q) pnorm(q, mean=mean(x), sd=sd(x))
# Finally, using the Normal approximation, what is the expected number of countries that should have a
# log10 1952 population between 6 and 7 (i.e., between 1 million and 10 million people)? 
(F(7) - F(6)) * length(x)

qqnorm(x)
# What is the quantile of the standard normal distribution which matches to the smallest number in x 
# (the first element of sort(x))?
n = length(x)
# The sorted values of x represent n quantiles of the sample distribution spread from 0.0 to 1.0 (0.0
# and 1.0 are exclusive)
qs = (1:n - 0.5)/n
qnorm(qs)
# We can then construct our plot, as we have the quantiles of the sample distribution, and can plug 'qs' 
# into qnorm to get the matching sample quantiles of a normal distribution:
plot(qnorm(qs), sort(x))
