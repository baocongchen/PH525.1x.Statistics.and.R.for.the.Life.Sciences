library(dplyr)
setwd("/home/thongtb/PH525.1x.Statistics.and.R.for.the.Life.Sciences")
data <- read.csv("msleep_ggplot2.csv")
class(data)
head(data)
dim(data)

# How many hours of total sleep are there for the first animal in the table?
data[1,"sleep_total"]

# Let's go ahead and make a plot of the brain weight and the total sleep, to see what the data look like:
plot(data$brainwt, data$sleep_total)
# Once more, with a logarithmic scale x-axis:
plot(data$brainwt, data$sleep_total, log="x")

# What is the 3rd quartile of the total sleep of all the animals?
fivenum(data$sleep_total)

# Subsetting a dataframe to the first two rows:
data[ c(1,2), ]

# What is the average total sleep, using the function mean() and vector subsetting, for the animals with total
# sleep greater than 18 hours?
data %>%
  select(sleep_total) %>%
    filter(sleep_total>18) %>%
      unlist() %>%
        mean()

# What is the row number of the animal which has more than 18 hours of total sleep and less than 3 hours of REM sleep?
which(data$sleep_total >  18 & data$sleep_rem < 3)

# What is the index of the animal (so the row number) with the least total sleep?
which(data$sleep_total == min(data$sleep_total)) # Or use the function below
order(data$sleep_total)

# What's the rank of the animal in the first row of the table in terms of total sleep?
data %>%
  select(sleep_total) %>%
    rank()
# What is the row number for "Cotton rat" in the "data" dataframe?
match(c("Cotton rat"), data$name) # Or use the function below
which(data$name == "Cotton rat")

# How many rodents (Rodentia) are in the table?
animal  <- data %>%
             select(order) 
table(animal)               

# What is the mean hours of total sleep of the rodents?
dat <- split(data$sleep_total,data$order)
mean(dat$Rodentia)

# What is the standard deviation of total hours of sleep for the Primates Order? (The standard deviation function is sd() in R.)
tapply(data$sleep_total,data$order,sd)

