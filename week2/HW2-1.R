# Load the data
dat = read.csv("femaleMiceWeights.csv.txt")
# Calculate the difference in weights of high fat diet and control 
mean(dat[13:24,2]) - mean(dat[1:12,2])
# Make a plot of these two group
s = split(dat[,2], dat[,1])
stripchart(s, vertical=TRUE, col=1:2)
# Add the means to the plot 
abline(h=sapply(s, mean), col=1:2)
# How many (what's the sum) of the high fat mice weigh less than the mean of the control mice (chow)?
sum(dat[13:24,2] < dat[1:12,2])
# How many (what's the sum) of the control mice weigh more than the mean of the high fat mice?
sum(dat[1:12,2] > mean(dat[13:24,2]))
# The proportion of high fat diet mice over 30 is the sum of high fat diet mice over 30 divided by the 
# number of high fat diet mice, in other words, the mean of a vector of 1s and 0s. What is the proportion
# of high fat diet mice over 30?
highfat = s[["hf"]]
as.numeric(highfat > 30)
3/12
########################################
population = read.csv("femaleControlsPopulation.csv.txt")
# What's the control population mean?
mean(population[,1])

# RANDOM SAMPLES
# Calculate the difference between two random samples of 12 from the control mice
null = replicate(10000, mean(sample(population[,1], 12)) - mean(sample(population[,1], 12)))
head(null)
plot(null)
########################################
# Visualize the data 
hist(null)
diff = mean(dat[13:24,2]) - mean(dat[1:12,2])
abline(v=diff, col="red")
abline(v=-diff, col="red")
# What is the one-tailed probability of seeing as big a difference as we observed, calculated from your 
# null distribution?
mean(null > abs(diff))
# What is the two-tailed probability of seeing as big a difference as we observed, calculated from your 
# null distribution?
mean(null > abs(diff))*2