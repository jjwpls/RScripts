library(MASS)
library(survey)
library(sampling)
library(ggplot2)

# This is motivated by Exercise 3.2 of Thomas Lumley's 2010 book: Complex Surveys.
# Considering the 2004 general election data as the universe, here I
#  
# (1) perform random simple surveys on the universe and plot the distributions of
# the sample means of the three candidates' votes (Bush, Kerry and Nader), and 
# (2) examine whether the sample means are close to the true population total,

rm(list = ls())

# Data from the "survey" package
data(election)

View(election)
dim(election)
# Each row represents the data of a county

# Summing up the votes across the three candidates and you get the total votes in each county, 
# which will be used to calcuate the probably of being selected for each county.
election$votes<-with(election, Bush+Kerry+Nader)

# First, we calculate the population totals
colSums(election[,c("Bush","Kerry","Nader")])

# Then, we define a function "samplemean()" to calcuate the sample means of the three candidates. 
# There are two function arguments, i and n, where
# 
# i = 1 if Bush, i = 2 if Kerry, and i = 3 if Nadar
# n = sample size of each draw
# 

samplemean = function(i, n) {
  
  # The probability of being selected
  election$p <- n * election$votes/sum(election$votes)
  
  # The UPtille() function performs simple random sampling
  insample <- UPtille(election$p) 
  
  #These are the counties that have been selected
  ppsample <- election[insample==1,] 
  
  # Sampling weights are the inverse of the probabilities
  ppsample$wt <- 1/ppsample$p
  
  # Declare survey design
  pps_design <- svydesign(id=~1, weight=~wt, data=ppsample)
  
  # a, b, and c are the sampling mean of the Bush, Kerry, and Nader, respectively
  a = svytotal(~Bush + Kerry + Nader, pps_design,deff=TRUE)[[1]]
  b = svytotal(~Bush + Kerry + Nader, pps_design,deff=TRUE)[[2]]
  c = svytotal(~Bush + Kerry + Nader, pps_design,deff=TRUE)[[3]]
  x = c(a,b,c)
  return(x[i])
}

# Test the function
samplemean(1,40)
# This sample shows Bush got 59612844 votes; the actual number is 59645156

samplemean(2,40)
# This sample show Kerry got 55799935 votes; the actual number is 56149771

# Draw from the universe 100 times, each draw with n = 50
simulation = replicate(100, samplemean(1,50))

# Plot the simulation
qplot(simulation,type="histogram")

# Calculate the standard error
sd(simulation)

# Compare the plot with the population total again
colSums(election[,4:6])

