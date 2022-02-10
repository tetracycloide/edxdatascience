library(tidyverse)
library(dslabs)
take_poll(25)    # draw 25 beads

# `N` represents the number of people polled
N <- 25
# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p = seq(0,1,length=100)
# Create a variable `se` that contains the standard error of each sample average
se = sqrt(p*(1-p)/N)
# Plot `p` on the x-axis and `se` on the y-axis
plot(p,se)
# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)
# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)
# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.
for(N in sample_sizes)
{se = sqrt(p*(1-p)/N)
plot(p,se, ylim = c(0,0.1))}
# `N` represents the number of people polled
N <- 25
# `p` represents the proportion of Democratic voters
p <- 0.45
# Calculate the standard error of the spread. Print this value to the console.
2*sqrt(p*(1-p)/N)
# Code: Computing the probability of X-bar being within .01 of p
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)
#Code: Monte Carlo simulation using a set value of p
p <- 0.45    # unknown p to estimate
N <- 1000
# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)
# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})
#Code: Histogram and QQ-plot of Monte Carlo results
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)
#calculate the spread and SE of the spread
2*(p-1)
2*sqrt(p*(1-p)/N)
#Code: Plotting margin of error in an extremely large poll over a range of values of p
library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()

# Write a function called `take_sample` that takes `p` and `N` as arguments and returns the average value of a randomly sampled population.
take_sample <- function(p,N)
{
  sample_taken <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(sample_taken)
}
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45
# Define `N` as the number of people polled
N <- 100
# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p,N)
# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000
# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B, p-take_sample(p,N))
# Calculate the mean of the errors. Print this value to the console.
mean(errors)
# Calculate the mean of the absolute value of each simulated error. Print this value to the console.
mean(abs(errors))
# Calculate the standard deviation of `errors`
sqrt(mean(errors^2))
# Calculate the standard error
sqrt(p*(1-p)/N)
# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
# Define `X_bar` as the average sampled proportion
X_bar <- mean(X)
# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar*(1-X_bar)/N)
# Create a plot of the largest standard error for  ranging from 100 to 5,000.
N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N,se)
# Generate a qq-plot of `errors` with a qq-line showing a normal distribution
qqnorm(errors)
qqline(errors)
# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.
1-pnorm((.5-p)/(sqrt(p*(1-p)/N)))
# Define `N` as the number of people polled
N <-100
# Define `X_hat` as the sample average
X_hat <- 0.51
# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)
# Calculate the probability that the error is 0.01 or larger
1-pnorm(0.01/se_hat) + pnorm(-0.01/se_hat)
#95% confidance interval
pnorm(qnorm(1-0.995))
#Code: geom_smooth confidence interval example
#The shaded area around the curve is related to the concept of confidence intervals.
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")
#Code: Monte Carlo simulation of confidence intervals
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean
#Code: Solving for  with qnorm
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval
#Code: Monte Carlo simulation
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)
#Code: Confidence interval for the spread with sample size of 25
#Note that to compute the exact 95% confidence interval, we would use c(-qnorm(.975), qnorm(.975)) instead of 1.96.
N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)
#Code: Computing a p-value for observed spread of 0.02
N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))
# Load the data
data(polls_us_election_2016)
# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls <- filter(polls_us_election_2016, enddate >= "2016-10-31" & state=="U.S.")
# How many rows does `polls` contain? Print this value to the console.
nrow(polls)
# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls[1,"samplesize"]
N
# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat <- polls[1,"rawpoll_clinton"]/100
X_hat
# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(X_hat - qnorm(.975)*se_hat, X_hat + qnorm(.975)*se_hat)
# The `polls` object that filtered all the data by date and nation has already been loaded. Examine it using the `head` function.
head(polls)
# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
pollster_results <- polls %>%
  mutate(X_hat = rawpoll_clinton/100,
         se_hat = sqrt(X_hat*(1-X_hat)/samplesize),
         lower = X_hat - qnorm(.975)*se_hat,
         upper = X_hat + qnorm(.975)*se_hat) %>% 
  select(pollster,enddate, X_hat,se_hat,lower,upper)
# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)
# Add a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% mutate(hit = 0.482 > lower & 0.482 < upper) %>% summarize(avg_hit = mean(hit))
# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>% mutate (d_hat = rawpoll_clinton/100-rawpoll_trump/100)
# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls[1,"samplesize"]
N
# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- polls[1,"d_hat"]
d_hat
# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat + 1)/2
# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(d_hat - qnorm(.975)*se_hat, d_hat + qnorm(.975)*se_hat)
# Create a new object called `pollster_results` that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
pollster_results <- polls %>%
  mutate(X_hat = (d_hat + 1)/2,
         se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize),
         lower = d_hat - qnorm(.975)*se_hat,
         upper = d_hat + qnorm(.975)*se_hat) %>% 
  select(pollster, enddate, d_hat, lower, upper)
# Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% mutate(hit = 0.021 > lower & 0.021 < upper) %>% summarize(avg_hit = mean(hit))
# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster.
polls %>% mutate(error = d_hat - 0.021) %>% ggplot(aes(x = error, y = pollster)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster, but only for pollsters who took 5 or more polls.
polls %>% mutate(error = d_hat - 0.021) %>% 
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  ggplot(aes(x = error, y = pollster)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Code: Simulating polls
#Note that to compute the exact 95% confidence interval, we would use 
#qnorm(.975)*SE_hat instead of 2*SE_hat.
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2
# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})
# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls
#Code: Generating simulated poll data
library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)
# keep only national polls from week before election with a grade considered reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))
# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
# compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat
# compute margin of error
p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
# histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)
#Code: Investigating poll data and pollster bias
# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())
# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975) instead of 1.96.
# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()
# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)
# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)

# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)
# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height
# Calculate the population average. Print this value to the console.
mean(x)
# Calculate the population standard deviation. Print this value to the console.
sd(x)
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `N` as the number of people measured
N <- 50
# Define `X` as a random sample from our population `x`
X <- sample(x, size=N, replace=TRUE)
# Calculate the sample average. Print this value to the console.
mean(X)
# Calculate the sample standard deviation. Print this value to the console.
sd(X)
# Define `se` as the standard error of the estimate. Print this value to the console.
se <- sd(X)/sqrt(N)
# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(mean(X)-qnorm(.975)*se,mean(X)+qnorm(.975)*se)
# Define `mu` as the population average
mu <- mean(x)
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `N` as the number of people measured
N <- 50
# Define `B` as the number of times to run the model
B <- 10000
# Define an object `res` that contains a logical vector for simulated intervals that contain mu
res <- replicate(B, {
  X <- sample(x, size=N, replace=TRUE)
  se <- sd(X)/sqrt(N)
  interval <- c(mean(X)-qnorm(.975)*se,mean(X)+qnorm(.975)*se)
  between(mu,interval[1],interval[2])    
})
# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)
# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")
# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 
# Make a boxplot with points of the spread for each pollster
polls %>% ggplot(aes(pollster, spread)) +
  geom_boxplot() +
  geom_point()
# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma <- polls %>% 
  group_by(pollster) %>% 
  summarize(s = sd(spread))
# Print the contents of sigma to the console
sigma
# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% 
  group_by(pollster) %>% 
  summarize(average = mean(spread), s = sd(spread), N = n()) 
res
# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate <- res$avg[2] - res$avg[1]
estimate
# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
se_hat
# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(estimate-qnorm(.975)*se_hat,estimate+qnorm(.975)*se_hat)
ci
# Calculate the p-value for expected difference of 0 of a random varaible larger than estimate
2*(1-pnorm(estimate, 0, se_hat))
# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()
# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.
var <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread))
var

#Code: Monte Carlo simulation Bayes' Theorem
prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))
N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy
# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))
table(outcome, test)
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500
# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100
# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2
# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000
# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50
# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
Pr_AB <- Pr_BA*Pr_A/Pr_B
Pr_A
# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)
# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
# Examine the `polls` object using the `head` function
head(polls)
# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
results <- polls %>% summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread)))
results
# Define `mu` and `tau`
mu <- 0
tau <- 0.01
# Define a variable called `sigma` that contains the standard error in the object `results`
sigma <- results$se
# Define a variable called `Y` that contains the average in the object `results`
Y <- results$avg
# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B <- sigma^2/(sigma^2 + tau^2)
# Calculate the expected value of the posterior distribution
mu+(1-B)*Y
# Compute the standard error of the posterior distribution. Print this value to the console.
sqrt(1/(1/sigma^2+1/tau^2))
# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(mu+(1-B)*Y - qnorm(.975)*se, mu+(1-B)*Y + qnorm(.975)*se)
# Assign the expected value of the posterior distribution to the variable `exp_value`
exp_value <- B*mu + (1-B)*Y 
# Assign the standard error of the posterior distribution to the variable `se`
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0, exp_value, se)
# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg
# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)
# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc <- function(tau){
  B <- sigma^2 / (sigma^2 + tau^2)
  pnorm(0, B*mu + (1-B)*Y, sqrt( 1/ (1/sigma^2 + 1/tau^2)))
}
# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps <- p_calc(taus)
# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus, ps)





# suggested libraries and options
library(tidyverse)
options(digits = 3)
# load brexit_polls object
library(dslabs)
data(brexit_polls)
#Question 1: Expected value and standard error of a poll
N=1500
p=0.481
#expected number of remain
N*p
#SE of total number of remains
sqrt(N*p*(1-p))
#expected proportion of remain
p
#SE of proportion of remain in sample
sqrt(p*(1-p)/N)
#expected spread
2*p-1
#SE of spread
2*sqrt(p*(1-p)/N)
#Question 2: Actual Brexit poll estimates
#Calculate x_hat for each poll
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)
mean(brexit_polls.spread)
#What is the average of the observed spreads (spread)?
mean(brexit_polls$spread)
#What is the standard deviation of the observed spreads?
sd(brexit_polls$spread)
#What is the average of x_hat, the estimates of the parameter ?
mean(brexit_polls$x_hat)
#What is the standard deviation of x_hat?
sd(brexit_polls$x_hat)
#for brexit_polls[1,] calculate 95% confidance interval
x_hat <- brexit_polls[1,"x_hat"]
N <- brexit_polls[1,"samplesize"]
se_hat <- sqrt(x_hat*(1-x_hat)/N)
c(x_hat - qnorm(.975)*se_hat, x_hat + qnorm(.975)*se_hat)
#Does the 95% confidence interval predict a winner (does not cover )? 
!between(0.5, x_hat - qnorm(.975)*se_hat, x_hat + qnorm(.975)*se_hat)    # predicts winner
#Does the 95% confidence interval cover the true value of  observed during the referendum?
between(0.481, x_hat - qnorm(.975)*se_hat, x_hat + qnorm(.975)*se_hat)    # does not cover p
#Create the data frame june_polls containing only Brexit polls ending in June 2016 (enddate of "2016-06-01" and later). 
#We will calculate confidence intervals for all polls and determine how many cover the true value of d.
#First, use mutate() to calculate a plug-in estimate se_x_hat for the standard error of the estimate  for each poll given its sample size and value of  (x_hat).
#Second, use mutate() to calculate an estimate for the standard error of the spread for each poll given the value of se_x_hat. 
#Then, use mutate() to calculate upper and lower bounds for 95% confidence intervals of the spread.
#Last, add a column hit that indicates whether the confidence interval for each poll covers the correct spread
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)
p <- 0.481
june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01")
june_polls <- june_polls %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread = 2*se_x_hat,
         ci_lower_spread = spread - qnorm(0.975)*se_spread,
         ci_upper_spread = spread + qnorm(0.975)*se_spread,
         hit = (2*p-1 > ci_lower_spread) & (2*p-1 < ci_upper_spread))
#How many polls are in june_polls?
nrow(june_polls)
#What proportion of polls have a confidence interval that covers the value 0?
mean(june_polls$ci_lower_spread < 0 & june_polls$ci_upper_spread > 0)
#What proportion of polls predict "Remain" (confidence interval entirely above 0)?
mean(june_polls$ci_lower_spread > 0)
#What proportion of polls have a confidence interval covering the true value of d?
mean(june_polls$hit)
#Group and summarize the june_polls object by pollster to find the proportion of hits for each pollster and the number of polls per pollster. Use arrange() to sort by hit rate.
june_polls %>% group_by(pollster) %>% summarize(count = n(), hit_rate = mean(hit)) %>% arrange(hit_rate)
#Make a boxplot of the spread in june_polls by poll type.
june_polls %>% ggplot(aes(poll_type, spread)) +
  geom_boxplot()
# calculate confidance intervals by poll type
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
            se_spread = 2*sqrt(p_hat*(1-p_hat)/N),
            spread_lower = spread - qnorm(.975)*se_spread,
            spread_upper = spread + qnorm(.975)*se_spread)
# lower 95% confidance interval for online voters
combined_by_type %>%
  filter(poll_type == "Online") %>%
  pull(spread_lower)
#upper bound 95%
combined_by_type %>%
  filter(poll_type == "Online") %>%
  pull(spread_upper)
