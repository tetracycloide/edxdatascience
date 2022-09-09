# installing the dslabs package
install.packages("dslabs")
# loading the dslabs package into the R session
library(dslabs)
install.packages("dslabs")  # to install a single package
install.packages("tidyverse", "dslabs") # to install two packages at the same time
installed.packages() # to see the list of all installed packages
library(tidyverse)
library(dslabs)
data(murders)

murders %>%
  ggplot(aes(population, total, label=abb, color=region)) +
  geom_label()

# assigning values to variables
a <-1
b <-1
c <--1

# solving the quadratic equation
(-b + sqrt(b^2 - 4*a*c))/(2*a)
(-b - sqrt(b^2 - 4*a*c))/(2*a)

# loading the dslabs package and the murders dataset
library(dslabs)
data(murders)

# determining that the murders dataset is of the "data frame" class
class(murders)
# finding out more about the structure of the object
str(murders)
# showing the first 6 lines of the dataset
head(murders)

# using the accessor operator to obtain the population column
murders$population
# displaying the variable names in the murders dataset
names(murders)
# determining how many entries are in a vector
pop <- murders$population
length(pop)
# vectors can be of class numeric and character
class(pop)
class(murders$state)

# logical vectors are either TRUE or FALSE
z <- 3 == 2
z
class(z)

# factors are another type of class
class(murders$region)
# obtaining the levels of a factor
levels(murders$region)

# SECTION 2
# We may create vectors of class numeric or character with the concatenate function
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")

# We can also name the elements of a numeric vector
# Note that the two lines of code below have the same result
codes <- c(italy = 380, canada = 124, egypt = 818)
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818)

# We can also name the elements of a numeric vector using the names() function
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country

# We can create sequences using seq
# seq(Start, End, Increment)
# integers increment by one has a short hand
seq(1,10)
seq(1, 10, 2)
1:10

# Using square brackets is useful for subsetting to access specific elements of a vector
codes[2]
codes[c(1,3)]
codes[1:2]

# If the entries of a vector are named, they may be accessed by referring to their name
codes["canada"]
codes[c("egypt","italy")]

# R coerced the data into characters. It guessed that because you put a character 
# string in the vector, you meant the 1 and 3 to actually be character strings, "1" and "3".
x <- c(1, "canada", 3)

#Sorting
library(dslabs)
data(murders)
sort(murders$total)

x <- c(31, 4, 15, 92, 65)
x
sort(x)    # puts elements in order

index <- order(x)    # returns index that will put x in order
x[index]    # rearranging by this index puts elements in order
order(x)

murders$state[1:10]
murders$abb[1:10]

index <- order(murders$total)
murders$abb[index]    # order abbreviations by total murders

max(murders$total)    # highest number of total murders
i_max <- which.max(murders$total)    # index with highest number of murders
murders$state[i_max]    # state name with highest number of total murders

x <- c(31, 4, 15, 92, 65)
x
rank(x)    # returns ranks (smallest to largest)

# Define a variable states to be the state names from the murders data frame
states <- murders$state

# Define a variable ranks to determine the population size ranks 
ranks <- rank(murders$population)

# Define a variable ind to store the indexes needed to order the population values
ind <- order(murders$population)

# Create a data frame my_df with the state name and its rank and ordered from least populous to most 
my_df <- data.frame(States = states[ind], ranks = ranks[ind])

# Using new dataset 
library(dslabs)
data(na_example)

# Checking the structure 
str(na_example)

# Find out the mean of the entire dataset 
mean(na_example)

# Use is.na to create a logical index ind that tells which entries are NA
is.na(na_example)

# Determine how many NA ind has using the sum function
sum(is.na(na_example))

# Note what we can do with the ! operator
x <- c(1, 2, 3)
ind <- c(FALSE, TRUE, FALSE)
x[!ind]

# Create the ind vector
library(dslabs)
data(na_example)
ind <- is.na(na_example)

# We saw that this gives an NA
mean(na_example)

# Compute the average, for entries of na_example that are not NA 
mean(na_example[!ind])

# The name of the state with the maximum population is found by doing the following
murders$state[which.max(murders$population)]

# how to obtain the murder rate
murder_rate <- murders$total / murders$population * 100000

# ordering the states by murder rate, in decreasing order
murders$state[order(murder_rate, decreasing=TRUE)]

# Assign city names to `city` 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

# Store temperature values in `temp`
temp <- c(35, 88, 42, 84, 81, 30)

# Convert temperature into Celsius and overwrite the original values of 'temp' with these Celsius values
temp <- 5/9 * (temp - 32)

# Create a data frame `city_temps` 
city_temps <- data.frame(City = city, Temperature = temp)

# Define an object `x` with the numbers 1 through 100
x <- 1:100
# Compute the sum 
sum(1/x^2)

# Load the data
library(dslabs)
data(murders)

# Store the per 100,000 murder rate for each state in murder_rate
murder_rate <- murders$total / murders$population * 100000

# Calculate the average murder rate in the US 
mean(murder_rate)

# defining murder rate as before
murder_rate <- murders$total / murders$population * 100000
# creating a logical vector that specifies if the murder rate in that state is less than or equal to 0.71
index <- murder_rate <= 0.71
# determining which states have murder rates less than or equal to 0.71
murders$state[index]
# calculating how many states have a murder rate less than or equal to 0.71
sum(index)

# creating the two logical vectors representing our conditions
west <- murders$region == "West"
safe <- murder_rate <= 1
# defining an index and identifying states with both conditions true
index <- safe & west
murders$state[index]

x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)    # returns indices that are TRUE

# to determine the murder rate in Massachusetts we may do the following
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]

# to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murder_rate[index]

x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x

# to see if Boston, Dakota, and Washington are states
c("Boston", "Dakota", "Washington") %in% murders$state

# Store the murder rate per 100,000 for each state, in `murder_rate`
murder_rate <- murders$total / murders$population * 100000
# Store the `murder_rate < 1` in `low` 
low <- murder_rate < 1
# Get the indices of entries that are below 1
which(low)
# Names of states with murder rates lower than 1
murders$state[low]
# Create a vector ind for states in the Northeast and with murder rates lower than 1. 
ind <- murders$region == "Northeast" & low
# Names of states in `ind` 
murders$state[ind]
# Compute the average murder rate using `mean` and store it in object named `avg`
avg <- mean(murder_rate)
# How many states have murder rates below avg ? Check using sum 
sum(murder_rate<avg)
# Store the 3 abbreviations in a vector called `abbs` (remember that they are character vectors and need quotes)
abbs <- c("AK", "MI", "IA")
# Match the abbs to the murders$abb and store in ind
ind <- match(abbs,murders$abb)
# Print state names from ind
murders$state[ind]
# Store the 5 abbreviations in `abbs`. (remember that they are character vectors)
abbs <- c("MA", "ME", "MI", "MO", "MU")
# Use the %in% command to check if the entries of abbs are abbreviations in the the murders data frame
abbs%in%murders$abb
# Use the `which` command and `!` operator to find out which index abbreviations are not actually part of the dataset and store in `ind`
ind <- which(!(abbs%in%murders$abb))
# Names of abbreviations in `ind`
abbs[ind]

# Section 3.2 Data Wrangling
# installing and loading the dplyr package
install.packages("dplyr")
library(dplyr)
# adding a column with mutate
library(dslabs)
data("murders")
murders <- mutate(murders, rate = total / population * 100000)
# subsetting with filter
filter(murders, rate <= 0.71)
# selecting columns with select
new_table <- select(murders, state, region, rate)
# using the pipe
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)
# creating a data frame with stringAsFactors = FALSE
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)
# Loading data
library(dslabs)
data(murders)
# Loading dplyr
library(dplyr)
# Redefine murders so that it includes a column named rate with the per 100,000 murder rates
murders <- mutate(murders, rate = total / population * 100000)
# Note that if you want ranks from highest to lowest you can take the negative and then compute the ranks 
x <- c(88, 100, 83, 92, 94)
rank(-x)
# Defining rate
rate <-  murders$total/ murders$population * 100000
# Redefine murders to include a column named rank
# with the ranks of rate from highest to lowest
murders <- mutate(murders, rank = rank(-rate))
# Use select to only show state names and abbreviations from murders
select(murders,state,abb)
# Filter to show the top 5 states with the highest murder rates
filter(murders, rank <= 5)
# Use filter to create a new data frame no_south
no_south <- filter(murders, region != "South")
# Use nrow() to calculate the number of rows
nrow(no_south)
# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw <- filter(murders, region %in% c("Northeast", "West"))
# Number of states (rows) in this category 
nrow(murders_nw)
# Create a table, call it my_states, that satisfies both the conditions 
my_states <- filter(murders, region %in% c("Northeast", "West") & rate < 1)
# Use select to show only the state name, the murder rate and the rank
select(my_states, state, rate, rank)
# show the result and only include the state, rate, and rank columns, all in one line, in that order
murders  %>% filter(region %in% c("Northeast", "West") & rate < 1) %>% select(state,rate,rank)
# Create new data frame called my_states (with specifications in the instructions)
my_states <- murders %>% mutate(rate =  total / population * 100000, rank = rank(-rate)) %>% filter(region %in% c("Northeast", "West") & rate < 1) %>% select(state,rate,rank)

#section 3.3 basic plots
library(dplyr)
library(dslabs)
data("murders")
# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)
# a histogram of murder rates
murders <- mutate(murders, rate = total / population * 100000)
hist(murders$rate)
# boxplots of murder rates by region
boxplot(rate~region, data = murders)
# Load the datasets and define some variables
library(dslabs)
data(murders)
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)
# Transform population (not population in millions) using the log10 transformation and save to object log10_population
log10_population <- log10(murders$population)
# Transform total gun murders using log10 transformation and save to object log10_total_gun_murders
log10_total_gun_murders <- log10(murders$total)
# Create a scatterplot with the log scale transformed population and murders 
plot(log10_population, log10_total_gun_murders)
# Create a histogram of this variable
hist(population_in_millions)
# Create a boxplot of state populations by region for the murders dataset
boxplot(population~region, data=murders)

#section 4 Programming Basics
# an example showing the general structure of an if-else statement
a <- 0
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}
# an example that tells us which states, if any, have a murder rate less than 0.5
library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind]) 
} else{
  print("No state has murder rate that low")
}
# changing the condition to < 0.25 changes the result
if(murder_rate[ind] < 0.25){
  print(murders$state[ind]) 
} else{
  print("No state has a murder rate that low.")
}
# the ifelse() function works similarly to an if-else conditional
a <- 0
ifelse(a > 0, 1/a, NA)
# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)
# the ifelse() function is also helpful for replacing missing values
data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))
# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)
# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}
# we see that the above function and the pre-built R mean() function are identical
x <- 1:100
identical(mean(x), avg(x))
# variables inside a function are not defined in the workspace
s <- 3
avg(1:10)
s
# the general form of a function
#my_function <- function(VARIABLE_NAME){
#  perform operations on VARIABLE_NAME and calculate VALUE
#  VALUE
#}
# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}
# creating a function that computes the sum of integers 1 through n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
# a very simple for-loop
for(i in 1:5){
  print(i)
}
# a for-loop for our summation
m <- 25
s_n <- vector(length = m) # create an empty vector
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}
# creating a plot for our summation function
n <- 1:m
plot(n, s_n)
# a table of values comparing our function to the summation formula
head(data.frame(s_n = s_n, formula = n*(n+1)/2))
# overlaying our function with the summation formula
plot(n, s_n)
lines(n, n*(n+1)/2)
# Assign the state abbreviation when the state name is longer than 8 characters 
new_names <- ifelse(nchar(murders$state) > 8, murders$abb, murders$state)
# Create function called `sum_n`
sum_n <- function(n){
  sum(1:n)
}
# Use the function to determine the sum of integers from 1 to 5000
sum_n(5000)
# Create `altman_plot` 
altman_plot <- function(x,y){
  plot(x+y,y-x)
}
# Run this code 
x <- 3
my_func <- function(y){
  x <- 5
  y+5
}
# Print the value of x 
print(x)
# Here is an example of a function that adds numbers from 1 to n
example_func <- function(n){
  x <- 1:n
  sum(x)
}
# Here is the sum of the first 100 numbers
example_func(100)
# Write a function compute_s_n with argument n that for any given n computes the sum of 1 + 2^2 + ...+ n^2
compute_s_n <- function(n){
  x <- (1:n)^2
  sum(x)
}
# Report the value of the sum when n=10
compute_s_n(10)
# Define a function and store it in `compute_s_n`
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}
# Create a vector for storing results
s_n <- vector("numeric", 25)
# write a for-loop to store the results in s_n
n <- 25
for(i in 1:n){
  s_n[i] <- compute_s_n(i)
}
# Define the vector of n
n <- 1:25
# Define the vector to store data
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}
#  Create the plot 
plot(n,s_n)
# Define the function
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}
# Define the vector of n
n <- 1:25
# Define the vector to store data
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}
# Check that s_n is identical to the formula given in the instructions.
identical(s_n,n*(n+1)*(2*n+1)/6)


# write a function to get the slope, standard error, and pvalue
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}
#take the tibble dat and add three new columns the coefficient, se, and p-value for the BB term
dat %>% 
  group_by(HR) %>% 
  do(get_slope(.))
#create a tibble with league id, HR, BB,and R
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 
#check if the relationship between HR and R varies by league
dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 
dat %>% 
  group_by(lgID) %>% 
  do(glance(lm(R ~ HR, data = .)))
dat %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR")
dat %>% 
  group_by(lgID) %>% 
  do(mod = lm(R ~ HR, data = .))
