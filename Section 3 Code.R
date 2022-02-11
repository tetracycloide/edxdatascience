# Pr (A) = probability of event A

#Monte Carlo simulation to approximate probability
#The sample() function draws random outcomes from a set of options.
#The replicate() function repeats lines of code a set number of times. 
#It is used with sample() and similar functions to run Monte Carlo simulations.

beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

#If you are running R 3.6, you can revert to the original seed setting behavior 
#by adding the argument sample.kind="Rounding". For example:
set.seed(1)
set.seed(1, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5

#Suppose you have the vector beads from a previous video:
beads <- rep(c("red", "blue"), times = c(2,3))
beads
#[1] "red" "red" "blue" "blue" "blue"
#To find the probability of drawing a blue bead at random, you can run:
mean(beads == "blue")
#[1] 0.6

#Conditional probabilities compute the probability that an event occurs given 
#information about dependent events. For example, the probability of drawing a second king given that the first draw is a king is:
#Pr(Card 2 is a king|Card 1 is a King) = 3/51
#If two events  and  are independent, Pr (A|B) = Pr(A).
#To determine the probability of multiple events occurring, we use the multiplication rule.

#Equations
#The multiplication rule for independent events is:
#Pr (A and B and C) = Pr(A)xPr(B)xPr(C)  
#The multiplication rule for dependent events considers the conditional probability of both events occurring:
# Pr (A and B) = Pr(A)xPr(B|A)  
#We can expand the multiplication rule for dependent events to more than 2 events:
# Pr (A and B and C) = Pr(A) x Pr(B|A) x Pr(C|A and B)

#Exercise
cyan <- 3
magenta <- 5
yellow <- 7

# Assign a variable `p` as the probability of choosing a cyan ball from the box
p_1 <- cyan / (cyan + magenta + yellow)

# Print the variable `p` to the console
p
# `p` is defined as the probability of choosing a cyan ball from a box containing: 3 cyan balls, 5 magenta balls, and 7 yellow balls.
# Using variable `p`, calculate the probability of choosing any ball that is not cyan from the box
1-p
# Assign a variable `p_2` as the probability of not choosing a cyan ball on the second draw without replacement.after one cyan ball has been removed 
p_2 <- 1 - ((cyan -1)  / (cyan  + magenta + yellow -1))
# Calculate the probability that the first draw is cyan and the second draw is not cyan using `p_1` and `p_2`.
p_1*p_2
# Assign a variable 'p_2' as the probability of not choosing a cyan ball on the second draw with replacement.
p_2 <- 1 - ((cyan)  / (cyan  + magenta + yellow ))
# Calculate the probability that the first draw is cyan and the second draw is not cyan using `p_1` and `p_2`.
p_1*p_2

#Code: Introducing paste() and expand.grid()
# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))
#Code: Generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)
#Code: Permutations and combinations
#Correction: The code shown does not generate all 7 digit phone numbers because phone numbers can have repeated digits. It generates all possible 7 digit numbers without repeats.

library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter
#Code: Probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)
#Code: Probability of a natural 21 in blackjack
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))
#Code: Monte Carlo simulation of natural 21 in blackjack
#Note that your exact values will differ because the process is random and the seed is not set.

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

#Code: The birthday problem
# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays

#Code: Function for birthday problem Monte Carlo simulations
#Note that the function body of compute_prob() is the code that we wrote in the previous video. If we write this code as a function, we can use sapply() to apply this function to several values of n.

# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)
#Code: Element-wise operation over vectors and sapply
x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)
#Code: Computing birthday problem probabilities with sapply
# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob

#Code: Estimating a practical value of B
#This code runs Monte Carlo simulations to estimate the probability of shared birthdays using several B values and plots the results. When B is large enough that the estimated probability stays stable, then we have selected a useful value of B.

B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 

cyan <- 3
magenta <- 5
yellow <- 7

# Assign the variable 'p_yellow' as the probability that a yellow ball is drawn from the box.
p_yellow <- yellow / (cyan + magenta + yellow)
# Using the variable 'p_yellow', calculate the probability of drawing a yellow ball on the sixth draw. Print this value to the console.
p_yello
# Assign the variable 'p_no6' as the probability of not seeing a 6 on a single roll.
p_no6 <- 5/6

# Calculate the probability of not seeing a 6 on six rolls using `p_no6`. Print your result to the console: do not assign it to a variable.
p_no6^6
# Assign the variable `p_cavs_win4` as the probability that the Cavs will win the first four games of the series.
#The cavs have a 60% chance of winning each game
p_cavs_win4 <- .6^4

# Using the variable `p_cavs_win4`, calculate the probability that the Celtics win at least one game in the first four games of the series.
1-p_cavs_win4

# This line of example code simulates four independent random games where the Celtics either lose or win. Copy this example code to use within the `replicate` function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `celtic_wins` that replicates two steps for B iterations: (1) generating a random four-game series `simulated_games` using the example code, then (2) determining whether the simulated series contains at least one win for the Celtics.
celtic_wins <- replicate(B,{
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games=="win")})

# Calculate the frequency out of B iterations that the Celtics won at least one game. Print your answer to the console.
mean(celtic_wins)

# Probability of A or B is the probability of A plus probability of B minue probability of A and B
# Pr (A or B) = Pr (A) + Pr (B) -Pr (A and B)
#Monty Hall Monte Carlo
#Code: Monte Carlo simulation of stick strategy
B <- 10000 #10,000 replicates
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking
#Code: Monte Carlo simulation of switch strategy
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching

#7 game series, first game a loss 50:50 odds.  Chances to win the series
# Assign a variable 'n' as the number of remaining games.
n <- 7-1

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes <-c(0,1)

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`.
l <- rep(list(outcomes), n)

# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities <- expand.grid(l)

# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
results <- rowSums(possibilities)

# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results > 3)

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `results` that replicates for `B` iterations a simulated series and determines whether that series contains at least four wins for the Cavs.
results <- replicate(B, sum(sample(c(0,1),6,replace=TRUE)))

# Calculate the frequency out of `B` iterations that the Cavs won at least four games in the remainder of the series. Print your answer to the console.
mean(results > 3)
# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win. Call this object 'Pr'.
Pr <- sapply(p, prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p, Pr)
# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}

# Assign the variable 'N' as the vector of series lengths. Use only odd numbers ranging from 1 to 25 games.
N <- seq(1,25,2)

# Apply the 'prob_win' function across the vector of series lengths to determine the probability that team B will win. Call this object `Pr`.
Pr <- sapply(N, prob_win)

# Plot the number of games in the series 'N' on the x-axis and 'Pr' on the y-axis.
plot(N, Pr)

#Assessment probability
library(gtools)
library(tidyverse)

#run a monte carlo for the outcome of a race starting with these runners for all
#medalists to be Jamaican
B <- 10000
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
results <- replicate(B, {
  medalists <- sample(runners,3)
  medalists[1] == "Jamaica"&medalists[3] == "Jamaica"&medalists[2] == "Jamaica"
})

#Use the information below to answer the following five questions.
#A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day of the year. He doesn't think his current special actually allows that number of choices, but wants to change his special if needed to allow at least 365 choices.
#A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from a list of 6 options, a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.
#How many meal combinations are possible with the current menu?
6 * nrow(combinations(6,2)) * 2
#How many combinations are possible if he expands his original special to 3 drink options?
6 * nrow(combinations(6,2)) * 3
#How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
6 * nrow(combinations(6,3)) * 3
#Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.
#Use sapply() to apply the function to entree option counts ranging from 1 to 12.
meal_com <- function(n) {
  n * nrow(combinations(6,2)) * 3
}
sapply(1:12,meal_com)
#What is the minimum number of entree options required in order to generate more than 365 combinations?
data.frame(entrees = 1:12, combos = sapply(1:12,meal_com)) %>% filter(combos >365) %>% min(.$entrees)
#Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 3 drink choices, and a selection of 2 sides from the specified number of side choices.
#Use sapply() to apply the function to side counts ranging from 2 to 12.
meal_com <- function(n) {
  6 * nrow(combinations(n,2)) * 3
}
sapply(2:12,meal_com)
#What is the minimum number of side options required in order to generate more than 365 combinations?
data.frame(sides = 2:12, combos = sapply(2:12,meal_com)) %>% filter(combos >365) %>% min(.$sides)

#You will be using this dataset to answer the following four multi-part questions
head(esoph)
#The study compares 
#people with esophageal cancer (cases, counted in ncases)
#
#alcohol intake in grams per day (alcgp) and 
#tobacco intake in grams per day (tobgp) 
#across cases and controls grouped by age range (agegp).
#How many groups are in the study?
nrow(esoph)
#How many cases are there? Save this value as all_controls for later problems.
all_cases <- sum(esoph$ncases)
#How many controls are there?
all_controls <- sum(esoph$ncontrols)
#What is the probability that a subject in the highest alcohol consumption group is a cancer case?
esoph %>%
  filter(alcgp == max(alcgp)) %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)
#What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
esoph %>%
  filter(alcgp == min(alcgp)) %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)
#Given that a person is a case, what is the probability that they smoke 10g or more a day?
esoph %>%
  filter(tobgp >"0-9g/day") %>% summarize(sum(ncases)) / all_cases
#Given that a person is a control, what is the probability that they smoke 10g or more a day?
esoph %>%
  filter(tobgp >"0-9g/day") %>% summarize(sum(ncontrols)) / all_controls
#For cases, what is the probability of being in the highest alcohol group?
p_case_high_alc <- esoph %>%
  filter(alcgp == max(alcgp)) %>%
  summarize(sum(ncases)) / all_cases
#For cases, what is the probability of being in the highest tobacco group?
esoph %>%
  filter(tobgp == max(tobgp)) %>%
  summarize(sum(ncases)) / all_cases
#For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
esoph %>%
  filter(tobgp == max(tobgp)&alcgp == max(alcgp)) %>%
  summarize(sum(ncases)) / all_cases
#For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
p_case_high_tob_or_alc <- esoph %>%
  filter(tobgp == max(tobgp)|alcgp == max(alcgp)) %>%
  summarize(sum(ncases)) / all_cases
#For controls, what is the probability of being in the highest alcohol group?
p_control_high_alc <- esoph %>%
  filter(alcgp == max(alcgp)) %>%
  summarize(sum(ncontrols)) / all_controls
#How many times more likely are cases than controls to be in the highest alcohol group?
p_case_high_alc/p_control_high_alc
#For controls, what is the probability of being in the highest tobacco group?
p_control_high_tob <- esoph %>%
  filter(tobgp == max(tobgp)) %>%
  summarize(sum(ncontrols)) / all_controls
#For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?
p_control_high_tob_and_alc <- esoph %>%
  filter(tobgp == max(tobgp)&alcgp == max(alcgp)) %>%
  summarize(sum(ncontrols)) / all_controls
#For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?
p_control_high_tob_or_alc <- esoph %>%
  filter(tobgp == max(tobgp)|alcgp == max(alcgp)) %>%
  summarize(sum(ncontrols)) / all_controls
#How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?
p_case_high_tob_or_alc/p_control_high_tob_or_alc

#The cumulative distribution function (CDF) is a distribution function for continuous data  that reports the proportion of the data below  for all values of :
# F(a) = Pr (x<=a)
#The CDF is the probability distribution function for continuous variables. For example, to determine the probability that a male student is taller than 70.5 inches given a vector of male heights , we can use the CDF:
# Pr (x > 70.5) = 1-Pr(x<=70.5) = 1 - F(70.5)
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches
#Given male heights x:
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
#We can estimate the probability that a male is taller than 70.5 inches using:
1 - pnorm(70.5, mean(x), sd(x))
#Code: Discretization and the normal approximation
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")
# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)
# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))
# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))
# The probability density  is defined such that the integral of  over a range gives the CDF of that range.
# F(a) = Pr (X <= a) = \int_-infinity^a f(x)dx

#We can use dnorm() to plot the density curve for the normal distribution. 
#dnorm(z) gives the probability density  of a certain z-score, so we can draw a curve by calculating the density over a range of possible values of z.
library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()
#Note that dnorm() gives densities for the standard normal distribution by default. 
#Probabilities for alternative normal distributions with mean mu and standard deviation sigma can be evaluated with:
dnorm(z, mu, sigma)
#rnorm(n, avg, s) generates n random numbers from the normal distribution with average avg and standard deviation s.
#By generating random numbers from the normal distribution, we can simulate height data with similar properties to our dataset. 
#Here we generate simulated height data using the normal distribution.

#Code: Generating normally distributed random numbers
# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)
#Code: Monte Carlo simulation of tallest person over 7 feet
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)

#You may encounter other continuous distributions (Student t, chi-squared, exponential, gamma, beta, etc.).
#R provides functions for density (d), quantile (q), probability distribution (p) and random number generation (r) for many of these distributions.
#Each distribution has a matching abbreviation (for example, norm() or t()) that is paired with the related function abbreviations (d, p, q, r) to create appropriate functions.
#For example, use rt() to generate random numbers for a Monte Carlo simulation using the Student t distribution.
#Use d to plot the density function of a continuous distribution. Here is the density function for the normal distribution (abbreviation norm()):
  x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()

# Assign a variable 'female_avg' as the average female height.
female_avg <- 64
# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3
# Using variables 'female_avg' and 'female_sd', calculate the probability that a randomly selected female is shorter than 5 feet. Print this value to the console.
pnorm(60,female_avg,female_sd)
# Using variables 'female_avg' and 'female_sd', calculate the probability that a randomly selected female is 6 feet or taller. Print this value to the console.
1- pnorm(72,female_avg, female_sd)
# Using variables 'female_avg' and 'female_sd', calculate the probability that a randomly selected female is between the desired height range. Print this value to the console
pnorm(67,female_avg,female_sd) - pnorm(61,female_avg,female_sd)
# Assign a variable 'female_avg' as the average female height. Convert this value to centimeters.
female_avg <- 64*2.54
# Assign a variable 'female_sd' as the standard deviation for female heights. Convert this value to centimeters.
female_sd <- 3*2.54
# Using variables 'female_avg' and 'female_sd', calculate the probability that a randomly selected female is between the desired height range. Print this value to the console.
pnorm(67*2.54,female_avg,female_sd) - pnorm(61*2.54,female_avg,female_sd)
# To a variable named 'taller', assign the value of a height that is one SD taller than average.
taller <- female_avg + female_sd
# To a variable named 'shorter', assign the value of a height that is one SD shorter than average.
shorter <- female_avg - female_sd
# Calculate the probability that a randomly selected female is between the desired height range. Print this value to the console.
pnorm(taller,female_avg,female_sd) - pnorm(shorter,female_avg,female_sd)
# Assign a variable 'male_avg' as the average male height.
male_avg <- 69
# Assign a variable 'male_sd' as the standard deviation for male heights.
male_sd <- 3
# Determine the height of a man in the 99th percentile of the distribution.
qnorm(.99, male_avg, male_sd)
# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000
# Use the `set.seed` function to make sure your answer matches the expected result after random number generation.
set.seed(1)
# Create an object called `highestIQ` that contains the highest IQ score from each random distribution of 10,000 people.
highestIQ <- replicate(B, {
  max(rnorm(10000,100,15))
})
# Make a histogram of the highest IQ scores.
hist(highestIQ)

#Set the seed to 16, then use rnorm() to generate a normal distribution of 10000 tests with a mean of 20.9 and standard deviation of 5.7. 
#Save these values as act_scores. You'll be using this dataset throughout these four multi-part questions.
set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000,20.9,5.7)
#What is the mean of act_scores?
mean(act_scores)
#What is the standard deviation of act_scores
sd(act_scores)
#A perfect score is 36 or greater (the maximum reported score is 36).
#In act_scores, how many perfect scores are there out of 10,000 simulated tests?
sum(act_scores >= 36)
#In act_scores, what is the probability of an ACT score greater than 30?
mean(act_scores > 30)
#In act_scores, what is the probability of an ACT score less than or equal to 10?
mean(act_scores <= 10)
#Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value of the probability density function 
#over x given a mean of 20.9 and standard deviation of 5.7; save the result as f_x. Plot x against f_x.
x <- 1:36
f_x <- dnorm(x,20.9,5.7)
data.frame(x, f_x) %>%
  ggplot(aes(x, f_x)) +
  geom_line
#Convert act_scores to Z-scores. Recall from Data Visualization (the second course in this series) that to standardize values 
#(convert values into Z-scores, that is, values distributed with a mean of 0 and standard deviation of 1), you must 
#subtract the mean and then divide by the standard deviation. Use the mean and standard deviation of act_scores, not the original values used to generate random test scores.
z_scores <- (act_scores - mean(act_scores))/sd(act_scores)
#What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
mean(z_scores > 2)
#What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
mean(act_scores)+2*sd(act_scores)
#Use qnorm() to determine the 97.5th percentile of normally distributed data with the mean and standard deviation observed in act_scores.
#What is the 97.5th percentile of act_scores?
qnorm(.975,mean(act_scores), sd(act_scores))
#Write a function that takes a value and produces the probability of an ACT score less than or equal to that value (the CDF). Apply this function to the range 1 to 36.
cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
#What is the minimum integer score such that the probability of that score or lower is at least .95?
min(which(cdf >= .95))
#Use qnorm() to determine the expected 95th percentile, the value for which the probability of receiving that score or lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7.
#What is the expected 95th percentile of ACT scores?
qnorm(.95,20.9,5.7)
#Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th percentiles of the act_scores data. Save these as sample_quantiles.
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
#In what percentile is a score of 26?
names(sample_quantiles[max(which(sample_quantiles < 26))])
#Make a corresponding set of theoretical quantiles using qnorm() over the interval p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard deviation 5.7. 
#Save these as theoretical_quantiles. Make a QQ-plot graphing sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis.
theoretical_quantiles <- qnorm(p,20.9,5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()

#Modeling a random variable
# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)

#Monte Carlo simulation: Chance of casino losing money on roulette
#We build a sampling model for the random variable  that represents the casino's total winnings. 
# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]
# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S
#We use the sampling model to run a Monte Carlo simulation and use the results to estimate the probability of the casino losing money.
n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
    X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
    sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money
#We can plot a histogram of the observed values of S as well as the normal density curve based on the mean and standard deviation of S.
library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
    ggplot(aes(S, ..density..)) +
    geom_histogram(color = "black", binwidth = 10) +
    ylab("Probability") +
    geom_line(data = normal_density, mapping = aes(s, f), color = "blue")
#These equations apply to the case where there are only two outcomes, a and b with proportions p and (1-p) respectively. 
#The general principles above also apply to random variables with more than two outcomes.

#Expected value of a random variable:
# ap + b(1-p)
#Expected value of the sum of n draws of a random variable: 
# n * (ap + b(1 -p))
#Standard deviation of an urn with two values: 
# abs(b-a) * sqrt(p(1 - p))
#Standard error of the sum of n draws of a random variable:
# sqrt(n) * abs(b-a) * sqrt(p*(1-p))
#CLT instead of monte carlo
mu <- n * (20-18)/38
se <- sqrt(n) * 2 * sqrt(90)/19
pnorm(0,mu,se)

# The variables `green`, `black`, and `red` contain the number of pockets for each color
green <- 2
black <- 18
red <- 18
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / sum(green,black,red)
# Print the variable `p_green` to the console
p_green
# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green
# Create a model to predict the random variable `X`, your winnings from betting on green. Sample one time.
X <- sample(c(-1, 17), 1, replace = TRUE, prob = c(p_not_green, p_green))    
# Print the value of `X` to the console
X
# Calculate the expected outcome if you win $17 if the ball lands on green and you lose $1 if the ball doesn't land on green
17 * p_green + -1 * p_not_green
# Compute the standard error of the random variable
abs(17--1) * sqrt(p_green*p_not_green)
# Define the number of bets using the variable 'n'
n <- 1000
# Create a vector called 'X' that contains the outcomes of 1000 samples
X <- sample(c(-1,17), n, replace = TRUE, prob = c(p_not_green, p_green))
# Assign the sum of all 1000 outcomes to the variable 'S'
S <- sum(X)
# Print the value of 'S' to the console
S
# Calculate the expected outcome of 1,000 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
n * (17*p_green + -1*p_not_green)
# Compute the standard error of the sum of 1,000 outcomes
b <- -1
a <- 17
sqrt(n) * abs(b-a) * sqrt(p_green*p_not_green)
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38
# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green
# Define the number of bets using the variable 'n'
n <- 100
# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)
# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)
# Using the expected value 'avg' and standard error 'se', compute the probability that you win money betting on green 100 times.
1-pnorm(0,avg,se)
# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000
# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S <- replicate(B,sum(sample(c(17,-1), n, replace = TRUE, prob = c(p_green,p_not_green))))
# Compute the average value for 'S'
mean(S)
# Calculate the standard deviation of 'S'
sd(S)
# Calculate the proportion of outcomes in the vector `S` that exceed $0
mean(S>0)
# Define the number of bets using the variable 'n'
n <- 10000
# Create a vector called `X` that contains the outcomes of `n` bets
X <- sample(c(17,-1), n, replace = TRUE, prob = c(p_green,p_not_green))
# Define a variable `Y` that contains the mean outcome per bet. Print this mean to the console.
Y <- mean(X)
Y
# Calculate the expected outcome of `Y`, the mean outcome per bet in 10,000 bets
(17*p_green+-1*p_not_green)
# Compute the standard error of 'Y', the mean outcome per bet from 10,000 bets.
(abs(17--1) * sqrt(p_green*p_not_green))/sqrt(n)
# We defined the average using the following code
avg <- 17*p_green + -1*p_not_green
# We defined standard error using this equation
se <- 1/sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)
# Given this average and standard error, determine the probability of winning more than $0. Print the result to the console.
1-pnorm(0,avg,se)
# The variable `n` specifies the number of independent bets on green
n <- 10000
# The variable `B` specifies the number of times we want the simulation to run
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random number generation
set.seed(1)
# Generate a vector `S` that contains the the average outcomes of 10,000 bets modeled 10,000 times
S <- replicate(B,mean(sample(c(17,-1), n, replace = TRUE, prob = c(p_green,p_not_green))))
# Compute the average of `S`
mean(S)
# Compute the standard deviation of `S`
sd(S)
# Compute the proportion of outcomes in the vector 'S' where you won more than $0
mean(S>0)
# 1 point for correct -0.25 for wrong.  44 multiple choice with 5 answer choices
p <- 1/5 # one correct choice of 5 options
p
#expected value of guessing
a <- 1
b <- -0.25
mu <- a*p + b*(1-p)
mu
#standard error of guessing
n <- 44
sigma <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
sigma
#probability guessing gets 8 points or more
1-pnorm(8,mu,sigma)
#Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.
set.seed(21, sample.kind = "Rounding")
B <- 10000
n <- 44
p <- 0.2
tests <- replicate(B, {
  X <- sample(c(1, -0.25), n, replace = TRUE, prob = c(p, 1-p))
  sum(X)
})
#probability of scoring 8 or higher
mean(tests >= 8)
#options reduced to 4 penalty for guessing removed
p <- 1/4
a <- 1
b <- 0
mu <- a*p + b*(1-p)
mu
#expected value on new test
mu*n
# consider range of correct answer probabilities p <- seq(0.25, 0.95, 0.05)
# lowest p such that probability of scoring over 35 exceeds 80%
p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

min(p[which(exp_val > 0.8)])
#casino roulette win = 6 loss = -1 bet on 5 out of 38 total
#chance of loss after 500 bets
#expected value for 1 bet
p <- 5/38
a <- 6
b <- -1
mu <- a*p + b*(1-p)
mu
#standard error of the payout for 1 bet
sigma <- abs(b-a) * sqrt(p*(1-p))
sigma
#expected value of the average payout
mu
#standard error of the average payout over 500 bets
n <- 500
sigma/sqrt(n)
#expected value of the sum of 500 bets
mu*n
#SE of the sum of 500 bets
sqrt(n) * abs(b-a) * sqrt(p*(1-p))
#calculate probability of losing money
pnorm(0,mu*n,sqrt(n) * sigma)

#Code: Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)
# Code: Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})
# Code: Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")
# Code: Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

# Code: Calculating interest rates for expected value of 0
# We can calculate the amount x to add to each loan so that the expected value is 0 using the equation:
# lp + x(1-p)0. 
# Note that this equation is the definition of expected value given a loss per foreclosure l with foreclosure probability p and profit x if there is no foreclosure (probability ).
# We solve for x = -l*p/(1-p) and calculate x:
x = - loss_per_foreclosure*p/(1-p)
x
# On a $180,000 loan, this equals an interest rate of:
x/180000
# Code: Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))\x
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans
# Code: Monte Carlo simulation for 1% probability of losing money
# Note that your results will vary from the video because the seed is not set.
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money
#Code: Expected value with higher default rate and interest rate
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)
# Code: Calculating number of loans for desired probability of losing money
# The number of loans required is:
  z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n 
# Code: Monte Carlo simulation with known default probability
# This Monte Carlo simulation estimates the expected profit given a known probability of p=0.04 default . 
# Note that your results will differ from the video because the seed is not set.
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)
# Code: Monte Carlo simulation with unknown default probability
# This Monte Carlo simulation estimates the expected profit given an unknown probability of default 0.03 <= p <= 0.05, modeling the situation where an event changes the probability of default for all borrowers simultaneously. 
# Note that your results will differ from the video because the seed is not set.
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million

#Assessment
# Assign the number of loans to the variable `n`
n <- 10000
# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000
# Assign the probability of default to the variable `p_default`
p_default <- 0.03
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Generate a vector called `defaults` that contains the default outcomes of `n` loans
defaults <- sample( c(0,1), n, prob=c(1-p_default, p_default), replace = TRUE)
# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
S <- sum(defaults * loss_per_foreclosure)
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# The variable `B` specifies the number of times we want the simulation to run
B <- 10000
# Generate a list of summed losses 'S'. Replicate the code from the previous exercise over 'B' iterations to generate a list of summed losses for 'n' loans.  Ignore any warnings for now.
S <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p_default, p_default), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})
# Plot a histogram of 'S'.  Ignore any warnings for now.
hist(S)
# Calculate the expected loss due to default out of 10,000 loans
n*(p_default*loss_per_foreclosure + (1-p_default)*0)
# Compute the standard error of the sum of 10,000 loans
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p_default*(1-p_default))
# Assign a variable `x` as the total amount necessary to have an expected outcome of $0
x = - loss_per_foreclosure*p_default/(1-p_default)
# Convert `x` to a rate, given that the loan amount is $180,000. Print this value to the console.
x/180000
# Generate a variable `z` using the `qnorm` function
z <- qnorm(0.05)
# Generate a variable `x` using `z`, `p_default`, `loss_per_foreclosure`, and `n`
x <- -loss_per_foreclosure*( n*p_default - z*sqrt(n*p_default*(1-p_default)))/ ( n*(1-p_default) + z*sqrt(n*p_default*(1-p_default)))
# Convert `x` to an interest rate, given that the loan amount is $180,000. Print this value to the console.
x/180000