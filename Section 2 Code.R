 # load the dataset
library(dslabs)
data(murders)
head(murders)
library(dslabs)
data(heights)
 
 # check data for type categorical, ordinal, measurement
 library(dslabs)
 data(heights)
 names(heights)
 x <- heights$height
 length(unique(x))
 tab <- table(heights$height)
 sum(tab==1)
 
 # make a table of category proportions
 prop.table(table(heights$sex))
 
 #make a CDF plot of heights in height
 a <- seq(min(heights$height), max(heights$height), length = 100)    # define range of values spanning the dataset
 cdf_function <- function(x) {    # computes prob. for a single value
         mean(heights$height <= x)
 }
 cdf_values <- sapply(a, cdf_function)
 plot(a, cdf_values)
 
 # define x as vector of male heights
 library(tidyverse)
 library(dslabs)
 data(heights)
 index <- heights$sex=="Male"
 x <- heights$height[index]
 
 # calculate the mean and standard deviation manually
 average <- sum(x)/length(x)
 SD <- sqrt(sum((x - average)^2)/length(x))
 
 # built-in mean and sd functions - note that the audio and printed values disagree
 average <- mean(x)
 SD <- sd(x)
 c(average = average, SD = SD)
 
 # calculate standard units
 z <- scale(x)
 
 # calculate proportion of values within 2 SD of mean
 mean(abs(z) < 2)
 
 #Given male heights
 library(tidyverse)
 library(dslabs)
 data(heights)
 x <- heights %>% filter(sex=="Male") %>% pull(height)
 1 - pnorm(70.5, mean(x), sd(x))
 
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
 
 #assignment
 library(dslabs)
 data(heights)
 x <- heights$height[heights$sex == "Male"]
 mean(x <= 72) - mean(x <= 69)
 avg <- mean(x)
 stdev <- sd(x)
 pnorm(72, avg, stdev) - pnorm(69, avg, stdev)
 exact <- mean(x > 79 & x <= 81)
 approx <- pnorm(81, mean(x), sd(x)) - pnorm(79, mean(x), sd(x))
 exact / approx
 #Assume that the distribution of adult men in the world as normally distributed
 #with an average of 69 inches and a standard deviation of 3 inches.
 #estimate the proportion of adult men that are taller than 7 feet
 1 - pnorm(7*12, 69,3)
 #estimate how many of these 1 billion men are at least seven feet tall
 p <- 1 - pnorm(7*12, 69,3)
 round(p*(10^9))
 # 10 NBA players that are 7 feet tall or higher.
 # calc proportion of the world's 18 to 40 year old seven footers that are in the NBA
 10/N
 ## Change the solution to previous answer calc 6'12" proprtion of 150 players
 p <- 1 - pnorm(6*12+8, 69, 3)
 N <- round(p * 10^9)
 150/N
 
#Given a dataset data and desired quantile q, you can find the qth quantile of data with:
#quantile(data,q) 
 p <- seq(0.01, 0.99, 0.01)
 quantile(data, p)
 #Load the heights dataset from the dslabs package:
library(dslabs)
data(heights)
 #Use summary() on the heights$height variable to find the quartiles:
summary(heights$height)
 #Find the percentiles of heights$height:
p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)
#Confirm that the 25th and 75th percentiles match the 1st and 3rd quartiles. Note that quantile() returns a named vector. You can access the 25th and 75th percentiles like this (adapt the code for other percentile values):
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]
theoretical_quantiles <- qnorm(p, 69, 3)

# define x and z
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)

# proportion of data below 69.5
mean(x <= 69.5)

# calculate observed and theoretical quantiles
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))

# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

#assignment
library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
length(male)
length(female)
#10th 30th 50th 70th 90th percentiles as vectores stored in a data frame df
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
female_percentiles <- c(quantile(female, seq(.01, 0.99, 0.01))[names(quantile(female, seq(.01, 0.99, 0.01))) == "10%"],
                        quantile(female, seq(.01, 0.99, 0.01))[names(quantile(female, seq(.01, 0.99, 0.01))) == "30%"],
                        quantile(female, seq(.01, 0.99, 0.01))[names(quantile(female, seq(.01, 0.99, 0.01))) == "50%"],
                        quantile(female, seq(.01, 0.99, 0.01))[names(quantile(female, seq(.01, 0.99, 0.01))) == "70%"],
                        quantile(female, seq(.01, 0.99, 0.01))[names(quantile(female, seq(.01, 0.99, 0.01))) == "90%"])
male_percentiles <- c(quantile(male, seq(.01, 0.99, 0.01))[names(quantile(male, seq(.01, 0.99, 0.01))) == "10%"],
                      quantile(male, seq(.01, 0.99, 0.01))[names(quantile(male, seq(.01, 0.99, 0.01))) == "30%"],
                      quantile(male, seq(.01, 0.99, 0.01))[names(quantile(male, seq(.01, 0.99, 0.01))) == "50%"],
                      quantile(male, seq(.01, 0.99, 0.01))[names(quantile(male, seq(.01, 0.99, 0.01))) == "70%"],
                      quantile(male, seq(.01, 0.99, 0.01))[names(quantile(male, seq(.01, 0.99, 0.01))) == "90%"])
df <- data.frame(female = female_percentiles, male = male_percentiles)

#assignment
library(HistData)
data(Galton)
x <- Galton$child
#Compute the average and median of height of the children
mean(x)
median(x)
#Compute the standard deviation and the median absolute deviation of these data.
sd(x)
mad(x)
#imagine a mistake introduced here
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
#how many inches the average grows after this mistake
mean(x_with_error) - mean(x)
#how many inches the SD grows after this mistake
sd(x_with_error) - sd(x)
#how many inches the median grows after the mistake.
median(x_with_error) - median(x)
#how many inches the MAD grows after the mistake
mad(x_with_error) - mad(x)
#write a function that takes a value and returns the average of the vector x after the first entry changed to k
error_avg <- function(k){
        x_with_error <- x 
        x_with_error[1] <- k
        mean(x_with_error)
}
error_avg(10000)
error_avg(-10000)

#ggplot2
library(tidyverse)
library(dslabs)
data(murders)
ggplot(data = murders)
murders %>% ggplot()
p <- ggplot(data = murders)
class(p)
print(p)    # this is equivalent to simply typing p
p

#adding layers
library(tidyverse)
library(dslabs)
data(murders)

murders %>% ggplot() +
        geom_point(aes(x = population/10^6, y = total))

# add points layer to predefined ggplot object
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total))

# add text layer to scatterplot
p + geom_point(aes(population/10^6, total)) +
        geom_text(aes(population/10^6, total, label = abb))

#Example of aes behavior
# no error from this call
p_test <- p + geom_text(aes(population/10^6, total, label = abb))

# error - "abb" is not a globally defined variable and cannot be found outside of aes
p_test <- p + geom_text(aes(population/10^6, total), label = abb)

# change the size of the points
p + geom_point(aes(population/10^6, total), size = 3) +
        geom_text(aes(population/10^6, total, label = abb))

# move text labels slightly to the right
p + geom_point(aes(population/10^6, total), size = 3) +
        geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)

# simplify code by adding global aesthetic
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) +
        geom_text(nudge_x = 1.5)

# local aesthetics override global aesthetics
p + geom_point(size = 3) +
        geom_text(aes(x = 10, y = 800, label = "Hello there!"))

#Code: Log-scale the x- and y-axis
# define p
library(tidyverse)
library(dslabs)
data(murders)
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))

# log base 10 scale the x-axis and y-axis
p + geom_point(size = 3) +
        geom_text(nudge_x = 0.05) +
        scale_x_continuous(trans = "log10") +
        scale_y_continuous(trans = "log10")

# efficient log scaling of the axes
p + geom_point(size = 3) +
        geom_text(nudge_x = 0.075) +
        scale_x_log10() +
        scale_y_log10()
#Code: Add labels and title
p + geom_point(size = 3) +
        geom_text(nudge_x = 0.075) +
        scale_x_log10() +
        scale_y_log10() +
        xlab("Population in millions (log scale)") +
        ylab("Total number of murders (log scale)") +
        ggtitle("US Gun Murders in 2010")
#Code: Change color of the points
# redefine p to be everything except the points layer
p <- murders %>%
        ggplot(aes(population/10^6, total, label = abb)) +
        geom_text(nudge_x = 0.075) +
        scale_x_log10() +
        scale_y_log10() +
        xlab("Population in millions (log scale)") +
        ylab("Total number of murders (log scale)") +
        ggtitle("US Gun Murders in 2010")

# make all points blue
p + geom_point(size = 3, color = "blue")

# color points by region
p + geom_point(aes(col = region), size = 3)
#Code: Add a line with average murder rate
# define average murder rate
r <- murders %>%
        summarize(rate = sum(total) / sum(population) * 10^6) %>%
        pull(rate)

# basic line with average murder rate for the country
p <- p + geom_point(aes(col = region), size = 3) +
        geom_abline(intercept = log10(r))    # slope is default of 1

# change line to dashed and dark grey, line under points
p + 
        geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
        geom_point(aes(col = region), size = 3)
#Code: Change legend title
p <- p + scale_color_discrete(name = "Region")    # capitalize legend title

# theme used for graphs in the textbook and course
library(dslabs)
ds_theme_set()

# themes from ggthemes
library(ggthemes)
p + theme_economist()    # style of the Economist magazine
p + theme_fivethirtyeight()    # style of the FiveThirtyEight website

# load libraries
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)

# define the intercept
r <- murders %>%
        summarize(rate = sum(total) / sum(population) * 10^6) %>%
        .$rate

# make the plot, combining all elements
murders %>%
        ggplot(aes(population/10^6, total, label = abb)) +
        geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
        geom_point(aes(col = region), size = 3) +
        geom_text_repel() +
        scale_x_log10() +
        scale_y_log10() +
        xlab("Population in millions (log scale)") +
        ylab("Total number of murders (log scale)") +
        ggtitle("US Gun Murders in 2010") +
        scale_color_discrete(name = "Region") +
        theme_economist()

#Code: Histograms in ggplot2
# load heights data
library(tidyverse)
library(dslabs)
data(heights)

# define p
p <- heights %>%
        filter(sex == "Male") %>%
        ggplot(aes(x = height))

# basic histograms
p + geom_histogram()
p + geom_histogram(binwidth = 1)

# histogram with blue fill, black outline, labels and title
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
        xlab("Male heights in inches") +
        ggtitle("Histogram")
#Code: Smooth density plots in ggplot2
p + geom_density()
p + geom_density(fill = "blue")
#Code: Quantile-quantile plots in ggplot2
# basic QQ-plot
p <- heights %>% filter(sex == "Male") %>%
        ggplot(aes(sample = height))
p + geom_qq()

# QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>%
        filter(sex == "Male") %>%
        summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
        geom_abline()

# QQ-plot of scaled data against the standard normal distribution
heights %>%
        ggplot(aes(sample = scale(height))) +
        geom_qq() +
        geom_abline()
#Code: Grids of plots with the gridExtra package
# define plots p1, p2, p3
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")

# arrange plots next to each other in 1 row, 3 columns
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)

#assignment
library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
data(murders)
p <- ggplot(murders)
#what is the class of p
class(p)

data(heights)
# define ggplot object called p like in the previous exercise but using a pipe 
p <- heights %>% ggplot()

## Fill in the blanks
murders %>% ggplot(aes(x = , y = )) +
        geom_point()
## Fill in the blanks
murders %>% ggplot(aes(x = population, y = total)) +
        geom_point()
#Remake the plot but flip the axes
murders %>% ggplot(aes(total, population)) +
        geom_point()
#Rewrite the code from the previous exercise to:
#add a label aesthetic to aes equal to the state abbreviation
#use geom_label instead of geom_point
murders %>% ggplot(aes(population, total, label = abb)) +
        geom_label()
#Rewrite the code above to make the labels blue by adding an argument to geom_label.
murders %>% ggplot(aes(population, total,label= abb)) +
        geom_label(color = "blue")
#color by region
murders %>% ggplot(aes(population, total, label = abb, col=region)) +
        geom_label()
#change both axis to be in the log scale
p + scale_x_log10() + 
        scale_y_log10()
#add a title
p + scale_x_log10() + 
        scale_y_log10() +
        ggtitle("Gun murder data")
#Create a ggplot object called p using the pipe to assign the heights data to a ggplot object.
#Assign height to the x values through the aes function.
p <- heights %>% ggplot(aes(x = height))
#make the histogram
p + geom_histogram()
#Use the binwidth argument to change the histogram to use bins of size 1 inch.
p + geom_histogram(binwidth = 1)
#create a smooth density plot of heights
heights %>% 
        ggplot(aes(height)) +
        geom_density()
#Create separate smooth density plots for males and females by defining group by sex.
heights %>% 
        ggplot(aes(x = height, group = sex)) + geom_density()
#Change the density plots from the previous exercise to add color for each group.
heights %>% 
        ggplot(aes(height, color = sex)) + geom_density()
#using fill to fill the area under the line use alph to change the alph of the fill
heights %>% 
        ggplot(aes(height, fill = sex)) + 
        geom_density(alpha = 0.2) 

#dplyr functions
library(tidyverse)
library(dslabs)
data(heights)
data(murders)

# compute average and standard deviation for males
s <- heights %>%
        filter(sex == "Male") %>%
        summarize(average = mean(height), standard_deviation = sd(height))

# access average and standard deviation from summary table
s$average
s$standard_deviation

# compute median, min and max
heights %>%
        filter(sex == "Male") %>%
        summarize(median = median(height),
                  minimum = min(height),
                  maximum = max(height))
# alternative way to get min, median, max in base R
quantile(heights$height, c(0, 0.5, 1))

# NOTE: The following code will NOT generate an error if using dplyr 1.0 or later

# generates an error: summarize can only take functions that return a single value
heights %>%
        filter(sex == "Male") %>%
        summarize(range = quantile(height, c(0, 0.5, 1)))

#dot operator
murders <- murders %>% mutate(murder_rate = total/population*100000)
summarize(murders, mean(murder_rate))

# calculate US murder rate, generating a data frame
us_murder_rate <- murders %>%
        summarize(rate = sum(total) / sum(population) * 100000)
us_murder_rate

# extract the numeric US murder rate with the dot operator
us_murder_rate %>% .$rate

# calculate and extract the murder rate with one pipe
us_murder_rate <- murders %>%
        summarize(rate = sum(total) / sum(population) * 100000) %>%
        .$rate
# compute separate average and standard deviation for male/female heights
heights %>%
        group_by(sex) %>%
        summarize(average = mean(height), standard_deviation = sd(height))

# compute median murder rate in 4 regions of country
murders <- murders %>%
        mutate(murder_rate = total/population * 100000)
murders %>%
        group_by(region) %>%
        summarize(median_rate = median(murder_rate))
# set up murders object
murders <- murders %>%
        mutate(murder_rate = total/population * 100000)

# arrange by population column, smallest to largest
murders %>% arrange(population) %>% head()

# arrange by murder rate, smallest to largest
murders %>% arrange(murder_rate) %>% head()

# arrange by murder rate in descending order
murders %>% arrange(desc(murder_rate)) %>% head()

# arrange by region alphabetically, then by murder rate within each region
murders %>% arrange(region, murder_rate) %>% head()

# show the top 10 states with highest murder rate, not ordered by rate
murders %>% top_n(10, murder_rate)

# show the top 10 states with highest murder rate, ordered by rate
murders %>% arrange(desc(murder_rate)) %>% top_n(10)

# alternatively, can use the slice_max function
murders %>% slice_max(murder_rate, n = 10)

#exercise
library(NHANES)
data(NHANES)
#To ignore the NAs, we can use the na.rm argument:
mean(na_example, na.rm = TRUE)
sd(na_example, na.rm = TRUE)
#pipe into tab nhanes with 20-29 age filter female sex filter
tab <- NHANES %>% filter (Gender == "female", AgeDecade == " 20-29")
#Save the average and standard deviation of systolic blood pressure as average and standard_deviation to a variable called ref.
#Use the summarize function filtering for 20-29 year old females and connect the results using the pipe %>%. When doing this remember there are NAs in the data!
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))
# assign the average to a numeric variable called ref_avg using the . or pull.
ref_avg <- NHANES %>%
        filter(AgeDecade == " 20-29" & Gender == "female") %>%
        summarize(average = mean(BPSysAve, na.rm = TRUE), 
                  standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>%
        .$average
#Report the min and max values for the same group as in the previous exercises.
NHANES %>%
        filter(AgeDecade == " 20-29"  & Gender == "female") %>%
        summarize(minbp = min(BPSysAve, na.rm = TRUE),
                  maxbp = max(BPSysAve, na.rm = TRUE))
#Use the functions filter, group_by, summarize, and the pipe %>% to compute the average and standard deviation of systolic blood pressure for females for each age group separately.
NHANES %>%
        filter(Gender == "female") %>%
        group_by(AgeDecade) %>%
        summarize(average = mean(BPSysAve, na.rm = TRUE), 
                  standard_deviation = sd(BPSysAve, na.rm=TRUE))
#Calculate the average and standard deviation of systolic blood pressure for males for each age group separately using the same methods as in the previous exercise.
NHANES %>%
        filter(Gender == "male") %>%
        group_by(AgeDecade) %>%
        summarize(average = mean(BPSysAve, na.rm = TRUE), 
                  standard_deviation = sd(BPSysAve, na.rm=TRUE))
#Create a single summary table for the average and standard deviation of systolic blood pressure using group_by(AgeDecade, Gender).
NHANES %>%
        group_by(AgeDecade, Gender) %>%
        summarize(average = mean(BPSysAve, na.rm = TRUE), 
                  standard_deviation = sd(BPSysAve, na.rm=TRUE))
#Compute the average and standard deviation for each value of Race1 for males in the age decade 40-49.
#Order the resulting table from lowest to highest average systolic blood pressure.
#Use the functions filter, group_by, summarize, arrange, and the pipe %>% to do this in one line of code.
#Within summarize, save the average and standard deviation of systolic blood pressure as average and standard_deviation
NHANES %>%
        filter(AgeDecade == " 40-49"  &Gender == "male") %>%
        group_by(Race1) %>%
        summarize(average = mean(BPSysAve, na.rm = TRUE), 
                  standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>%
        arrange(average)

#gapminder dataset
# load and inspect gapminder data
library(dslabs)
data(gapminder)
head(gapminder)

# compare infant mortality in Sri Lanka and Turkey
gapminder %>%
        filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
        select(country, infant_mortality)
# basic scatterplot of life expectancy versus fertility
ds_theme_set()    # set plot theme
filter(gapminder, year == 1962) %>%
ggplot(aes(fertility, life_expectancy)) +
geom_point()
               
# add color as continent
filter(gapminder, year == 1962) %>%
ggplot(aes(fertility, life_expectancy, color = continent)) +
geom_point()

# facet by continent and year
filter(gapminder, year %in% c(1962, 2012)) %>%
        ggplot(aes(fertility, life_expectancy, col = continent)) +
        geom_point() +
        facet_grid(continent ~ year)

# facet by year only
filter(gapminder, year %in% c(1962, 2012)) %>%
        ggplot(aes(fertility, life_expectancy, col = continent)) +
        geom_point() +
        facet_grid(. ~ year)

# facet by year, plots wrapped onto multiple rows
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
        filter(year %in% years & continent %in% continents) %>%
        ggplot(aes(fertility, life_expectancy, col = continent)) +
        geom_point() +
        facet_wrap(~year)
# scatterplot of US fertility by year
gapminder %>%
        filter(country == "United States") %>%
        ggplot(aes(year, fertility)) +
        geom_point()

# line plot of US fertility by year
gapminder %>%
        filter(country == "United States") %>%
        ggplot(aes(year, fertility)) +
        geom_line()
# line plot fertility time series for two countries- only one line (incorrect)
countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>%
        ggplot(aes(year, fertility)) +
        geom_line()

# line plot fertility time series for two countries - one line per country
gapminder %>% filter(country %in% countries) %>%
        ggplot(aes(year, fertility, group = country)) +
        geom_line()

# fertility time series for two countries - lines colored by country
gapminder %>% filter(country %in% countries) %>%
        ggplot(aes(year, fertility, col = country)) +
        geom_line()

# life expectancy time series - lines colored by country and labeled, no legend
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
        ggplot(aes(year, life_expectancy, col = country)) +
        geom_line() +
        geom_text(data = labels, aes(x, y, label = country), size = 5) +
        theme(legend.position = "none")

# add dollars per day variable
gapminder <- gapminder %>%
        mutate(dollars_per_day = gdp/population/365)

# histogram of dollars per day
past_year <- 1970
gapminder %>%
        filter(year == past_year & !is.na(gdp)) %>%
        ggplot(aes(dollars_per_day)) +
        geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled data
gapminder %>%
        filter(year == past_year & !is.na(gdp)) %>%
        ggplot(aes(log2(dollars_per_day))) +
        geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled x-axis
gapminder %>%
        filter(year == past_year & !is.na(gdp)) %>%
        ggplot(aes(dollars_per_day)) +
        geom_histogram(binwidth = 1, color = "black") +
        scale_x_continuous(trans = "log2")
# box plot of GDP by region
#dollars per day variable
gapminder <- gapminder %>%
        mutate(dollars_per_day = gdp/population/365)

# number of regions
length(levels(gapminder$region))

# boxplot of GDP by region in 1970
past_year <- 1970
p <- gapminder %>%
        filter(year == past_year & !is.na(gdp)) %>%
        ggplot(aes(region, dollars_per_day))
p + geom_boxplot()

# rotate names on x-axis
p + geom_boxplot() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

#reorder
# by default, factor order is alphabetical
fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)

# reorder factor by the category means
value <- c(10, 11, 12, 6, 4)
fac <- reorder(fac, value, FUN = mean)
levels(fac)

#enhance with ordering, sorting, and showing data
# reorder by median income and color by continent
p <- gapminder %>%
        filter(year == past_year & !is.na(gdp)) %>%
        mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
        ggplot(aes(region, dollars_per_day, fill = continent)) +    # color by continent
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("")
p

# log2 scale y-axis
p + scale_y_continuous(trans = "log2")

# add data points
p + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)

#histogram of income in west vs developing 1970 and 2010
# add dollars per day variable and define past year
gapminder <- gapminder %>%
        mutate(dollars_per_day = gdp/population/365)
past_year <- 1970

# define Western countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

# facet by West vs devloping
gapminder %>%
        filter(year == past_year & !is.na(gdp)) %>%
        mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
        ggplot(aes(dollars_per_day)) +
        geom_histogram(binwidth = 1, color = "black") +
        scale_x_continuous(trans = "log2") +
        facet_grid(. ~ group)

# facet by West/developing and year
present_year <- 2010
gapminder %>%
        filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
        mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
        ggplot(aes(dollars_per_day)) +
        geom_histogram(binwidth = 1, color = "black") +
        scale_x_continuous(trans = "log2") +
        facet_grid(year ~ group)

# define countries that have data available in both years
country_list_1 <- gapminder %>%
        filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
        filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

# make histogram including only countries with data available in both years
gapminder %>%
        filter(year %in% c(past_year, present_year) & country %in% country_list) %>%    # keep only selected countries
        mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
        ggplot(aes(dollars_per_day)) +
        geom_histogram(binwidth = 1, color = "black") +
        scale_x_continuous(trans = "log2") +
        facet_grid(year ~ group)

#boxplots of income in west vs dev 1970 and 2010 after
p <- gapminder %>%
        filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
        mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
        ggplot() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("") + scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
        facet_grid(year ~ .)

# arrange matching boxplots next to each other, colored by year
p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))

# see the code below the previous video for variable definitions

# smooth density plots - area under each curve adds to 1
gapminder %>%
        filter(year == past_year & country %in% country_list) %>%
        mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
        summarize(n = n()) %>% knitr::kable()

# smooth density plots - variable counts on y-axis
p <- gapminder %>%
        filter(year == past_year & country %in% country_list) %>%
        mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
        ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
        scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)

# add group as a factor, grouping regions
gapminder <- gapminder %>%
        mutate(group = case_when(
                .$region %in% west ~ "West",
                .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
                .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
                .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
                TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
        mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

# note you must redefine p with the new gapminder object first
p <- gapminder %>%
        filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
        ggplot(aes(dollars_per_day, fill = group)) +
        scale_x_continuous(trans = "log2")

# stacked density plot
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
        facet_grid(year ~ .)

# weighted stacked density plot
gapminder %>%
        filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
        group_by(year) %>%
        mutate(weight = population/sum(population*2)) %>%
        ungroup() %>%
        ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
        scale_x_continuous(trans = "log2") +
        geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)

# define gapminder
library(tidyverse)
library(dslabs)
data(gapminder)

# add additional cases
gapminder <- gapminder %>%
        mutate(group = case_when(
                .$region %in% west ~ "The West",
                .$region %in% "Northern Africa" ~ "Northern Africa",
                .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
                .$region == "Southern Asia" ~ "Southern Asia",
                .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
                .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
                .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

# define a data frame with group average income and average infant survival rate
surv_income <- gapminder %>%
        filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
        group_by(group) %>%
        summarize(income = sum(gdp)/sum(population)/365,
                  infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

# plot infant survival versus income, with transformed axes
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
        scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
        scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                           breaks = c(.85, .90, .95, .99, .995, .998)) +
        geom_label(size = 3, show.legend = FALSE) 

#assignment
#Using ggplot and the points layer, create a scatter plot of life expectancy versus fertility for the African continent in 2012.
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder %>% filter(year == 2012 & continent == "Africa") %>%
        ggplot(aes(fertility, life_expectancy )) +
        geom_point()
#Remake the plot from the previous exercises but this time use color to distinguish the different regions of Africa
gapminder %>% filter(year == 2012 & continent == "Africa") %>%
        ggplot(aes(fertility, life_expectancy, color=region )) +
        geom_point()
#Create a table showing the country and region for the African countries (use select) that in 2012 had fertility rates of 3 or less and life expectancies of at least 70.
df <- gapminder %>% filter(year == 2012 & continent == "Africa" & fertility <= 3 & life_expectancy >= 70) %>%
        select(country, region)
#Use filter to create a table with data for the years from 1960 to 2010 in Vietnam and the United States.
#Save the table in an object called tab.
tab <- gapminder %>% filter(year %in% (1960:2010) & country %in% c("United States", "Vietnam"))
#Use geom_line to plot life expectancy vs year for Vietnam and the United States and save the plot as p. The data table is stored in tab.
#Use color to distinguish the two countries.
#Print the object p.
p <- tab %>% ggplot(aes(year, life_expectancy, color = country)) + geom_line()
p
#Use a single line of code to create a time series plot from 1960 to 2010 of life expectancy vs year for Cambodia.
gapminder %>% filter(year %in% (1960:2010) & country == "Cambodia") %>%
        ggplot(aes(year, life_expectancy)) +
        geom_line()

#Use mutate to create a dollars_per_day variable, which is defined as gdp/population/365.
#Create the dollars_per_day variable for African countries for the year 2010.
#Remove any NA values.
#Save the mutated dataset as daydollars.
daydollars <- gapminder %>%
        filter(year == 2010 & continent == "Africa" & !is.na(gdp) & !is.na(population)) %>%
        mutate(dollars_per_day = gdp/population/365)
#Create a smooth density plot of dollars per day from daydollars.
#Use scale_x_continuous to change the x-axis to a log (base 2) scale.
daydollars %>% ggplot(aes(dollars_per_day)) +
        geom_density() +
        scale_x_continuous(trans = "log2")
#Create the dollars_per_day variable as in Exercise 7, but for African countries in the years 1970 and 2010 this time.
#Make sure you remove any NA values.
#Create a smooth density plot of dollars per day for 1970 and 2010 using a log (base 2) scale for the x axis.
#Use facet_grid to show a different density plot for 1970 and 2010.
gapminder %>%
        filter(year %in% c(1970,2010) & continent == "Africa" & !is.na(gdp) & !is.na(population)) %>%
        mutate(dollars_per_day = gdp/population/365) %>%
        ggplot(aes(dollars_per_day)) +
        geom_density() +
        scale_x_continuous(trans = "log2") +
        facet_grid(. ~ year)

#Create the dollars_per_day variable as in Exercise 7, but for African countries in the years 1970 and 2010 this time.
#Make sure you remove any NA values.
#Create a smooth density plot of dollars per day for 1970 and 2010 using a log (base 2) scale for the x axis.
#Use facet_grid to show a different density plot for 1970 and 2010.
#Make sure the densities are smooth by using bw = 0.5.
#Use the fill and position arguments where appropriate to create the stacked density plot of each region.
gapminder %>%
        filter(year %in% c(1970,2010) & continent == "Africa" & !is.na(gdp) & !is.na(population)) %>%
        mutate(dollars_per_day = gdp/population/365) %>%
        ggplot(aes(dollars_per_day, fill = region)) +
        geom_density(bw = 0.5, position = "stack") +
        scale_x_continuous(trans = "log2") +
        facet_grid(year ~ .)

#Generate dollars_per_day using mutate and filter for the year 2010 for African countries.
#Remember to remove NA values.
#Store the mutated dataset in gapminder_Africa_2010.
#Make a scatter plot of infant_mortality versus dollars_per_day for countries in the African continent.
#Use color to denote the different regions of Africa.
gapminder_Africa_2010 <- gapminder %>%
        filter(year == 2010 & continent == "Africa" & !is.na(gdp) & !is.na(population)) %>%
        mutate(dollars_per_day = gdp/population/365)

gapminder_Africa_2010 %>%
        ggplot(aes(dollars_per_day, infant_mortality, color = region)) +
        geom_point()

#Transform the x axis to be in the log (base 2) scale.
gapminder_Africa_2010 %>%  
        ggplot(aes(dollars_per_day, infant_mortality, color = region)) +
        geom_point() + 
        scale_x_continuous(trans = "log2")
#Add a geom_text layer to display country names in addition to of points.
gapminder_Africa_2010 %>%  
        ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
        geom_point() + 
        scale_x_continuous(trans = "log2") + 
        geom_text(aes(dollars_per_day, infant_mortality, label = country), nudge_x = 1)
#Generate dollars_per_day using mutate and filter for the years 1970 and 2010 for African countries.
#Remember to remove NA values.
#As in the previous exercise, make a scatter plot of infant_mortality versus dollars_per_day for countries in the African continent.
#As in the previous exercise, use color to denote the different regions of Africa.
#As in the previous exercise, transform the x axis to be in the log (base 2) scale.
#As in the previous exercise, add a layer to display country names instead of points.
#Use facet_grid to show different plots for 1970 and 2010. Align the plots vertically.
gapminder %>%
        filter(year %in% c(1970,2010) & continent == "Africa" & !is.na(gdp) & !is.na(population) & !is.na(infant_mortality)) %>%
        mutate(dollars_per_day = gdp/population/365) %>%
        ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
        geom_point() + 
        scale_x_continuous(trans = "log2") + 
        geom_text(aes(dollars_per_day, infant_mortality, label = country), nudge_x = 1) +
        facet_grid (year ~ .)

# dot plot showing the data
heights %>% ggplot(aes(sex, height)) + geom_point()

# jittered, alpha blended point plot
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)

#color blind friendly colors
color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
        ggplot(aes(x, y, color = col)) +
        geom_point(size = 5)
p1 + scale_color_manual(values = color_blind_friendly_cols)

#assignemnt
#Redefine the state object so that the levels are re-ordered by rate.
#Print the new object state and its levels (using levels) so you can see that the vector is now re-ordered by the levels.
library(dplyr)
library(ggplot2)
library(dslabs)
dat <- us_contagious_diseases %>%
        filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% mutate(rate = count / population * 10000 * 52 / weeks_reporting)
state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)
state <- reorder(state, rate)
state
levels(state)

#Add a single line of code to the definition of the dat table that uses mutate to reorder the states by the rate variable.
#The sample code provided will then create a bar plot using the newly defined dat.
library(dplyr)
library(ggplot2)
library(dslabs)
data(us_contagious_diseases)
dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
        mutate(rate = count / population * 10000 * 52 / weeks_reporting, ) %>%
        mutate(state = reorder(state, rate)) #line added
dat %>% ggplot(aes(state, rate)) +
        geom_bar(stat="identity") +
        coord_flip()

#Order the regions by their median murder rate by using mutate and reorder.
#Make a box plot of the murder rates by region.
#Show all of the points on the box plot.
murders %>% mutate(rate = total/population*100000)%>%
        mutate(region = reorder(region, rate, FUN = median)) %>%
        ggplot(aes(region, rate)) +
        geom_boxplot() +
        geom_point()

#slope charts
library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
        filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
        mutate(location = ifelse(year == 2010, 1, 2),
               location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                                 location + 0.22, location),
               hjust = ifelse(year == 2010, 1, 0)) %>%
        mutate(year = as.factor(year)) %>%
        ggplot(aes(year, life_expectancy, group = country)) +
        geom_line(aes(color = country), show.legend = FALSE) +
        geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
        xlab("") +
        ylab("Life Expectancy") 

#bland-altman plot
library(ggrepel)
dat %>%
        mutate(year = paste0("life_expectancy_", year)) %>%
        select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
        mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
               difference = life_expectancy_2015 - life_expectancy_2010) %>%
        ggplot(aes(average, difference, label = country)) +
        geom_point() +
        geom_text_repel() +
        geom_abline(lty = 2) +
        xlab("Average of 2010 and 2015") +
        ylab("Difference between 2015 and 2010")

# Tile plot of measles rate by year and state
# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
        filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
        mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
        mutate(state = reorder(state, rate))

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
        ggplot(aes(year, rate)) +
        geom_line() +
        ylab("Cases per 10,000") +
        geom_vline(xintercept=1963, col = "blue")

# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
        geom_tile(color = "grey50") +
        scale_x_continuous(expand = c(0,0)) +
        scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
        geom_vline(xintercept = 1963, col = "blue") +
        theme_minimal() + theme(panel.grid = element_blank()) +
        ggtitle(the_disease) +
        ylab("") +
        xlab("")
# Line plot of measles rate by year and state
# compute US average measles rate by year
avg <- us_contagious_diseases %>%
        filter(disease == the_disease) %>% group_by(year) %>%
        summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

# make line plot of measles rate by year by state
dat %>%
        filter(!is.na(rate)) %>%
        ggplot() +
        geom_line(aes(year, rate, group = state), color = "grey50", 
                  show.legend = FALSE, alpha = 0.2, size = 1) +
        geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
        scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
        ggtitle("Cases per 10,000 by state") +
        xlab("") +
        ylab("") +
        geom_text(data = data.frame(x = 1955, y = 50),
                  mapping = aes(x, y, label = "US average"), color = "black") +
        geom_vline(xintercept = 1963, col = "blue")

#Modify the tile plot to show the rate of smallpox cases instead of measles cases.
#Exclude years in which cases were reported in fewer than 10 weeks from the plot.
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)

the_disease = "Smallpox" #changed to Smallpox
dat <- us_contagious_diseases %>% 
        filter(!state%in%c("Hawaii","Alaska") & weeks_reporting >= 10 & disease == the_disease) %>% #added filter for weeks_reporting
        mutate(rate = count / population * 10000) %>% 
        mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) + 
        geom_tile(color = "grey50") + 
        scale_x_continuous(expand=c(0,0)) + 
        scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
        theme_minimal() + 
        theme(panel.grid = element_blank()) + 
        ggtitle(the_disease) + 
        ylab("") + 
        xlab("")

library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

#Modify the tile plot to show the rate of smallpox cases instead of measles cases.
#Exclude years in which cases were reported in fewer than 10 weeks from the plot.
the_disease = "Smallpox" #changed to smallpox
dat <- us_contagious_diseases %>%
        filter(!state%in%c("Hawaii","Alaska") & weeks_reporting >= 10 & disease == the_disease) %>% #added weeks_reporting filter
        mutate(rate = count / population * 10000) %>%
        mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
        filter(disease==the_disease) %>% group_by(year) %>%
        summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
        geom_line(aes(year, rate, group = state),  color = "grey50", 
                  show.legend = FALSE, alpha = 0.2, size = 1) +
        geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
        scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
        ggtitle("Cases per 10,000 by state") + 
        xlab("") + 
        ylab("") +
        geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
        geom_vline(xintercept=1963, col = "blue")
#For the state of California, make a time series plot showing rates for all diseases.
#Include only years with 10 or more weeks reporting.
#Use a different color for each disease.
#Include your aes function inside of ggplot rather than inside your geom layer.
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>% filter(state=="California", weeks_reporting >= 10) %>% #added weeks_reporting filter
        group_by(year, disease) %>%
        summarize(rate = sum(count)/sum(population)*10000) %>%
        ggplot(aes(year, rate, color=disease)) + #added color
        geom_line()

#Compute the US rate by using summarize to sum over states. Call the variable rate.
#The US rate for each disease will be the total number of cases divided by the total population.
#Remember to convert to cases per 10,000.
#You will need to filter for !is.na(population) to get all the data.
#Plot each disease in a different color.
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>% filter(!is.na(population)) %>% 
        group_by(year, disease) %>%
        summarize(rate = sum(count)/sum(population)*10000) %>%
        ggplot(aes(year, rate, color=disease)) +
        geom_line()

#Titanic survival Exercises
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
        select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
        mutate(Survived = factor(Survived),
               Pclass = factor(Pclass),
               Sex = factor(Sex))
# faceted plot for comparing sex
titanic %>%
        ggplot(aes(Age, fill = Sex)) +
        geom_density(alpha = 0.2) +
        facet_grid(Sex ~ .)
# stacked density plot with count on the y-axis
titanic %>%
        ggplot(aes(Age, y = ..count.., fill = Sex)) +
        geom_density(alpha = 0.2, position = "stack")
# filled by sex with alpha blending
titanic %>%
        ggplot(aes(Age, fill = Sex)) +
        geom_density(alpha = 0.2)
#QQ-plot of Age Distribution
params <- titanic %>%
        filter(!is.na(Age)) %>%
        summarize(mean = mean(Age), sd = sd(Age))
titanic %>%
        filter(!is.na(Age)) %>%
        ggplot(aes(sample = Age)) +
        geom_qq(dparams = params) +
        geom_abline()
#plot 1 - survival filled by sex
titanic %>%
        ggplot(aes(Survived, fill = Sex)) +
        geom_bar()
# plot 2 - survival filled by sex with position_dodge
titanic %>%
        ggplot(aes(Survived, fill = Sex)) +
        geom_bar(position = position_dodge())
#plot 3 - sex filled by survival
titanic %>%
        ggplot(aes(Sex, fill = Survived)) +
        geom_bar()
#density plot of age filled by survival status set for counts with alpha blending
titanic %>%
        ggplot(aes(Age, y = ..count.., fill = Survived)) +
        geom_density(alpha = 0.2)
#Filter fare of 0. boxplot of fare grouped by survival 
#log2 transformation of fares. Add the data points with jitter and alpha blending.
titanic %>%
        filter(Fare > 0) %>%
        ggplot(aes(Survived, Fare)) +
        geom_boxplot() +
        scale_y_continuous(trans = "log2") +
        geom_jitter(alpha = 0.2)

# barplot of passenger class filled by survival
titanic %>%
        ggplot(aes(Pclass, fill = Survived)) +
        geom_bar() +
        ylab("Proportion")
# barplot of passenger class filled by survival with position_fill
titanic %>%
        ggplot(aes(Pclass, fill = Survived)) +
        geom_bar(position = position_fill()) +
        ylab("Proportion")
# Barplot of survival filled by passenger class with position_fill
titanic %>%
        ggplot(aes(Survived, fill = Pclass)) +
        geom_bar(position = position_fill()) +
        ylab("Proportion")
#Survival by Age, Sex and Passenger Class
titanic %>%
        ggplot(aes(Age, y = ..count.., fill = Survived)) +
        geom_density(position = "stack") +
        facet_grid(Sex ~ Pclass)

#stars
library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)
head(stars)
mean(stars$magnitude)
sd(stars$magnitude)

#make a density plot of magnitude
stars %>% ggplot(aes(magnitude)) + geom_density()
#make a density plot of temp
stars %>% ggplot(aes(temp)) + geom_density()
#scatter plot  temperature on the x-axis and magnitude on the y-axis
stars %>% ggplot(aes(temp, magnitude)) + geom_point()
# flip y axis, temp log10 scale, flip x axis
stars %>%
        ggplot(aes(log10(temp), magnitude)) +
        geom_point() +
        scale_x_reverse() +
        scale_y_reverse()
# Question 7 suggests the plot from question 5 to estimate the actual temperature of a giant but the plot from question 5 takes the log base 10 of temperature
# Add text labels to the plot
stars %>% filter(star %in% c("Antares", "Castor", "Mirfak", "Polaris", "vanMaanen'sStar", "Rigel", "Deneb", "*SiriusB", "Alnitak", "Alnilam", "Betelgeuse", "Antares", "Wolf359", "G51-I5")) %>%
        ggplot(aes(log10(temp), magnitude)) +
        geom_point() +
        scale_x_reverse() +
        scale_y_reverse() +
        geom_text(aes(label = star))
#color by type
stars %>%
        ggplot(aes(log10(temp), magnitude, color = type)) +
        geom_point() +
        scale_x_reverse() +
        scale_y_reverse()

stars %>% filter(type == 'G') %>%
        ggplot(aes(log10(temp), magnitude, color = type)) +
        geom_point() +
        scale_x_reverse() +
        scale_y_reverse()

#Climate Change exercises
install.packages("dslabs")
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

#first and last year for which carbon emissions available


#time series line plot of temperature anomaly. Only include years where temperatures are reported. Save this plot to the object p
p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(year, temp_anomaly)) + geom_line()

#Change the y-axis label to be "Temperature anomaly (degrees C)". 
#Add a title, "Temperature anomaly relative to 20th century mean, 1880-2018". 
#add a text layer to the plot: the x-coordinate should be 2000, the y-coordinate should be 0.05, the text should be "20th century mean", and the text color should be blue.
p + ylab("Temperature anomaly (degrees C)") +
        ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
        geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

#Add line graphs of (ocean_anomaly) and land_anomaly. Assign different colors to the lines.
temp_carbon %>%
        ggplot(aes(year, temp_anomaly)) +
        geom_line() +
        geom_line(aes(year, land_anomaly), col = "red") +
        geom_line(aes(year, ocean_anomaly), col = "blue") +
        ylab("Temperature anomaly (degrees C)") +
        xlim(c(1880, 2018)) +
        ggtitle("Temperature anomaly on land and ocean")

# line plot of y = concentration x = year facet by gas aligned vertically
# vertical line at the year 1850
greenhouse_gases %>%
        ggplot(aes(year,concentration)) +
        geom_line() +
        facet_grid(gas, scales = "free") +
        geom_vline(aes(xintercept = 1850)) +
        ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
        ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

#time series line plot of carbon emissions (carbon_emissions) from the temp_carbon dataset. 
#The y-axis is metric tons of carbon emitted per year.
temp_carbon %>% filter(!is.na(carbon_emissions)) %>%
        ggplot(aes(year,carbon_emissions)) +
        geom_vline(aes(xintercept = 1850)) +
        geom_line()

#Make a line plot of co2 concentration over time (year), coloring by the measurement source (source). Save this plot as co2_time for later use.
co2_time <- historic_co2 %>%
        ggplot(aes(year, co2, col = source)) +
        geom_line() +
        ggtitle("Atmospheric CO2 concentration, -800,000 BC to today") +
        ylab("co2 (ppmv)")
co2_time