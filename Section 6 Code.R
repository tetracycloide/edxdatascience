#Data wrangling
# see working directory
getwd()
# change your working directory
setwd()
# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package="dslabs")
list.files(path)
# generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath
# copy file from dslabs package to your working directory
file.copy(fullpath, getwd())
# check if the file exists
file.exists(filename)
#readr and readxl
library(dslabs)
library(tidyverse)    # includes readr
library(readxl)
# inspect the first 3 lines
read_lines("murders.csv", n_max = 3)
# read file in CSV format
dat <- read_csv(filename)
#read using full path
dat <- read_csv(fullpath)
head(dat)
#Ex：
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files
filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat=read.csv(file.path(path, filename))
dat1=read.csv(file.path(path, filename1))
dat2=read.csv(file.path(path, filename2))
# importing using r-base functions
# filename is defined in the previous video
# read.csv converts strings to factors
dat2 <- read.csv(filename)
class(dat2$abb)
class(dat2$region)
# Downloading Files from the Internet
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

#tidying data
library(tidyverse)
library(dslabs)
data(gapminder)
# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)
# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()
# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)
# original wide data
library(tidyverse) 
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
# tidy data from dslabs
library(dslabs)
data("gapminder")
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
# gather wide data to make new tidy data
new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)
# gather all columns except country
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)
# gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)
# convert gathered column names to numeric
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)
# ggplot works on new tidy data
new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()
# spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)
# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)
# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]
# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
dat %>% separate(key, c("year", "variable_name"))
# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                 fill = "right")
# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")
# separate then spread
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 
# separate then unite
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")
# full code for tidying data
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)
# reshaping data
library(tidyverse)
library(dslabs)
# observe data
co2
#setup variables
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
#use gather to make this data tidy
co2_tidy <- gather(co2_wide,month,co2,-year)
#plot the data
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()
# admissions data
library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
#get data in the shape that has one row for each major
dat_tidy <- spread(dat, gender, admitted)
# organize in columns major, gender, key and value
tmp <- gather(admissions, key, value, admitted:applicants)
tmp
# Combine the key and gender and create a new column called column_name 
# get a variable with the following values: admitted_men, admitted_women, applicants_men and applicants_women
tmp2 <- unite(tmp, column_name, c(key, gender))
# what fucntion to reeshape tmp2 to a table with six rows five columns named 
# major, admitted_men, admitted_women, applicants_men and applicants_women
spread(tmp2, column_name, value)

# joining tables
# The join functions in the dplyr package combine two tables such that matching rows are together.
# left_join() only keeps rows that have information in the first table.
# right_join() only keeps rows that have information in the second table.
# inner_join() only keeps rows that have information in both tables.
# full_join() keeps all rows from both tables.
# semi_join() keeps the part of first table for which we have information in the second.
# anti_join() keeps the elements of the first table for which there is no information in the second.
# import US murders data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)
# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)
# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)
# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)
# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2
# experiment with different joins
left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1, tab2)
semi_join(tab1, tab2)
anti_join(tab1, tab2)
#binding data sets
bind_cols(a = 1:3, b = 4:6)
tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)
tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)
# intersect vectors or data frames
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)
# perform a union of vectors or data frames
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)
# set difference of vectors or data frames
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)
# setequal determines whether sets have the same elements, regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)

#load data and set variables
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
#inspect master
top %>% as_tibble()
#use join or bind to create a combined table
top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names
#filter salaries to 2016 and bind or join to add salary column to top_names
top_salary <- Salaries %>% 
  filter(yearID == 2016) %>% 
  right_join(top_names) %>% 
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary
#number of players in top 10 HR with awards
Awards_2016 <- AwardsPlayers %>% 
  filter(yearID == 2016)
length(intersect(Awards_2016$playerID, top_names$playerID))
#number of players not in top 10 HR with awards
length(setdiff(Awards_2016$playerID, top_names$playerID))
#Web Scraping
# import an HTML webpage into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h
tab <- h %>% html_nodes("table")
tab <- tab[[2]]
tab <- tab %>% html_table
class(tab)
tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)
# For the guacamole recipe page, we already have done this and determined that we need the following selectors:
  h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
# You can see how complex the selectors are. In any case we are now ready to extract what we want and create a list:
  guacamole <- list(recipe, prep_time, ingredients)
guacamole
# Since recipe pages from this website follow this general layout, we can use this code to create a function that extracts this information:
  get_recipe <- function(url){
    h <- read_html(url)
    recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
    prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
    ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
    return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
  } 
# and then use it on any of their webpages:
  get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")
# There are several other powerful tools provided by rvest. For example, the functions html_form(), set_values(),
# and submit_form() permit you to query a webpage from R. This is a more advanced topic not covered here.

# Load the following web page, which contains information about Major League Baseball payrolls, into R: 
#   https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm External link
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
# We learned that tables in html are associated with the table node.  Use the html_nodes() function and the table 
# node type to extract the first table. Store it in an object nodes:
nodes <- html_nodes(h, "table")
# The html_nodes() function returns a list of objects of class xml_node. We can see the content of each one using,
# for example, the html_text() function. You can see the content for an arbitrarily picked component like this:
html_text(nodes[[8]])
# If the content of this object is an html table, we can use the html_table() function to convert it to a data frame:
html_table(nodes[[8]])
# Inspect tables 1-4 as data frames to look for payroll data
sapply(nodes[1:4], html_table)    # 2, 3, 4 give tables with payroll info
# Inspect the last 3 tables as data frames
sapply(nodes[(length(nodes)-2):(length(nodes))], html_table)
# create tab_1 from entry 10 of nodes and tab_2 from entry 19
tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
# remove 1st column of tab_1, chage the column names to Team Payroll Average and full_join() tabs 1 and 2
col_names <- c("Team", "Payroll", "Average")
tab_1 <- tab_1[-1, -1]
tab_2 <- tab_2[-1,]
names(tab_2) <- col_names
names(tab_1) <- col_names
full_join(tab_1,tab_2, by = "Team")
#New data set assign tab to be the html nodes of the webpage on brexit
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- html_nodes(read_html(url), "table")
# inspect the tables starting with 1 for a 9 column table with a Date(s) conducted column
tab[[1]] %>% html_table(fill = TRUE) %>% names()    # inspect column names
tab[[2]] %>% html_table(fill = TRUE) %>% names()    # inspect column names
tab[[3]] %>% html_table(fill = TRUE) %>% names()    # inspect column names
tab[[4]] %>% html_table(fill = TRUE) %>% names()    # inspect column names
tab[[5]] %>% html_table(fill = TRUE) %>% names()    # inspect column names
tab[[6]] %>% html_table(fill = TRUE) %>% names()    # inspect column names

# String processing
# read in raw murders data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))
# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)
# #quotes and escape characters
# s <- "Hello!"    # double quotes define a string
# s <- 'Hello!'    # single quotes define a string
# s <- `Hello`    # backquotes do not
# s <- "10""    # error - unclosed quotes
# s <- '10"'    # correct
# # cat shows what the string actually looks like inside R
# cat(s)
# s <- "5'"
# cat(s)
# # to include both single and double quotes in string, escape with \
# s <- '5'10"'    # error
# s <- "5'10""    # error
# s <- '5\'10"'    # correct
# cat(s)
# s <- "5'10\""    # correct
# cat(s)

# murders_raw defined in web scraping video
# direct conversion to numeric fails because of commas
murders_raw$population[1:3]
as.numeric(murders_raw$population[1:3])
library(tidyverse)    # includes stringr
# detect whether there are commas
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))
# replace commas with the empty string and convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)
# parse_number also removes commas and converts to numeric
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head
# dat is a data from with columns 2 and 3 in format $100,000 convert to number two ways
dat %>% mutate_at(2:3, parse_number)
dat %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
  mutate_at(2:3, as.numeric)

# load raw heights data and inspect
library(dslabs)
data(reported_heights)
class(reported_heights$height)
# convert to numeric, inspect, count NAs
x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x))
# keep only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)
# calculate cutoffs that cover 99.999% of human population
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)
# keep only entries that either result in NAs or are outside the plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}
# number of problematic entries
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)
# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat
# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat
# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat
#regular expressions
# load stringr through tidyverse
library(tidyverse)
# detect whether a comma is present
pattern <- ","
str_detect(murders_raw$total, pattern) 
# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")
# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")
# highlight the first occurrence of a pattern
str_view(s, pattern)
# highlight all instances of a pattern
str_view_all(s, pattern)
# character classes anchors and quantifiers
# s was defined in the previous video
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"
# [56] means 5 or 6
str_view(s, "[56]")
# [4-7] means 4, 5, 6 or 7
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")
# ^ means start of string, $ means end of string
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)
# curly braces define quantifiers: 1 or 2 digits 
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)
# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)
# search and replace with regex
# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))
# inspect examples of entries with problems
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")
# replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()
# R does not ignore whitespace
identical("Hi", "Hi ")
# \\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)
# * means 0 or more instances of a character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")
# test how *, ? and + differ
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))
# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()
# groups with regex
# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"
# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
# demonstrate the effect of groups
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)
# demonstrate difference between str_match and str_extract
str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups)
# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")
# final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head
# testing and improving
# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}
# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format
# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)
converted[!index]    # show problems
# given this vector identify the pattern of numeric characters or the text ft
s <- c("70" ,      "5 ft"  ,   "4'11"   ,  ""    ,     "."     ,   "Six feet")
pattern <- "\\d|ft"
str_view_all(s, pattern)
# given the vector check with a pattern of at least one lower case letter
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)
# given the vector check with a pattern of an upper case letter at the end of the string
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)
# given the vector check with a patter of 4 or 5 lowercase letters
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)
animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "mo*"
str_detect(animals, pattern)
pattern <- "mo?"
str_detect(animals, pattern)
pattern <- "mo+"
str_detect(animals, pattern)
pattern <- "moo*"
str_detect(animals, pattern)
# vector
schools <- c("U. Kentucky","Univ New Hampshire","Univ. of Massachusetts","University Georgia","U California","California State University")
# clean up to replace with full name accounting for abbreviations of University
schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")
# using not_inches and problems from before
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
converted[!index]
# work up a pattern to fix the remaining issues
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)
converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)
# first example - normally formatted heights
s <- c("5'10", "6'1")
tab <- data.frame(x = s)
# the separate and extract functions behave similarly
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")
# second example - some heights with unusual formats
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)
# separate fails because it leaves in extra characters, but extract keeps only the digits because of regex groups
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")






# Assessment Puerto Rico Hurricane Mortality: Part 1
library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits
# load data file
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
# open the file
system("cmd.exe", input = paste("start", fn))
# read fn and store in txt
txt <- pdf_text(fn)
# extract the 9th page and split into rows using new line characrer \n
x <- str_split(txt[9], "\n")
# class of x
class(x)
# size of x
length(x)
# define s as the first entry of x and check class and length
s <- x[[1]]
class(s)
length(s)
# trim s
s <- str_trim(s)
s[1]    # print string, visually inspect last character
# find the header in the string by looking for 2015
header_index <- str_which(s, "2015")[1]
header_index
# split header row into month and header
tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
month <- tmp[1]
header <- tmp[-1]
month
# find index for summary statistics row using "Total"
tail_index  <- str_which(s, "Total")
tail_index
# create objec n with count of numbers in each row
n <- str_count(s, "\\d+")
sum(n == 1)
# remove everything before header_index, after tail_index, and where n=1
out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s)