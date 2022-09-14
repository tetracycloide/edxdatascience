##########################################################
# Create test set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

# record start time
started.at=proc.time()
# install packages if required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

# open libraries
library(tidyverse)
library(lubridate)
library(caret)
library(data.table)
library(tinytex)
library(dplyr)

# Video Game Sales with Ratings data set:
# https://www.kaggle.com/datasets/rush4ratio/video-game-sales-with-ratings
# unzipped csv file Video_Games_Sales_as_at_22_Dec_2016.csv on github
# Repo: https://github.com/tetracycloide/edxdatascience
# files for the CYO project are stored in the CYO VGS 2016 directory
dl <- getwd()
download.file("https://github.com/tetracycloide/edxdatascience/blob/main/CYO-VGS2016/Video_Games_Sales_as_at_22_Dec_2016.csv", dl)
vgs <- fread("Video_Games_Sales_as_at_22_Dec_2016.csv")
# explore the created data frame
str(vgs)
# some fields are blank and some nas are present.  User_Score is stored as a chr but should be a number
# counts of blanks by column
sapply(vgs, function(y) sum(is.na(y)))
# looks like some blanks are being stored as blank rather than na lets fix our import to read blank strings as NAs
vgs <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv", na.strings = c("", "NA"))
# and recheck counts
sapply(vgs, function(y) sum(is.na(y)))
# User_Score is stored as character let us explore
unique(vgs$User_Score)
# looks like some NAs but mostly 0-10 scores with a decimal
# Comparing with Critic_Score
unique(vgs$Critic_Score)
# 0-100 score here let us convert user score to the same format
# fix format of user_score multiplying by 10 and using integer to match Critic_Score    
vgs <- vgs %>% mutate(User_Score = as.integer(as.numeric(User_Score)*10))
# NAs introduced by coercion but this is a good thing let's check NA counts again
sapply(vgs, function(y) sum(is.na(y)))
# now User_Score and User Count match for number of rows with NAs which makes much more sense
# next we will look at blank developers
vgs %>% filter(is.na(Developer))
# many of these entries are quite famous and look to be self published
# we will assume missing developer fields are self published for our analysis 
vgs <- vgs %>% mutate(Developer = coalesce(Developer,Publisher))
# examine ratings values
unique(vgs$Rating)
# these match common ESRB ratings but some are missing and some are out of date
# research here https://en.wikipedia.org/wiki/Entertainment_Software_Rating_Board
# K-A and E are the same, blanks will be UR
vgs <- vgs %>% mutate(Rating = coalesce(str_replace(Rating, "K-A", "E"),"UR"))
# dividing into 6 sets of data for 2 models
# model 1 to predict Global Sales
sales_validate
sales_train
sales_cv
# model 2 to predict Critic Score
critic_validate
critic_train
sales_cv