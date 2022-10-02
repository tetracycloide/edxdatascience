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
if(!require(gridExtra)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(digest)) install.packages("digest", repos = "http://cran.us.r-project.org")

# open libraries
library(tidyverse)
library(lubridate)
library(caret)
library(data.table)
library(tinytex)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(randomForest)
library(digest)

# Video Game Sales with Ratings data set:
# https://www.kaggle.com/datasets/rush4ratio/video-game-sales-with-ratings
# unzipped csv file Video_Games_Sales_as_at_22_Dec_2016.csv on github
# Repo: https://github.com/tetracycloide/edxdatascience
# files for the CYO project are stored in the CYO VGS 2016 directory
# here we inport and convert any blanks or 'NA' strings to NAs while inporting
vgs <- fread("https://github.com/tetracycloide/edxdatascience/raw/main/CYO-VGS2016/DataSource/Video_Games_Sales_as_at_22_Dec_2016.csv", 
             na.strings = c("", "NA","N/A","tbd"), stringsAsFactors = TRUE)
# explore the created data frame
str(vgs)
# check na counts by column
sapply(vgs, function(y) sum(is.na(y)))%>% knitr::kable()
# Blank names jumps out first as anomalous
vgs %>% filter(is.na(Name))
# no ratings or scores or genre either and only 2 data points we will discard them
vgs <- vgs %>% filter(!is.na(Name))
# check for anomalous years
unique(vgs$Year_of_Release)
# blank years will interfere with later analysis
vgs %>% filter(is.na(Year_of_Release))
# some titles have years from yearly release series
vgs %>% filter(is.na(Year_of_Release), str_detect(Name,"(\\d{4})"))
# in this case these look like release years we can borrow
# we can use these to fill the Year of Release field where available
vgs <- vgs %>% 
  mutate(Year_of_Release = coalesce(Year_of_Release,as.integer(str_extract(Name,"(\\d{4})")))) %>%
  filter(!is.na(Year_of_Release))
# 2017-2020 release years are suspect, data set is from 2016.  Pre-release listings?
vgs %>% filter(Year_of_Release > 2016)
# data for these release years insufficent to be included
vgs <- vgs %>% filter(Year_of_Release <= 2016)
# explore missing publishers
vgs %>% filter(is.na(Publisher))
# we will assume missing publisher fields are self published for our analysis 
vgs <- vgs %>% mutate(Publisher = coalesce(Publisher, Developer))
# looks like anthologies and videos mostly we will discard
vgs <- vgs %>% filter(!is.na(Publisher))
# User_Score and Critic_score seem similar but are stored differently let's explor
unique(vgs$User_Score)
# l0-10 scores with precision of 0.1
# Comparing with Critic_Score
unique(vgs$Critic_Score)
# 0-100 score here let us convert user score to the same format
# fix format of user_score multiplying by 10 and using integer to match Critic_Score    
vgs <- vgs %>% mutate(User_Score = as.integer(as.numeric(User_Score)*10))
# next we will look at blank developers
vgs %>% filter(is.na(Developer))
# many of these entries are quite famous and look to be self published
# we will assume missing developer fields are self published for our analysis 
vgs <- vgs %>% mutate(Developer = coalesce(Developer,Publisher))
# check for counts
sapply(vgs, function(y) sum(is.na(y)))
# Developer is fully populated now
# examine ratings values
vgs %>% group_by(Rating) %>% summarize(Count = n()) %>% knitr::kable()
# these match common ESRB ratings but some are missing and some are out of date
# research here: https://en.wikipedia.org/wiki/Entertainment_Software_Rating_Board
# research reveals K-A and E are the rating under different names
# find rating of RP or AO in data
vgs %>% filter(Rating=="RP"|Rating=="AO") %>% select (Name, Year_of_Release, Rating)
# find similar to missing RP rating
vgs %>% filter(Publisher=="Paradox Interactive",Genre == "Strategy", Platform == "PC") %>% select(Name, Genre, Developer, Publisher, Rating)
# we will assume blanks are unrated and mark them as "UR"
vgs <- vgs %>% mutate(Rating = as.factor(coalesce(str_replace_all(Rating, c("K-A"="E","EC"="E","AO"="M","RP"="T")),"UR")))
# check counts
sapply(vgs, function(y) sum(is.na(y)))
# ratings fully populated

# year effect on data quality start with counts by year
vgs %>% group_by(Year_of_Release) %>% summarize(count = n()) %>% 
  ggplot(aes(x = Year_of_Release, y = count, color = "Year_of_Release")) +
  theme_solarized(base_size = 14) +
  ggtitle("Games Released per Year") +
  geom_line(show.legend=FALSE, size = 1.25) +
  theme(axis.title = element_blank())
# now sales by year with averages and error bars
vgs %>% group_by(Year_of_Release) %>% 
  summarize(n = n(), 
            Average_Sales = mean(Global_Sales), 
            SE = sd(Global_Sales)/sqrt(n()))  %>%
  ggplot(aes(x = Year_of_Release, y = Average_Sales, 
             ymin = Average_Sales - 2*SE, ymax = Average_Sales + 2*SE)) +
  ggtitle("Average Sales per Year with Error Bars") +
  geom_point(color="#6c71c4") +
  geom_errorbar(color="#6c71c4") +
  theme_solarized(base_size = 14) +
  theme(axis.title = element_blank())
# prior to 1997 sample size small and noisy bad for our model
vgs <- vgs %>% filter(Year_of_Release >= 1997)
# results of cleaning
vgs %>% group_by(Year_of_Release) %>% 
  summarize(n = n(), 
            Average_Sales = mean(Global_Sales), 
            SE = sd(Global_Sales)/sqrt(n()))  %>%
  ggplot(aes(x = Year_of_Release, y = Average_Sales, 
             ymin = Average_Sales - 2*SE, ymax = Average_Sales + 2*SE)) +
  ggtitle("Average Sales per Year with Error Bars") +
  geom_point(color="#6c71c4") +
  geom_errorbar(color="#6c71c4") +
  theme_solarized(base_size = 14) +
  theme(axis.title = element_blank())


# Genre effect on sales plot total global sales by genre
plot_gb_by_genre <- vgs %>%
  group_by(Genre) %>%
  summarise(Global = sum(Global_Sales)) %>%
  ggplot(aes(Global,Genre,fill=Global)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("Global Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
plot_gb_by_genre
# Sales appear to vary significantly by genre how to region affect this
# create the same plot for each of the 4 regions
plot_na_by_genre <- vgs %>%
  group_by(Genre) %>%
  summarise(NA_ = sum(NA_Sales)) %>%
  ggplot(aes(NA_,Genre,fill=NA_)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("North America Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
plot_eu_by_genre <- vgs %>%
  group_by(Genre) %>%
  summarise(EU = sum(EU_Sales)) %>%
  ggplot(aes(EU,Genre,fill=EU)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("Europe Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
plot_jp_by_genre <- vgs %>%
  group_by(Genre) %>%
  summarise(JP = sum(JP_Sales)) %>%
  ggplot(aes(JP,Genre,fill=JP)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("Japan Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
plot_ot_by_genre <- vgs %>%
  group_by(Genre) %>%
  summarise(Other = sum(Other_Sales)) %>%
  ggplot(aes(Other,Genre,fill=Other)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("Other Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
# compare all the regional plots
grid.arrange(plot_na_by_genre,plot_eu_by_genre,plot_jp_by_genre,plot_ot_by_genre)
# similar for NA/Eu/Other but JP significantly different

# Platform effect on sales plot total global sales by platform
plot_gb_by_platform <- vgs %>%
  group_by(Platform) %>%
  summarise(Global = sum(Global_Sales)) %>%
  ggplot(aes(Global,Platform,fill=Global)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("Global Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
plot_gb_by_platform
# Sales appear to vary significantly by Platform how to region affect this
# create the same plot for each of the 4 regions
plot_na_by_platform <- vgs %>%
  group_by(Platform) %>%
  summarise(Global = sum(Global_Sales), NA_ = sum(NA_Sales)) %>%
  top_n(.,10,Global) %>%
  ggplot(aes(NA_,Platform,fill=NA_)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("North America Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
plot_eu_by_platform <- vgs %>%
  group_by(Platform) %>%
  summarise(Global = sum(Global_Sales), EU = sum(EU_Sales)) %>%
  top_n(.,10,Global) %>%
  ggplot(aes(EU,Platform,fill=EU)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("Europe Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
plot_jp_by_platform <- vgs %>%
  group_by(Platform) %>%
  summarise(Global = sum(Global_Sales), JP = sum(JP_Sales)) %>%
  top_n(.,10,Global) %>%
  ggplot(aes(JP,Platform,fill=JP)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("Japan Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
plot_ot_by_platform <- vgs %>%
  group_by(Platform) %>%
  summarise(Global = sum(Global_Sales), Other = sum(Other_Sales)) %>%
  top_n(.,10,Global) %>%
  ggplot(aes(Other,Platform,fill=Other)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("Other Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
# compare all the regional plots
grid.arrange(plot_na_by_platform,plot_eu_by_platform,plot_jp_by_platform,plot_ot_by_platform)
# again significant differences by region based on platform

# Rating effect on sales plot average global sales by genre
plot_gb_by_Rating <- vgs %>%
  group_by(Rating) %>%
  summarise(Global = sum(Global_Sales)) %>%
  ggplot(aes(Global,Rating,fill=Global)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("Global Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
plot_gb_by_Rating
# Sales appear to vary significantly by Rating how to region affect this
# create the same plot for each of the 4 regions
plot_na_by_Rating <- vgs %>%
  group_by(Rating) %>%
  summarise(NA_ = sum(NA_Sales)) %>%
  ggplot(aes(NA_,Rating,fill=NA_)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("North America Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
plot_eu_by_Rating <- vgs %>%
  group_by(Rating) %>%
  summarise(EU = sum(EU_Sales)) %>%
  ggplot(aes(EU,Rating,fill=EU)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("Europe Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
plot_jp_by_Rating <- vgs %>%
  group_by(Rating) %>%
  summarise(JP = sum(JP_Sales)) %>%
  ggplot(aes(JP,Rating,fill=JP)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("Japan Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
plot_ot_by_Rating <- vgs %>%
  group_by(Rating) %>%
  summarise(Other = sum(Other_Sales)) %>%
  ggplot(aes(Other,Rating,fill=Other)) +
  theme_solarized(base_size = 14) +
  geom_col(show.legend = FALSE) +
  ggtitle("Other Sales") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
# compare all the regional plots
grid.arrange(plot_na_by_Rating,plot_eu_by_Rating,plot_jp_by_Rating,plot_ot_by_Rating)
# Heavy preference in all regions for E rated games
# 10+ is a new rating which explains the under-representation
# examine averages by developer
vgs %>% group_by(Developer) %>%
  summarize(average_sales = mean(Global_Sales)) %>%
  ggplot(aes(average_sales)) +
  geom_histogram(bins=129,color = "#073642",fill="#dc322f") +
  ggtitle("Averge Sales by Developer") +
  theme_solarized(base_size=16, light=FALSE) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank())
# some very big outliers top 10 devs
vgs %>% group_by(Developer) %>%
  summarize(average_sales = mean(Global_Sales)) %>%
  select(Developer, average_sales) %>%
  top_n(10,average_sales) %>%
  arrange(desc(average_sales))
# try a log 10 scale 
vgs %>% group_by(Developer) %>%
  summarize(average_sales = mean(Global_Sales)) %>%
  ggplot(aes(average_sales)) +
  geom_histogram(bins=129,color = "#073642",fill="#dc322f") +
  ggtitle("Averge Sales by Developer (Log 10 Scale)") +
  scale_x_log10() +
  theme_solarized(base_size=16, light=FALSE) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank())
# near 0 appears fairly normally distributed with a long tail skewing right
# examine averages by publishers
vgs %>% group_by(Publisher) %>%
  summarize(average_sales = mean(Global_Sales)) %>%
  ggplot(aes(average_sales)) +
  geom_histogram(bins=29,color = "#073642",fill="#268bd2") +
  ggtitle("Averge Sales by Publisher") +
  theme_solarized(base_size=16, light=FALSE) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank())
# again we see a right skew but not nearly as extreme as with developers
vgs %>% group_by(Publisher) %>%
  summarize(average_sales = mean(Global_Sales)) %>%
  select(Publisher, average_sales) %>%
  top_n(10) %>%
  arrange(desc(average_sales))
# top 10 values are much much lower than developers
# try a log 10 scale again
vgs %>% group_by(Publisher) %>%
  summarize(average_sales = mean(Global_Sales)) %>%
  ggplot(aes(average_sales)) +
  geom_histogram(bins=29,color = "#073642",fill="#268bd2") +
  ggtitle("Averge Sales by Publisher (Log 10 Scale)") +
  scale_x_log10() +
  theme_solarized(base_size=16, light=FALSE) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank())
# less normal here than developers but a slight bit of rounding
# check overall distribution of sales
vgs %>% 
  ggplot(aes(Global_Sales)) +
  geom_histogram(bins=25,color = "#073642",fill="#dc322f") +
  ggtitle("Overall Sales Distribution") +
  theme_solarized(base_size=16, light=FALSE) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank())
# these appear very tightly grouped around 0 let's log adjust again
vgs %>% 
  ggplot(aes(Global_Sales)) +
  geom_histogram(bins=25,color = "#073642",fill="#dc322f") +
  ggtitle("Overall Sales Distribution (Log 10 Scale") +
  scale_x_log10() +
  theme_solarized(base_size=16, light=FALSE) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank())
# this appears much more normally distributed we will log adjust our prices

# identified possible predictors: Platform, Year_of_Release, Genre, Publisher, Developer, Rating
# identified price needed a log10 adjustment
vgs <- vgs %>% mutate(Global_Sales = log10(Global_Sales)) %>% select(2,3,4,5,15,16,10)

# data for revenue prediction model
# sales_validation set will be 10% of vgs data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = vgs$Global_Sales, times = 1, p = 0.1, list = FALSE)
sales_train <- vgs[-test_index,]
temp <- vgs[test_index,]

# Make sure all predictors in sales_validation set are also in sales_train set
sales_validation <- temp %>% 
  semi_join(sales_train, by = "Platform") %>%
  semi_join(sales_train, by = "Year_of_Release") %>%
  semi_join(sales_train, by = "Genre") %>%
  semi_join(sales_train, by = "Publisher") %>%
  semi_join(sales_train, by = "Developer") %>%
  semi_join(sales_train, by = "Rating")

# Add rows removed from sales_validation set back into sales_train set
removed <- anti_join(temp, sales_validation, by = c("Platform", "Year_of_Release", "Genre", "Publisher", "Developer", "Rating", "Global_Sales"))
sales_train <- rbind(sales_train, removed)

# divide sales_train into cross validation and train tests
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = sales_train$Global_Sales, times = 1, p = 0.2, list = FALSE)
sales_train_cv <- sales_train[-test_index,]
temp <- sales_train[test_index,]

# Make sure all predictors in sales_test_cv set are also in sales_train_cv set
sales_test_cv <- temp %>% 
  semi_join(sales_train_cv, by = "Platform") %>%
  semi_join(sales_train_cv, by = "Year_of_Release") %>%
  semi_join(sales_train_cv, by = "Genre") %>%
  semi_join(sales_train_cv, by = "Publisher") %>%
  semi_join(sales_train_cv, by = "Developer") %>%
  semi_join(sales_train_cv, by = "Rating")

# Add rows removed from sales_test_cv set back into sales_train_cv set
removed <- anti_join(temp, sales_test_cv, by = c("Platform", "Year_of_Release", "Genre", "Publisher", "Developer", "Rating", "Global_Sales"))
sales_train_cv <- rbind(sales_train_cv, removed)

# dump extra variables
rm(test_index,temp,removed)

# create RMSE function to evaluate model results
rmse <- function(x, y){
  sqrt(mean((x - y)^2))
}
# model 1, use mean as prediction, creation
sales_mu <- mean(sales_train_cv$Global_Sales)
# RMSE of guessing mean
naive_rmse <- rmse(sales_test_cv$Global_Sales, sales_mu )
# We see that this error is fairly large
naive_rmse
# add naive_rmse to a table of rmse results
rmse_results <- tibble(Method = "Just the average", RMSE = naive_rmse)
# add dev effect to model
dev_avgs <- sales_train_cv %>% 
  group_by(Developer) %>% 
  summarize(b_dev = mean(Global_Sales - sales_mu))
# cross validate
model_1_rmse <- (sales_test_cv %>%
                   left_join(dev_avgs,by='Developer') %>%
                   mutate(pred = sales_mu + b_dev) %>%
                   summarize(rmse(.$pred,.$Global_Sales)))[1,1]
# add row to results table
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Dev Model",
                                 RMSE = model_1_rmse ))
# add publisher to model 
publisher_avgs <- sales_train_cv %>% 
  left_join(dev_avgs,by='Developer') %>%
  group_by(Publisher) %>% 
  summarize(b_pub = mean(Global_Sales - sales_mu - b_dev))
# cross validate
model_2_rmse <- (sales_test_cv %>%
                   left_join(dev_avgs,by='Developer') %>%
                   left_join(publisher_avgs,by='Publisher') %>%
                   mutate(pred = sales_mu + b_dev + b_pub) %>%
                   summarize(rmse(.$pred,.$Global_Sales)))[1,1]
# add row to results table
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Dev + Pub Model",
                                 RMSE = model_2_rmse )) 
# add genre to model
genre_avgs <- sales_train_cv %>%
  left_join(dev_avgs,by='Developer') %>%
  left_join(publisher_avgs,by='Publisher') %>% 
  group_by(Genre) %>% 
  summarize(b_g = mean(Global_Sales - sales_mu - b_dev - b_pub))
# cross validate
model_3_rmse <- (sales_test_cv %>%
                   left_join(dev_avgs,by='Developer') %>%
                   left_join(publisher_avgs,by='Publisher') %>%
                   left_join(genre_avgs,by='Genre') %>%
                   mutate(pred = sales_mu + b_dev + b_pub + b_g) %>%
                   summarize(rmse(.$pred,.$Global_Sales)))[1,1]
# add row to results table
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Dev + Pub + Genre Model",
                                 RMSE = model_3_rmse ))
# add platform to model
platform_avgs <- sales_train_cv %>%
  left_join(dev_avgs,by='Developer') %>%
  left_join(publisher_avgs,by='Publisher') %>%
  left_join(genre_avgs,by='Genre') %>% 
  group_by(Platform) %>% 
  summarize(b_plat = mean(Global_Sales - sales_mu - b_dev - b_pub - b_g))
# cross validate
model_4_rmse <- (sales_test_cv %>%
                   left_join(dev_avgs,by='Developer') %>%
                   left_join(publisher_avgs,by='Publisher') %>%
                   left_join(genre_avgs,by='Genre') %>% 
                   left_join(platform_avgs,by='Platform') %>%
                   mutate(pred = sales_mu + b_dev + b_pub + b_g + b_plat) %>%
                   summarize(rmse(.$pred,.$Global_Sales)))[1,1]
# add row to results table
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Dev + Pub + Genre + Platform Model",
                                 RMSE = model_4_rmse ))
# rating to model 
rating_avgs <- sales_train_cv %>%
  left_join(dev_avgs,by='Developer') %>%
  left_join(publisher_avgs,by='Publisher') %>%
  left_join(genre_avgs,by='Genre') %>% 
  left_join(platform_avgs,by='Platform') %>% 
  group_by(Rating) %>% 
  summarize(b_rt = mean(Global_Sales - sales_mu - b_dev - b_pub - b_g - b_plat))
# cross validate
model_5_rmse <- (sales_test_cv %>%
                   left_join(dev_avgs,by='Developer') %>%
                   left_join(publisher_avgs,by='Publisher') %>%
                   left_join(genre_avgs,by='Genre') %>% 
                   left_join(platform_avgs,by='Platform') %>% 
                   left_join(rating_avgs,by='Rating') %>%
                   mutate(pred = sales_mu + b_dev + b_pub + b_g + b_plat + b_rt) %>%
                   summarize(rmse(.$pred,.$Global_Sales)))[1,1]
# add rmse to table
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Dev + Pub + Genre + Platform + Rating Model",
                                 RMSE = model_5_rmse ))
# adding year of release to the model 5
year_avgs <- sales_train_cv %>%
  left_join(dev_avgs,by='Developer') %>%
  left_join(publisher_avgs,by='Publisher') %>%
  left_join(genre_avgs,by='Genre') %>% 
  left_join(platform_avgs,by='Platform') %>%
  left_join(rating_avgs,by='Rating') %>%
  group_by(Year_of_Release) %>% 
  summarize(b_yr = mean(Global_Sales - sales_mu - b_dev - b_pub - b_g - b_plat - b_rt))
# cross validate
model_6_rmse <- (sales_test_cv %>%
                   left_join(dev_avgs,by='Developer') %>%
                   left_join(publisher_avgs,by='Publisher') %>%
                   left_join(genre_avgs,by='Genre') %>% 
                   left_join(platform_avgs,by='Platform') %>% 
                   left_join(year_avgs,by='Year_of_Release') %>%
                   left_join(rating_avgs,by='Rating') %>%
                   mutate(pred = sales_mu + b_dev + b_pub + b_g + b_plat + b_rt + b_yr) %>%
                   summarize(rmse(.$pred,.$Global_Sales)))[1,1]
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Dev + Pub + Genre + Platform + Rating + Year Model",
                                 RMSE = model_6_rmse ))
# final table
rmse_results %>% knitr::kable()
# we see the best model is model 6 let us apply regularization
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(sales_train_cv$Global_Sales)
  b_dev <- sales_train_cv %>%
    group_by(Developer) %>%
    summarize(b_dev = sum(Global_Sales - mu)/(n()+l))
  b_pub <- sales_train_cv %>% 
    left_join(b_dev, by="Developer") %>%
    group_by(Publisher) %>%
    summarize(b_pub = sum(Global_Sales - b_dev - mu)/(n()+l))
  b_g <- sales_train_cv %>%
    left_join(b_dev, by="Developer") %>%
    left_join(b_pub, by="Publisher") %>%
    group_by(Genre) %>%
    summarize(b_g = sum(Global_Sales - b_dev - b_pub - mu)/(n()+l))
  b_plat <- sales_train_cv %>%
    left_join(b_dev, by="Developer") %>%
    left_join(b_pub, by="Publisher") %>%
    left_join(b_g, by="Genre") %>%
    group_by(Platform) %>%
    summarize(b_plat = sum(Global_Sales - b_dev - b_pub - b_g - mu)/(n()+l))
  b_rt <- sales_train_cv %>%
    left_join(b_dev, by="Developer") %>%
    left_join(b_pub, by="Publisher") %>%
    left_join(b_g, by="Genre") %>% 
    left_join(b_plat,by="Platform") %>%
    group_by(Rating) %>%
    summarize(b_rt = sum(Global_Sales - b_dev - b_pub - b_g - b_plat - mu)/(n()+l))
  b_y <- sales_train_cv %>% 
    left_join(b_dev, by = "Developer") %>%
    left_join(b_pub, by = "Publisher") %>%
    left_join(b_g, by = "Genre") %>%
    left_join(b_plat, by = "Platform") %>%
    left_join(b_rt, by = "Rating") %>%
    group_by(Year_of_Release) %>%
    summarize(b_y = sum(Global_Sales - b_dev - b_pub - b_g - b_plat - b_rt - mu)/(n()+l))
  predicted_global_sales <- 
    sales_test_cv %>% 
    left_join(b_dev, by = "Developer") %>%
    left_join(b_pub, by = "Publisher") %>%
    left_join(b_g, by = "Genre") %>%
    left_join(b_plat, by = "Platform") %>%
    left_join(b_rt, by = "Rating") %>%
    left_join(b_y, by = "Year_of_Release") %>%
    mutate(pred = mu + b_dev + b_pub + b_g + b_plat + b_rt + b_y) %>%
    .$pred
  return(RMSE(predicted_global_sales, sales_test_cv$Global_Sales))
})
# plot to visualize effect of lambda on RMSE
as.data.frame(list(lamdas = lambdas,rmses = rmses)) %>%
  ggplot(aes(x=lamdas,y=rmses)) + 
  geom_point(show.legend=FALSE,color = "#6c71c4") +
  theme_solarized(base_size = 14)
# store best lambda and add newest RMSE to table of results
lambda <- lambdas[which.min(rmses)]
# store best rmse from regularization to table
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Regularized Dev + Pub + Genre + Platform + Year Model",  
                                 RMSE = min(rmses)))
# full rmse table from model building
rmse_results %>% knitr::kable()
# with our CV complete, model and lambda set, apply model to full train set
# mean of full train set
mu <- mean(sales_train$Global_Sales)
# first a naive guess of the simple mean for comparison
final_naive_rmse <- rmse(sales_validation$Global_Sales, sales_mu )
# add result to table
final_rmse_results <- tibble(Method = "Just the average", RMSE = final_naive_rmse)
# create full model on full train set with lambda
b_dev <- sales_train %>%
  group_by(Developer) %>%
  summarize(b_dev = sum(Global_Sales - mu)/(n()+lambda))
b_pub <- sales_train %>% 
  left_join(b_dev, by="Developer") %>%
  group_by(Publisher) %>%
  summarize(b_pub = sum(Global_Sales - b_dev - mu)/(n()+lambda))
b_g <- sales_train %>%
  left_join(b_dev, by="Developer") %>%
  left_join(b_pub, by="Publisher") %>%
  group_by(Genre) %>%
  summarize(b_g = sum(Global_Sales - b_dev - b_pub - mu)/(n()+lambda))
b_plat <- sales_train %>%
  left_join(b_dev, by="Developer") %>%
  left_join(b_pub, by="Publisher") %>%
  left_join(b_g, by="Genre") %>%
  group_by(Platform) %>%
  summarize(b_plat = sum(Global_Sales - b_dev - b_pub - b_g - mu)/(n()+lambda))
b_rt <- sales_train %>%
  left_join(b_dev, by="Developer") %>%
  left_join(b_pub, by="Publisher") %>%
  left_join(b_g, by="Genre") %>% 
  left_join(b_plat,by="Platform") %>%
  group_by(Rating) %>%
  summarize(b_rt = sum(Global_Sales - b_dev - b_pub - b_g - b_plat - mu)/(n()+lambda))
b_y <- sales_train %>% 
  left_join(b_dev, by = "Developer") %>%
  left_join(b_pub, by = "Publisher") %>%
  left_join(b_g, by = "Genre") %>%
  left_join(b_plat, by = "Platform") %>%
  left_join(b_rt, by = "Rating") %>%
  group_by(Year_of_Release) %>%
  summarize(b_y = sum(Global_Sales - b_dev - b_pub - b_g - b_plat - b_rt - mu)/(n()+lambda))
predicted_global_sales <- sales_validation %>% 
  left_join(b_dev, by = "Developer") %>%
  left_join(b_pub, by = "Publisher") %>%
  left_join(b_g, by = "Genre") %>%
  left_join(b_plat, by = "Platform") %>%
  left_join(b_rt, by = "Rating") %>%
  left_join(b_y, by = "Year_of_Release") %>%
  mutate(pred = mu + b_dev + b_pub + b_g + b_plat + b_rt + b_y) %>%
  .$pred
# calculate final RMSE against validation set
final_linear_rmse <- RMSE(predicted_global_sales, sales_validation$Global_Sales)
# save final rmse to table
final_rmse_results <- bind_rows(final_rmse_results,
                          tibble(Method="Dev + Pub + Genre + Platform + Year with regularization Model",
                                 RMSE = final_linear_rmse ))
# final rmse results table
final_rmse_results %>% knitr::kable()
# an improvement over the naive estimate

# lets try another model, random forests
# start with hashing of developer and publisher names because there are to many categories for RF
sales_train_cv <- sales_train_cv %>%
  rowwise() %>%
  mutate(digest_dev = digest(Developer, algo = 'md5'), 
         digest_pub = digest(Publisher,algo='md5'),
         digest2int_dev = digest2int(digest_dev),
         digest2int_pub = digest2int(digest_pub)) %>%
  as.data.frame(.)
sales_test_cv <- sales_test_cv %>%
  rowwise() %>%
  mutate(digest_dev = digest(Developer, algo = 'md5'), 
         digest_pub = digest(Publisher,algo='md5'),
         digest2int_dev = digest2int(digest_dev),
         digest2int_pub = digest2int(digest_pub)) %>%
  as.data.frame(.)
sales_validation <- sales_validation %>%
  rowwise() %>%
  mutate(digest_dev = digest(Developer, algo = 'md5'), 
         digest_pub = digest(Publisher,algo='md5'),
         digest2int_dev = digest2int(digest_dev),
         digest2int_pub = digest2int(digest_pub)) %>%
  as.data.frame(.)

# set y
rf_y <- sales_train_cv$Global_Sales
# linear regression suggests Developer, Publisher, Genre, Platform, Rating, and Year_of_Release as variables
rf_x <- sales_train_cv %>% select(digest2int_dev,digest2int_pub,Genre,Platform,Rating,Year_of_Release)
# set seeed for repeatability
set.seed(1, sample.kind="Rounding")
# train the random forest, limit sample size to improve performance
train_rf <- randomForest(rf_x,rf_y,sampsize=6500)
# calculate RMSE 
rf_rmse <- rmse(predict(train_rf, sales_test_cv),sales_test_cv$Global_Sales)
# plot train set to examine number of trees for cv
as.data.frame(train_rf$mse) %>% mutate(Error = train_rf$mse, num_trees=row_number()) %>%
  ggplot(aes(x=num_trees,y=Error,color = "num_trees")) +
  theme_solarized(base_size = 14) +
  ggtitle("Mean Squared Error vs # of Trees") +
  geom_line(show.legend=FALSE, size = 1.25) +
  theme(axis.title = element_blank())
# use cross validation to choose mtry
mtries <- 1:6
rmses <- sapply(mtries, function(n)
{
  set.seed(1, sample.kind="Rounding")
  cv_tune <- randomForest(rf_x,rf_y,
                          sampsize=6500,
                          ntree=100,
                          mtry = n)  
  return(rmse(predict(cv_tune, sales_test_cv),sales_test_cv$Global_Sales))
})
# plot to visualize effect of mtry on RMSE
as.data.frame(list(mtries = mtries,rmses = rmses)) %>%
  ggplot(aes(x=mtries,y=rmses)) + 
  geom_point(show.legend=FALSE,color = "#6c71c4") +
  theme_solarized(base_size = 14)
# store best mtry
mt <- mtries[which.min(rmses)]
# lift sample size and raise ntree limits for final tune
# this will take several minutes
set.seed(1, sample.kind="Rounding")
train_rf_tuned <- randomForest(rf_x,rf_y,
                               mtry = mt,
                               ntree=2500)
# calculate RMSE
rf_rmse_tuned <- rmse(predict(train_rf_tuned, sales_test_cv),sales_test_cv$Global_Sales)
# store rmse from final rf model to table
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Tuned Random forest Model",  
                                 RMSE = min(rmses)))
# full rmse table from model building
rmse_results %>% knitr::kable()

# calculate final RMSE against validation set
final_rf_rmse <- rmse(predict(train_rf_tuned, sales_validation),sales_validation$Global_Sales)
# save final rmse to table 
final_rmse_results <- bind_rows(final_rmse_results,
                                tibble(Method="RF with all varaibles",
                                       RMSE = final_rf_rmse ))
# final table
final_rmse_results %>% knitr::kable()
# time taken typical result on local hardware ~10:13 elapsed
timetaken(started.at)