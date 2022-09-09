##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes
started.at=proc.time()
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(lubridate)
library(caret)
library(data.table)

# MovieLens 10M data set:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# test set will be 10% of edx data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from test set back into edx set
removed <- anti_join(temp, test)
train <- rbind(train, removed)

rm(test_index, temp, removed)

# explore the data
# dimensions
dim(edx)
# basic look at ratings
edx %>% filter(rating == 0) %>% tally()
edx %>% filter(rating == 3) %>% tally()
# number of unique movies
n_distinct(edx$movieId)
# number of unique users
n_distinct(edx$userId)
# number of movie ratings in a sample of common genres
genres <- unique((edx %>% filter(!str_detect(edx$genres, "\\|")))$genres)
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})
# number of ratings by titles
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
# chart of number of ratings by titles log scaled
edx %>% group_by(movieId) %>%
  summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(bins=30,color = "black") +
  scale_x_log10()
# most given ratings
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))
# preference for full star ratings?
# counts of each given rating
edx %>% group_by(rating) %>% summarize(count = n())
# visualization of preference for full star ratings
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()
# create RMSE funciton to evaluate model results
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
# find average rating in edx set
mu_hat <- mean(train$rating)
# print the average rating from the edx test est
mu_hat
# RMSE of guessing average rating against train set
naive_rmse <- RMSE(test$rating, mu_hat)
# We see that this error is fairly large, more than 1.
naive_rmse
# add naive_rmse to a table of rmse results
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
# calculate a movie factor, average difference between a movies score and the overall average score
movie_avgs <- train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_hat))
# histogram of movie factors
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))
# prediction based on per-movie average combined with overall average
predicted_ratings_MEM <- mu_hat + test %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
# RMSE of new predictions
model_1_rmse <- RMSE(predicted_ratings_MEM, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
# plot of mean ratings by user
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black") +
  geom_vline(xintercept = mean(edx$rating))
# calculate a user factor, average difference between a user's score and overall average including movie factor
user_avgs <- train %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))
# predictions based on user factor and movie factor
predicted_ratings_UEM <- test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  .$pred
# RMSE of model 2
model_2_rmse <- RMSE(predicted_ratings_UEM, test$rating)
# added to results table
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
# plot of genre effects, mean ratings by genre
edx %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating)) %>%
  ggplot(aes(b_g)) + 
  geom_histogram(bins = 30, color = "black")
# calculate genre factor, average difference between genre's score and overall average include u and i factors
genre_avgs <- train %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u))
# predictions based on genre, user, and movie effect factors
predicted_ratings_GEM <- test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  .$pred
# RMSE of model 3
model_3_rmse <- RMSE(predicted_ratings_GEM, test$rating)
# added to results table
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + Genre Effects Model",  
                                     RMSE = model_3_rmse ))
# explore relationship between time of rating and rating value
# plot time effects on the ratings
edx %>% mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()
# there appears to be a small effect
# explore time difference between movie release and rating
# check the results of our string match are returning 4 digit years
unique(mutate(edx, release_year = as.numeric(str_match(title,"\\((\\d{4})\\)$")[,2]))$release_year)
# plot time since release effects on ratings
edx %>%
  mutate(since_release = year(as_datetime(timestamp))-as.numeric(str_match(title,"\\((\\d{4})\\)$")[,2])) %>%
  group_by(since_release) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(since_release, rating)) +
  geom_point() +
  geom_smooth()
# there seems to be a correlation between how old a film is when it is rated and the rating given
# perhaps people are seeking out older films they already know to be good?
# what about the negative since release values?
edx %>%
  mutate(date = as_datetime(timestamp), 
         release_year = as.numeric(str_match(title,"\\((\\d{4})\\)$")[,2]), 
         since_release = year(as_datetime(timestamp))-as.numeric(str_match(title,"\\((\\d{4})\\)$")[,2])) %>% 
  filter (since_release < 0)
# these appear genine and there aren't many of them
# adding since release as a factor in our edx, train, and test sets
edx <- edx %>% 
  mutate(since_release = year(as_datetime(timestamp))-
           as.numeric(str_match(title,"\\((\\d{4})\\)$")[,2]))
train <- train %>% 
  mutate(since_release = year(as_datetime(timestamp))-
           as.numeric(str_match(title,"\\((\\d{4})\\)$")[,2]))
test <- test %>% 
  mutate(since_release = year(as_datetime(timestamp))-
           as.numeric(str_match(title,"\\((\\d{4})\\)$")[,2]))
# calculate since release factor
since_release_avgs <- train %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  group_by(since_release) %>%
  summarize(b_sr = mean(rating - mu_hat - b_i - b_u - b_g))
# predictions based on since release, genre, user, and movie effect factors
predicted_ratings_SREM <- test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(since_release_avgs, by='since_release') %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_sr) %>%
  .$pred
# RMSE of model 4
model_4_rmse <- RMSE(predicted_ratings_SREM, test$rating)
# added to results table
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + Genre + Since Release Effects Model",  
                                 RMSE = model_4_rmse ))
# test lambdas for regularization and apply to the model
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train$rating)
  b_i <- train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_g <- train %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+l))
  b_sr <- train %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(since_release) %>%
    summarize(b_sr = sum(rating - b_i - b_u - b_g - mu)/(n()+l))
  predicted_ratings <- 
    test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_sr, by = "since_release") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_sr) %>%
    .$pred
  return(RMSE(predicted_ratings, test$rating))
})

# plot to visualize effect of lambda on RMSE
plot(lambdas,rmses)
# store best lambda and add newest RMSE to table of results
lambda <- lambdas[which.min(rmses)]
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized Movie + User + Genre + Since Release Effect Model",  
                                     RMSE = min(rmses)))
# Check progression of RMSE as models evolve
rmse_results %>% knitr::kable()
# add the regularization factor into the models for movie, user, genre, and since release
movie_avgs <- train %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda))
user_avgs <- train %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu_hat - b_i)/(n()+lambda))
genre_avgs <- train %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu_hat - b_i - b_u)/(n()+lambda))
since_release_avgs <- train %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  group_by(since_release) %>%
  summarize(b_sr = sum(rating - mu_hat - b_i - b_u - b_g)/(n()+lambda))
# predict validation set with all 3 factors now including lambda
y_hat <- validation %>% 
  mutate(since_release = year(as_datetime(timestamp))-as.numeric(str_match(title,"\\((\\d{4})\\)$")[,2])) %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(since_release_avgs, by='since_release') %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_sr) %>%
  .$pred
# calculate RMSE of prediction against validation set
RMSE(validation$rating,y_hat)
# add target final results to table
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Target Final Result",  
                                 RMSE = 0.8649000))
# add final results to table
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Final Model vs Validation RMSE",  
                                     RMSE = RMSE(validation$rating,y_hat)))
# final RMSE results table
rmse_results %>% knitr::kable()

# time taken
timetaken(started.at)