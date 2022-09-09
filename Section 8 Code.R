#Knowledge Check
install.packages("dslabs")
library(dslabs)
data(heights)
class(heights)
class(heights$sex)
class(heights$height)
class("Male")
class(75.00000)
nrow(heights)
heights$height[777]
max(heights$height)
which.min(heights$height)
summary(heights)
mean(heights$height)
median(heights$height)
mean(heights$sex == "Male")
sum(heights$height > 78)
sum(heights$sex == "Female" & heights$height > 78)

#Basics
library(tidyverse)
library(caret)
library(dslabs)
data(heights)
# define the outcome and predictors
y <- heights$sex
x <- heights$height
# generate training and test sets split in 2  to have something to test the algorithm with
set.seed(2, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]
# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))
# compute accuracy
mean(y_hat == test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)
# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)
#how many features in mnist read by read_mnist()
library(dslabs)
mnist <- read_mnist()
ncol(mnist$train$images)
#confusion matrix
# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")
confusionMatrix(data = y_hat, reference = test_set$sex)

#Balanced accuracy and F1 score
# maximize F-score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)
#Prevalence matters in practice
#A machine learning algorithm with very high sensitivity and specificity may not be useful in practice 
#when prevalence is close to either 0 or 1. For example, if you develop an algorithm for disease diagnosis 
#with very high sensitivity, but the prevalence of the disease is pretty low, then the precision of your 
#algorithm is probably very low based on Bayes' theorem.
#ROC and precision-recall curves
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)
# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

#Practice Machine Learning Part 1
library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type
#% of female in class and online
sum(dat$type=="inclass" & dat$sex == "Female")/sum(dat$type=="inclass")
sum(dat$type=="online" & dat$sex == "Female")/sum(dat$type=="online")
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))
#predict sex with type
y_hat <- ifelse(x == "inclass", "Female", "Male") %>% factor(levels = levels(y))
mean(y_hat==y)
#use table() function and generate a confusion matrix between y hat and y
table(y_hat, y)
#what is the sensitivity
sensitivity(y_hat, y)
#what is the specificity
specificity(y_hat, y)
#what is the prevalence of females
mean(y == "Female")
#full confusion matrix stats
confusionMatrix(data = y_hat, reference = y)

#build machine learning algorithm using new dataset
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
# find best predictive feature
foo <- function(x){
  #range(x) gives min and max of supplied values, [1] gives first value (min) [2] gives second value max
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
  #for each value in the range of values compare to x and set 'virginica' if higher 'versicolor' if lower
    y_hat <- ifelse(x>i,'virginica','versicolor')
  #compare y_hat to training data for fit  
    mean(y_hat==train$Species)
  })
}
#train[,-5] is train minus the 5th colummn, 2 applies to columns, foo is the function to apply
predictions <- apply(train[,-5],2,foo) 
sapply(predictions,max)	
# accuracy using Petal.Length and smart cutoff
# replace predictions with only petal.length values (column 3)
predictions <- foo(train[,3])
# find range of values for only petal.length
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
# find the cutoff values with the maximum prediction
cutoffs <-rangedValues[which(predictions==max(predictions))]
# find accuracy using the cutoffs by comparing y_hat to y (y=test$species)
y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)
#find best predictive feature using test data
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)
#exploratory analysis
plot(iris,pch=21,bg=iris$Species)
# plots show 2 predictors plotted together color coded by species
# best combination for an OR condition is one color mostly confined to bottom left quadrant
# accuracy when optimal legth or width cutoff is used
petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)
# find best length cutoffs
length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7
# find best length cutoffs
width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5
# use both cutoffs with an or condition and compare to test set
y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)


#Conditional Probabilities
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
#Chance test is positive
mean(test)
#chance of false negative
mean(disease[test==0])
#chance of correct positive
mean(disease[test==1]==1)
#given positive test how many times more likely is the risk of having the disease
mean(disease[test==1]==1)/mean(disease==1)
#heights data set plot conditional probability of being male
library(dslabs)
data("heights")
# plot estimated conditional probability of male in data set with highets to the nearest inch 
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
qplot(height, p, data =.)
#plot by quantiles
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)
# generateLo a bivariate normal distribution from MASS package
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
# plot conditional expectations
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)

#Linear Regression for Prediction, Smoothing, and working with matrices
library(tidyverse)
library(HistData)
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)
avg <- mean(train_set$son)
avg
mean((avg - test_set$son)^2)
# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)
#predict funciton
#The predict() function takes a fitted object from functions such as lm() or glm() and a data frame 
#with the new predictors for which to predict. We can use predict like this:
y_hat <- predict(fit, test_set)
#predict() is a generic function in R that calls other functions depending on what kind of object it receives
#To learn about the specifics, you can read the help files using code like this: 
?predict.lm    # or ?predict.glm
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)
# read help files
?predict.lm
?predict.glm
# Linear regression
library(tidyverse)
library(caret)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
# replicate 100 times to train a linear model predicting y from x
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})
mean(rmse)
sd(rmse)
# write a function to repeate this process for several values of n
# set.seed(1) # if R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), sd = sd(rmse))
})
res

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
#find mean and sd of RMSE over 100 replicates
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})
mean(rmse)
sd(rmse)
#create another data set with 2 predictors
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
#calculate RMSE with each predictor then with both at once
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
#predict with x_1
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#predict with x_2
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#predict with both
fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
# repeat with highly correlated x_1 and x_2
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
#predict with x_1
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#predict with x_2
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
#predict with both
fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

# Regression fo categorical outcomes
library(dslabs)
data("heights")
y <- heights$height
# set seeds for illustrative purposes
set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind = "Rounding") #if you are using R 3.6 or later
# create the test index, training set, and testing set
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)
# find probability that a 66" person is Female
train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))
# repeat with rounded value for every height and plot the results
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()
# Convert sex factor to 0s and 1s, estimate using least squares regression
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
# find p-hat for this linear model
p_hat <- predict(lm_fit, test_set)
# create a decision rule guessing female if the probilibty of female is > 50%
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
# check the results for accuracy 
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]
# plot the linear regresion against the data
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])
# observe the range of estimates keep in mind P is between 0 and 1
range(p_hat)
# use logistic regression to fit the 0-1 nature of the probability
# fit logistic regression using glm_fit fuction family set to binomial
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")
# when using predict type = response to get probabilities.  
# default is to return logistic transfom values
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
# with our new estimate of the conditional probability find new predictions
# and plot them against the original data
tmp <- heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) 
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)
# check for accuracy with the new model
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]

# case study logistic regression to classify 2s and 7s in the zip code reader
# illustration of 2 examples with largest and smallest values of x_1 (upper left darkness)
mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)
# explore the data for teh two predictors, upper left and lower right panel for 2 and 7
data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()
# example of 2 examples with the largest and smallest lower right quadrant darkness X_2
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)
# model conditional probability using log transform
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]
# plot the true conditional probability
mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()
# choose better colors and draw a line between the x1 and x2 pairs
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 
# compare to logistical regression fit line
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 
# plot test data again aith x1 and x2 ploted against each other alone with the logistic regression
# points that fall on the wrong side of the line will be counted wrong
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)

# define a data set
library(tidyverse)
library(caret)
# set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
dat$train %>% ggplot(aes(x, color = y)) + geom_density()
#set seed to 1 and mu_1 to 25 points between 0 and 3 and plot the results of 25 sets
#set.seed(1) if you are using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)

#Smoothing
data("polls_2008")
qplot(day, margin, data = polls_2008)
# bin smoothers
span <- 7 
fit <- with(polls_2008,ksmooth(day, margin, x.points = day, kernel="box", bandwidth =span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")
# kernel
span <- 7
fit <- with(polls_2008, ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")
# loess line
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")
# loess line degree = 1 means local estimate is a straight line
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))
# loess line vs parabola degree 1 vs 2 (default)
total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

fit_2 <- loess(margin ~ day, span = span, data=polls_2008)


polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1) 
# Smoothing exercises
# setup
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")
# plot using loess degree = 1 span = 60 days
span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = "red")
# plot smoothness against days of the year all on the same plot color coded
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)
# 2s vs 7s
library(broom)
library(dslabs)
data("mnist_27")
# test predictive power with log regression and scatter plot
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)
# No useful results
# fit loess line to the data intead
mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")

# Matrices
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
class(mnist$train$images)
x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]
# matrix notation
length(x[,1])
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)
# dimensions
dim(x)
dim(x_1)
dim(as.matrix(x_1))
dim(x)
# converting vectors to matricies
my_vector <- 1:15
# fill the matrix by column
mat <- matrix(my_vector, 5, 3)
mat
# fill by row
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t
identical(t(mat), mat_t)
matrix(my_vector, 5, 5)
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)
# flip the image back
image(1:28, 1:28, grid[, 28:1])
# sum of each row
sums <- rowSums(x)
# average of each row
avg <- rowMeans(x)
# plot of row averages
data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")
# apply can apply any function to a matrix
# x is the matrix, 1 for rows 2 for columns, then funciton
avgs <- apply(x, 1, mean)
sds <- apply(x, 2, sd)
# filtering
library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])
#extract columns and rows
x[ ,c(351,352)]
x[c(2,3),]
new_x <- x[ ,colSds(x) > 60]
dim(new_x)
class(x[,1])
dim(x[1,])
#preserve the matrix class
class(x[ , 1, drop=FALSE])
dim(x[, 1, drop=FALSE])
# We can use logical operations with matrices:
  mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
# We can also binarize the data using just matrix operations:
  bin_x <- x
bin_x[bin_x < 255/2] <- 0 
bin_x[bin_x > 255/2] <- 1
Code
#index with matrices
mat <- matrix(1:15, 5, 3)
as.vector(mat)
qplot(as.vector(x), bins = 30, color = I("black"))
new_x <- x
new_x[new_x < 50] <- 0
mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat
mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat
#binarize the data
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
bin_X <- (x > 255/2)*1
# We can scale each row of a matrix using this line of code:
  (x - rowMeans(x)) / rowSds(x)
# To scale each column of a matrix, we use this code:
  t(t(x) - colMeans(x))
# We can also use a function called sweep() that works similarly to apply(). 
# It takes each entry of a vector and subtracts it from the corresponding row or column:
  X_mean_0 <- sweep(x, 2, colMeans(x))
#Matrix multiplication: 
  t(x) %*% x
#The cross product: 
  crossprod(x)
#The inverse of a function: 
  solve(crossprod(x))
#The QR decomposition: 
  qr(x)
#scale each row of a matrix
(x - rowMeans(x)) / rowSds(x)
#scale each column
t(t(x) - colMeans(x))
#take each entry of a vector and subtracts it from the corresponding row or column
x_mean_0 <- sweep(x, 2, colMeans(x))
#divide by the standard deviation
x_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")
# create a 100 by 10 matrix of randomly generated normal numbers and save as x
x <- matrix(rnorm(100*10), 100, 10)
# get the dimension
dim(x)
# number of rows
nrow(x) 
dim(x)[1]  
length(x[,1])
# number of columns
ncol(x) 
dim(x)[2] 
length(x[1,])
# add the scalar 1 to row 1 2 to row 2 etc
x <- x + seq(nrow(x))
x <- sweep(x, 1, 1:nrow(x),"+")
# mean of each row
rowMeans(x)
# mean of each column
colMeans(x)
#For each observation in the mnist training data, compute the proportion of pixels 
#that are in the grey area, defined as values between 50 and 205 (but not including 50 and 205). 
q6 <- mnist$train$images
q6[q6 >= 205] <- 0
q6[q6 <= 50] <- 0
q6[q6 > 0] <- 1
mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y) # proportion of pixels

#distance
# define distance between observations, using features or predictors.
# use the function dist(), 
# computes the distance between each row and produces an object of class dist():
  d <- dist(x)
# compute distances between predictors. 
# transpose the matrix first and then use dist():
  d <- dist(t(x))
# Code
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
set.seed(0) # if using R 3.5 or earlier
set.seed(0, sample.kind = "Rounding") # if using R 3.6 or later
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]
y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]
#distance between two numbers
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))
#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))
#compute distance between each row
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]
#visualize these distances
image(as.matrix(d))
#order the distance by labels
image(as.matrix(d)[order(y), order(y)])
#compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))
# distance exercises
library(dslabs)
data(tissue_gene_expression)
# observe matrix x
dim(tissue_gene_expression$x)
# observe tissue expression
table(tissue_gene_expression$y)
# compute distance between each observation
d <- dist(tissue_gene_expression$x)
# distance between 1 and 2, 39 and 40, 73 and 74
# like samples closer or not?
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]
# K-Nearest Neighbors (kNN)
# use distance to find nearest points and take an average similar to bin but multi-dimensional
# large k = smother small k = wiggly just like n for binning
# knn3() in install.packages("caret")
# load predictors
x <- as.matrix(mnist_27$train[,2:3])
# load outcomes
y <- mnist_27$train$y
# call function with predictors,outcomes
knn_fit <- knn3(x,y)
#Code
library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()
#logistic regression to compare against as baseline
library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]
#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)
# . uses all the predictors in the data variable specified default k = 5 specified
knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)
# predict the fitted object type = "class" to give actual predicted outcomes rather than probabilities
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
# use the prediction to get the accuracy of the predictions
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]
# overtraining and oversmoothing
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}
p1 <- plot_cond_prob() + ggtitle("True conditional probability")
p2 <- plot_cond_prob(predict(knn_fit, mnist_27$true_p)[,2]) +
  ggtitle("kNN-5 estimate")
library(gridExtra)
grid.arrange(p1, p2, nrow=1)
# compare accuracy on train and test set to observe overtraining
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class") 
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]
#fit knn with k=1 to show effect of extreme overtrining
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]
# compare to test sett to see accuracy is much lower now
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall[["Accuracy"]]
#plot with k=1 notice the extreme islands perfectly formed around the red dots
p1 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$train, aes(x_1, x_2, color= y),
             pch=21, show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Train set")
#same plot against the test set and the red dots are gone leaving a bad model
p2 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$test, aes(x_1, x_2, color= y), 
             pch=21, show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Test set")
grid.arrange(p1, p2, nrow=1)
#fit knn with k=401 k is too big now this is oversmoothing
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]
#plot p1 for the log regression p2 for over smoothed
p1 <- plot_cond_prob(predict(fit_glm, mnist_27$true_p)) +
  ggtitle("Logistic regression")
p2 <- plot_cond_prob(predict(knn_fit_401, mnist_27$true_p)[,2]) +
  ggtitle("kNN-401")
grid.arrange(p1, p2, nrow=1)
#pick the k in knn by trying all the odd numbers between 3 and 251
ks <- seq(3, 251, 2)
library(purrr)
# use map_df to compare test accuracy and train accuracy for each k in ks
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
# create y_hat for train set and compute accuracy with the cm  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
# create y_hat for test set and compute accuracy with cm  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
# output a tibble of the two errors  
  tibble(train = train_error, test = test_error)
})
# plot ks vs accuracy for both test and train sets
accuracy %>% mutate(k = ks) %>%
  gather(set, accuracy, -k) %>%
  mutate(set = factor(set, levels = c("train", "test"))) %>%
  ggplot(aes(k, accuracy, color = set)) + 
  geom_line() +
  geom_point() 
# plot true and new knn 41 estimate, very good fit
p1 <- plot_cond_prob() + ggtitle("True conditional probability")
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 41)
p2 <- plot_cond_prob(predict(knn_fit, newdata = mnist_27$true_p)[,2]) +
  ggtitle("kNN-41 estimate")
grid.arrange(p2, p1, nrow=1)
# accuracy of new model
max(accuracy$test)


#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)
# this is only iullustrative and a true ML solution should not sure the test set to find k

# heights with kNN
set.seed(1, sample.kind = "Rounding") 
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)
# perform knn with k values of seq(1, 101, 3) and measure f1 scores
library(dslabs)
library(tidyverse)
library(caret)
data("heights")
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     
ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]
# tissue gene expression with kNN
# using k seq(1,11,2) find the accuracy at each k for kNN
library(dslabs)
library(caret)
data("tissue_gene_expression")
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})

# k-fold cross-validation
# theorize true error as mean of several observed errors
# split into k non-overlapping sets to test with
# in practice split train and test 80-20 or 90-10
# then split train set into k non-overlapping sets
library(tidyverse)
library(caret)
# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]
# perform cross validation with all xs
fit <- train(x_subset, y, method = "glm")
fit$results
# search for more predictive predictors
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
# create vector of p values
pvals <- tt$p.value
# index of statistically significant paramaters p-value cutoff of 0.01
ind <- which(pvals <= 0.01)
length(ind)
# rerun cross validation with limited Xs
fit <- train(x[,ind], y, method = "glm")
fit$results
# rerun cross validation with kNN using k = seq(101, 301, 25)
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)
# use train on tissue sample data to find best k without splitting sets
data("tissue_gene_expression")
fit <- with(tissue_gene_expression, train(x, y, method = "knn", tuneGrid = data.frame( k = seq(1, 7, 2))))
ggplot(fit)
fit$results

# bootstrapping
# create fake random dataset of income data
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
# plot the data
qplot(log10(income), bins = 30, color = I("black"))
# m is population median
m <- median(income)
m
# sample data and estimate the median with sample median M
set.seed(1)
#use set.seed(1, sample.kind="Rounding") instead if using R 3.6 or later
N <- 250
X <- sample(income, N)
M<- median(X)
M
# construct monte carlo simulation of sample to observe the shape
library(gridExtra)
B <- 10^5
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)
# sample mean and standard deviation
mean(M)
sd(M)
# construct bootstrap sample with replacement samples from the sample
B <- 10^5
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})
# check how close it is to the actual distribution
p3 <- tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()
grid.arrange(p2,p3,ncol=2)
# quantiles for 95% confidance interval fairly close between M and M_star
quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))
# direct calculation of median confidance interval
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)
# estimate mean
mean(M) + 1.96 * sd(M) * c(-1,1)
# estimate standard error
mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)
# The createResample() function can be used to create bootstrap samples. 
# For example, we can create the indexes for 10 bootstrap samples for the mnist_27 dataset like this:
library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)
# observe sampling is with replacement by checking for number of 3, 4, and 7
sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)
# how many times does 3 appear in all sampled indexes
x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)
# generate a random data set
y <- rnorm(100, 0, 1)
# estimate 75th quantile with sample quantile
quantile(y, 0.75)
# set seed to 1 and monte carlo simulation b= 10,000 estimate 75th quantile each time
# set.seed(1995) # if R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
B <- 10000
q_75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(q_75)
sd(q_75)
# repeat with bootstrapping instead of monte carlo set seed to 1 again
# set.seed(1) # if R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)
# sample size of 100 from the sample with replacement
# set.seed(1) # if R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
# 10 bootstraps
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)
# now increase to 10000 bootstraps
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
# 10000 bootstraps
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

# Generative Models
# Discriminative approaches estimate the conditional probability directly and 
# do not consider the distribution of the predictors. 
# Generative models are methods that model the joint distribution and  
# X (we model how the entire data, X and, Y are generated).
# Generating train and test set
library("caret")
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)
# Estimating averages and standard deviations
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params
# Estimating the prevalence
pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi
# Getting an actual rule
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))
# Computing sensitivity
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))
# Computing specificity
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))
# Changing the cutoff of the decision rule
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
# Draw plot
qplot(x, p_hat_bayes_unbiased, geom = "line") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_vline(xintercept = 67, lty = 2)
#QDA
# Load data
data("mnist_27")
# Estimate parameters from the data
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
# Contour plots
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm", lwd = 1.5)
# Fit model
library(caret)
train_qda <- train(y ~., method = "qda", data = mnist_27$train)
# Obtain predictors and accuracy
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
# Draw separate plots for 2s and 7s
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm") +
  facet_wrap(~y)
#LDA
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
params <- params %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))
train_lda <- train(y ~., method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
# 3 classes example
if(!exists("mnist"))mnist <- read_mnist()

#set.seed(3456)    #use 
set.seed(3456, sample.kind="Rounding") #in R 3.6 or later
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)

# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

# binarize the values. Above 200 is ink, below is no ink
x <- x > 200 

# cbind proportion of pixels in upper right quadrant and proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x),
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])

test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])

train_set %>%  ggplot(aes(x_1, x_2, color=y)) + geom_point()

train_qda <- train(y ~ ., method = "qda", data = train_set)
predict(train_qda, test_set, type = "prob") %>% head()
predict(train_qda, test_set) %>% head()
confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]
train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]
train_set %>% mutate(y = factor(y)) %>% ggplot(aes(x_1, x_2, fill = y, color=y)) + geom_point(show.legend = FALSE) + stat_ellipse(type="norm")
# Generative models knowledge check
# setup
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")
# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
# use train() to estimate accuracy of LDA
fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]
# view finalModel means
fit_lda$finalModel["means"]
# chart finalmodel means
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()
# repeat with QDA
library(dslabs)      
library(caret)
data("tissue_gene_expression")
#set.seed(1993) 
set.seed(1993, sample.kind="Rounding") #if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
# train using qda and get accuracy
fit_qda <- train(x, y, method = "qda")
fit_qda$results["Accuracy"]
# view final model means
fit_qda$finalModel["means"]
# graph final model means
t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()
# rerun LDA with preProcess = "center"
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name, hippocampus)) +
  geom_point() +
  coord_flip()
# plot predictor values with color coding as a scatter plot to check for bivariate distribution
d <- apply(fit_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)
# repeat LDA but using all tissue types
library(dslabs)      
library(caret)
data("tissue_gene_expression")
# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
# run LDA trainig and get accuracy
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]

# Classification with more than 2 classes and the caret package
# Regression and decision trees and random forests
# Classification and Regression Trees (CART)
# Load data
library(tidyverse)
library(dslabs)
data("olive")
olive %>% as_tibble()
table(olive$region)
olive <- select(olive, -area)
# Predict region using KNN
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)
# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())
# plot values for eicosenoic and linoleic
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)
# load data for regression tree
data("polls_2008")
qplot(day, margin, data = polls_2008)

library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)
# visualize the splits 
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")
# change parameters
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")
# use cross validation to choose cp
library(caret)
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
ggplot(train_rpart)
# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")
# prune the tree 
pruned_fit <- prune(fit, cp = 0.01)

#classification trees (for categorical data)
# fit a classification tree and plot it
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)
# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
# random forests for 2008 polls
library(randomForest)
fit <- randomForest(margin~., data = polls_2008) 
# plot fit to see how fit changes with number of trees
plot(fit)
# plot of 2008 polls with red line for results of the model
polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")
# random forest for 2 or 7 example
library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
# use cross validation to choose parameter and use Rborist a faster random tree algorithm
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
# trees and random forests
# create dataset where outcome grows 0.75 units on every for every increse in the predictor
library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
# use an rpart regression tree to fit the data
fit <- rpart(y ~ ., data = dat)
# plot the tree with lables
plot(fit)
text(fit)
# make a scatter plot of y vs x along with predicted values
p_rpart <- dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)
# use random forests instead
library(randomForest)
p_randomforest <- fit <- randomForest(y ~ x, data = dat) 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
# check fit vs number of trees
plot(fit)
# change the node size to 50 with a max of 25 nodes
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
# caret package
  library(tidyverse)
  library(dslabs)
  data("mnist_27")
  
  library(caret)
  train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
  train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
  
  y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
  y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")
  
  confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
  confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]
# train can auto optimize, models can be looked up
  getModelInfo("knn")
  modelLookup("knn")
# run train with defaults which automatically cross-validates parameters
# default testing is performed on 25 bootstrap samples
# default range of k for knn model is 5,7,and 9
  train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
  ggplot(train_knn, highlight = TRUE)
# change range of parameters being checked by the train
# tune grid must be supplied by a data frame column names must match modellookup output
# knn itrained, modelLookup("knn") returns 1 param k data frame must have 1 column k
  train_knn <- train(y ~ ., method = "knn", 
                     data = mnist_27$train,
                     tuneGrid = data.frame(k = seq(9, 71, 2)))
  ggplot(train_knn, highlight = TRUE)
# best parameter for accuracy
  train_knn$bestTune
# best model with results
  train_knn$finalModel
# accuracy on test set, how well is the trained model predicting using the test data
  confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                  mnist_27$test$y)$overall["Accuracy"]
# tweaking cross-validation method with traincontrol
# method is cross-validation "cv" 10 fold number = 10
  control <- trainControl(method = "cv", number = 10, p = .9)
  train_knn_cv <- train(y ~ ., method = "knn", 
                        data = mnist_27$train,
                        tuneGrid = data.frame(k = seq(9, 71, 2)),
                        trControl = control)
  ggplot(train_knn_cv, highlight = TRUE)
# chart accuracy  with sdev of the tested values of k from the training above 
  train_knn$results %>% 
    ggplot(aes(x = k, y = Accuracy)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(x = k, 
                      ymin = Accuracy - AccuracySD,
                      ymax = Accuracy + AccuracySD))
# create a function taking p-hat and plotting conditional probability 
  plot_cond_prob <- function(p_hat=NULL){
    tmp <- mnist_27$true_p
    if(!is.null(p_hat)){
      tmp <- mutate(tmp, p=p_hat)
    }
    tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
      geom_raster(show.legend = FALSE) +
      scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
      stat_contour(breaks=c(0.5),color="black")
  }
# plot best fitting knn model with plot_cond_prob 
  plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])
# switch to gamloess, check for the required parameters  
  install.packages("gam")
  modelLookup("gamLoess")
# fix the degree at 1 and try different values for span (must define both columns)
  grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)
# train the model with gamloess using the tuned grid above  and plot
  train_loess <- train(y ~ ., 
                       method = "gamLoess",
                       tuneGrid=grid,
                       data = mnist_27$train)
  ggplot(train_loess, highlight = TRUE)
# check accuracy of this model
  confusionMatrix(data = predict(train_loess, mnist_27$test), 
                  reference = mnist_27$test$y)$overall["Accuracy"]
# use plot_cond_prob to plot the conditional probability  
  p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
  p1
# use rpart ot train a model for tissue gene expression with cp = seq(0, 0.1, 0.01)
  library(caret)
  library(rpart)          
  library(dslabs)
  # set.seed(1991) # if using R 3.5 or earlier
  set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
  data("tissue_gene_expression")
# fit rpart model using cp = seq(0, 0.1, 0.01)
  fit <- with(tissue_gene_expression, 
              train(x, y, method = "rpart",
                    tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
# plot fit to check for best result  
  ggplot(fit)
# default minsplit is 20, lower the minsplit cutoff to allow for smaller nodes
# set.seed(1991) # if using R 3.5 or earlier
  set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
  fit_rpart <- with(tissue_gene_expression, 
                    train(x, y, method = "rpart",
                          tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                          control = rpart.control(minsplit = 0)))
  ggplot(fit_rpart)
  confusionMatrix(fit_rpart)
# plot tree from the model
  plot(fit_rpart$finalModel)
  text(fit_rpart$finalModel)
# try for fewer trees with random forests
# model = rf, mtry seq(50, 200, 25), nodesize = 1, save to fit
library(randomForest)
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
fit <- with(tissue_gene_expression,
            train(x, y, method = "rf", 
                        tuneGrid = data.frame(mtry = seq(50, 200, 25)),
                        nodesize = 1)
)
# save varImp() of fit to imp
imp <- varImp(fit)
imp
# extract the predictor names and save to tree_terms
tree_terms <- as.character(unique(fit$finalModel$frame$var[!(fit$finalModel$frame$var == "<leaf>")]))
tree_terms
# find variable importance and rank of CFHR4 gene in teh random forest call

#Titanic Exercises
# Libraries and data
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)
# 3 significant digits
options(digits = 3)
# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
# set seed to 42
set.seed(42, sample.kind = "Rounding") # if using R 3.6 or later
# split titanic_clean into 20% test_set and 80% train_set
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]
# survival rate in the training set
mean(train_set$Survived == 1)
# set seed 3
set.seed(3, sample.kind = "Rounding") # if using R 3.6 or later
# random guess with equal probability of survival
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
# accuracy
mean(guess == test_set$Survived)
# examine sex impact on survival
# female survival rate
train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "female") %>%
  pull(Survived)
# male survival rate
train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "male") %>%
  pull(Survived)
# predict based on sex alone
sex_model <- ifelse(test_set$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_model == test_set$Survived)    # calculate accuracy
# survival by class
train_set %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived == 1))
# predict based on class alone cutoff of 0.5
class_model <- ifelse(test_set$Pclass == 1, 1, 0)    # predict survival only if first class
mean(class_model == test_set$Survived)    # calculate accuracy
# group by sex and class and calculate survival rate
train_set %>%
  group_by(Sex,Pclass) %>%
  summarize(Survived = mean(Survived == 1))
# predict based on sex and class
sex_class_model <- ifelse(test_set$Pclass != 3 & test_set$Sex == "female",1,0)
mean(sex_class_model == test_set$Survived)
# confusion matrix for each model
confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(class_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(sex_class_model), reference = factor(test_set$Survived))
# calculate F1 scores for each model and compare
F_meas(data = factor(sex_model), reference = test_set$Survived)
F_meas(data = factor(class_model), reference = test_set$Survived)
F_meas(data = factor(sex_class_model), reference = test_set$Survived)
# set seed to 1
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
# train LDA model using fare
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
lda_preds <- predict(train_lda, test_set)
mean(lda_preds == test_set$Survived)
# set seed to 1
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
# train QDA model using fare
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
qda_preds <- predict(train_qda, test_set)
mean(qda_preds == test_set$Survived)
# set seed to 1
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
# train glm model using age
train_glm_age <- train(Survived ~ Age, method = "glm", data = train_set)
glm_preds_age <- predict(train_glm_age, test_set)
mean(glm_preds_age == test_set$Survived)
# set seed to 1
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
# train glm model using sex, class, fare, and age
train_glm <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$Survived)
# set seed to 1
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
# train glm model using all predictors
train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
glm_all_preds <- predict(train_glm_all, test_set)
mean(glm_all_preds == test_set$Survived)
# set seed to 6
set.seed(6, sample.kind = "Rounding") # if using R 3.6 or later
# train knn model using all predictors
train_knn <- train(Survived ~ ., method = "knn", 
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
# plot k's
ggplot(train_knn, highlight = TRUE)
# best k
train_knn$bestTune
# accuracy of kNN model on test set
knn_all_preds <- predict(train_knn,test_set)
mean(knn_all_preds == test_set$Survived)
# set seed to 8
set.seed(8, sample.kind = "Rounding") # if using R 3.6 or later
# train kNN with 10 fold cv
train_knn_cv <- train(Survived ~ .,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
# optimal k
train_knn_cv$bestTune
# accuracy against test set
knn_cv_preds <- predict(train_knn_cv, test_set)
mean(knn_cv_preds == test_set$Survived)
# set seed 10
set.seed(10, sample.kind = "Rounding") # if using R 3.6 or later
# train rpart method
train_rpart <- train(Survived ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
# best cp
train_rpart$bestTune
# accuracy against test set
rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$Survived)
train_rpart$finalModel # inspect final model
# make plot of decision tree
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel)
# set seed 14
set.seed(14, sample.kind = "Rounding") # if using R 3.6 or later
# train rf method
train_rf <- train(Survived ~ .,
                  data = train_set,
                  method = "rf",
                  ntree = 100,
                  tuneGrid = data.frame(mtry = seq(1:7)))
# best mtry
train_rf$bestTune
# test accuracy against test set
rf_preds <- predict(train_rf, test_set)
mean(rf_preds == test_set$Survived)
# find most important variable in the model
varImp(train_rf)    # first row

# Model fitting and recommendation systems
# MNIST case study
library(dslabs)
mnist <- read_mnist()
names(mnist)
dim(mnist$train$images)
class(mnist$train$labels)
table(mnist$train$labels)
# sample 10k rows from training set, 1k rows from test set
set.seed(123)
# 10000 random rows for the train set
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
# 1000 random rows for the test set
index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])
# pre-processing to transform predictors or remove ones that are not useful
# examine sd of each feature and plot
library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))
# use near zero variance to plot features with little predictive power
library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))
# create index with the features with near zero variance removed using NZV
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)
# must add column names for caret package to work properly
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)
# optimize for number of neighbors (longer run time)
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)
ggplot(train_knn)
# test code on smaller data sets to save time when debugging
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
# optimized model
fit_knn <- knn3(x[ ,col_index], y,  k = 3)
# check accuracy of knn model
y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]
# check which digits are hardest to detect accurately and most common incorrect prediction
cm$byClass[,1:2]
# try a random forest model
# RF is slow so we need to cut down when looking for optimization
# rborist package is faster
# fitting for rf slow so we cut CV down to 5 fold
# subset for exploration of a smaller number of trees nSamp = 5000
library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
# plot and look at the best tuned parameters
ggplot(train_rf)
train_rf$bestTune
# fit with full set using the tuned parameters for minnode and predfixed
fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
# check accuracy of the new model
cm$overall["Accuracy"]
# check by digit again big improvement for 8s
cm$byClass[,1:2]
# some examples of the original images and the predicted value
rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}
# need to switch to randomforest package to examine variable importance
library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50)
imp <- importance(rf)
imp
# plot to visualize where in the image the important features are located in the image
image(matrix(imp, 28, 28))
# look at cases where the knn digit call was wrong to visualize mistakes
p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}
# look at cases where the rf digit call was wrong to visualize mistakes
p_max <- predict(fit_rf, x_test[,col_index])$census  
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}
# ensemble
# combine more than one model to improve accuracy
# compute new class probabilities by averaging two models together
p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)
confusionMatrix(y_pred, y_test)$byClass[,1:2]
# build a model of 10 most common ML models
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
# apply all models using train() with default parameters
library(caret)
library(dslabs)
library(tidyverse)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")
# fit each model in models to the data
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 
# add models as column names
names(fits) <- models
# create a matrix of predictions
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)
# accuracy for each model
acc <- colMeans(pred == mnist_27$test$y)
acc
# average accuracy of all models together
mean(acc)
# predict with ensemble by majority vote
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)
# check for models that do better than average
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]
# check accuracy against cv with training data for each model
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)
# filter out models below 80% accuracy and ensemble
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)
# recommendation systems
# Netflix Challenge each outcome has a different set of predictors
# movielens case study
library(tidyverse)
library(dslabs)
data("movielens")
# tidy format
movielens %>% as_tibble()
# each row a rating by a user
movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)
tab <- movielens %>%
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)
tab %>% knitr::kable()

users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

movielens %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

movielens %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")
# set seed to 755 and create a partition to create test and train sets
library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]
# filter out users and movies from the test set that are not in the train set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
# loss function for residual mean squared error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
# first model, simplest possible recommendation to predict same ratings for everyone
mu_hat <- mean(train_set$rating)
mu_hat
# prediction all unknown ratings with mu_hat and check RMSE for this prediciton
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse
# picking an arbitrary rating gives us higher RMSE 
predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)
# create a results table to track results across different approaches
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
# we can run lm but it will be very slow (not recommended)
# fit <- lm(rating ~ as.factor(userId), data = movielens)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
# visualize estimates
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))
# build a linear model manually from mean and b_i and predict outcomes
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
# store output of model to our results table
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
# check updated results table
rmse_results %>% knitr::kable()
# User effects -> compute averages by users when a user has 100 or more movies
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")
# again we could do a straight linear model but this would be very slow
# lm(rating ~ as.factor(movieId) + as.factor(userId))
# approximate lm by manually computing mu and b_i
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
# construct the predictors
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
# store to results table and check results
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()
# comprehension check: recommendation systems
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")
# find what year has the highest median number of ratings
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# select top 25 movies by average ratings per year (count/2018-year)
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))
# stratify post 1993 movies by ratings per year plot avg by year for trend
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()
# create new date field in movielens using the timestamp column
movielens <- mutate(movielens, date = as_datetime(timestamp))
# plot ratings trend over time using new date field
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()
# group by genres and get average and sd for each grenre of ratings and plot
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#regularization
library(dslabs)
library(tidyverse)
library(caret)
data("movielens")
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
# 10 largest mistakes from the existing model
test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% knitr::kable()
# top 10 best based on b_i
movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()
# top 10 worst based on b_i
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()
# include number of ratings in our training set
train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()
# include number of ratings in our training set
train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()
# add a pentalty term to our model to penalize low sample sizes, low n_i
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
# using lamda of 3 plot regularized estimates vs least squares estimates
data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)
# top 10 best with regularization
train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()
# top 10 worst with regularization
train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()
# does it improve mean squared error though?
# generated predicted ratings
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred
# run model and get rmse
model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()
# use cross validation to pick lamda
lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]
# include the user effect this time
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
# plot lamdas and rmses
qplot(lambdas, rmses)  
# cross validated lamda pick and value
lambda <- lambdas[which.min(rmses)]
lambda
# RMSE with regularized and user effect combined
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()
# matrix factorization
train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% #3252 is Scent of a Woman used in example
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

rownames(y)<- y[,1]
y <- y[,-1]
colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))

m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") %>% 
  knitr::kable()

set.seed(1)
options(digits = 2)
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)

X <- jitter(P%*%t(Q))
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(aling="c")

P

set.seed(1)
options(digits = 2)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(align="c")

P

six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")

y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

dim(pca$rotation)

dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)



# Regularization: Education
# reset digits to 7 and install libraries
library(dplyr)
options(digits=7)
# set seed and generate 1000 schools with a size of n
# set.seed(1986) # if using R 3.5 or earlier
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))
# assign a true quality for each school as mu
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
# top 10 schools by quality
schools %>% top_n(10, quality) %>% arrange(desc(quality))
# simulate a test taken at the school. test results have random variability but are correlated with quality
# simulate normally distributed scores with average determined by quality and a sdev of 30%
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))
# top 10 schools by score
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)
# median school size
median(schools$size)
# median top 10 schools by score
schools %>% top_n(10, score) %>% summarize(median(size))
# median bottom 10 schools by score
schools %>% top_n(-10, score) %>% summarize(median(size))
# plot scores by size and highlight the top 10 schools by quality
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)
# regularize using the average scores for all schools
overall <- mean(sapply(scores, mean))
# estimate difference from average by dividing by n + alpha  using alpha = 25
schools <- schools %>% mutate(regularized = (score*n+overall*25)/(n+25))
alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))
# Using values of  from 10 to 250, find the alpha that minimizes the RMSE
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]
# use the ideal alpha to get id of top school and regularized average of the 10th school
alpha <- 135
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))
# try without subtracting overall meant to demonstrate an easy mistake to make
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]

# Decomposition
set.seed(1987)
#if using R 3.6 or later, use `set.seed(1987, sample.kind="Rounding")` instead
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))
# visualize the 24 test scores
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)
# The students that test well are at the top of the image and there seem to be three groupings by subject.
# Examine the correlation
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# There is correlation among all tests, but higher if the tests are in science and math and even higher within each subject.
# compute the SVD of y
s <- svd(y)
names(s)
# Check the result
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))
# calculate sum of squares of the columns
ss_y <- apply(y^2, 2, sum)
sum(ss_y)
# calculte sum of squares of y_svd
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_yv)
# they are equal
# plot to see differences
plot(ss_y)
plot(ss_yv)
# plot ss_yv against D
data.frame(x = sqrt(ss_yv), y = s$d) %>%
  ggplot(aes(x,y)) +
  geom_point()
# % how much variability in first 3 columns
sum(s$d[1:3]^2) / sum(s$d^2)
# comupute UD without constructing diag(s$d)
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))
# plot U 1 D1,1 and the average score for each student 
plot(s$u[,1]*s$d[1], rowMeans(y))
# make an image of v
my_image(s$v)
# make an image of Y1D1,1V1
plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)
# looks a lot like the original matrix with just a vector of 100, a scalar, and a vector of 24
# explore high similarity within subjects by computing the difference between approximation and original data
resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# make an image of Y2D2,2V2 whithin subject variance
plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)
# image of U3d3,3V3 differences between math and science
plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)
# new residuals
resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# structure is gone
# model has 3 useful interpretations 1 = ability 2 = math/science vs art 3 = math vs science
# plot y, the 3 components of the model within the same zlim
y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))
my_image(y_hat, zlim = range(y))
my_image(y - y_hat, zlim = range(y))

#Final Assessment
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)
install.packages("dslabs")
#dimensions and properties
dim(brca$x)[1] #num of samples
dim(brca$x)[2] #num of predictors
mean(brca$y == "M") #proportion that are malignant
which.max(colMeans(brca$x)) #highest mean
which.min(colSds(brca$x)) #lowest sdev
# use sweep to subtract the column means and divide by the column stadard deviations
# divide by the standard deviation
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")
# SD of first column
colSds(x_scaled)[1]
sd(x_scaled[,1])
# median of first column
colMedians(x_scaled)[1]
median(x_scaled[,1])
# distance between sample 1 and other benign samples
d_samples <- dist(x_scaled)
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)])
# distance between sample 1 and malignant samples
dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM)
# max a heat map
d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)
# use hierarchical cluster on 30 features and cut the tree into 5 groups
split(names(cutree(hclust(d_features),k=5)),cutree(hclust(d_features),k=5))