library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()
#Scatterplot of the relationship between HRs and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
#Scatterplot of the relationship between stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
#Scatterplot of the relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
#Scatterplot of the relationship between at-bats per game and runs per game.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
#Scatterplot of the relationship between wins per game and number of fielding errors
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(W_per_game = W/G, E_per_game = E/G) %>%
  ggplot(aes(E_per_game, W_per_game)) + 
  geom_point(alpha = 0.5)
#Scatterplot of triples per game vs doubles per game
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(T_per_game = X3B/G, D_per_game = X2B/G) %>%
  ggplot(aes(T_per_game, D_per_game)) + 
  geom_point(alpha = 0.5)

# create the dataset
install.packages("HistData")
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))
# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)
# The correlation coefficient is defined for a list of pairs  as the product of the standardized values
rho <- mean(scale(x)*scale(y))
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)
# compute sample correlation
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R
# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
# expected value and standard error
mean(R)
sd(R)
# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

#correlation coefficient between number of runs per game and number of at bats per game?
Teams %>% filter(yearID %in% 1961:2001 ) %>% 
  mutate(R_per_game = R/G,AB_per_game = AB/G) %>% 
  summarize(r = cor(R_per_game, AB_per_game)) %>%
  pull(r)
#correlation coefficient between win rate (number of wins per game) and number of errors per game
Teams %>% filter(yearID %in% 1961:2001 ) %>% 
  mutate(W_per_game = W/G,E_per_game = E/G) %>% 
  summarize(r = cor(W_per_game, E_per_game)) %>%
  pull(r)
#correlation coefficient between doubles (X2B) per game and triples (X3B) per game
Teams %>% filter(yearID %in% 1961:2001 ) %>% 
  mutate(X2B_per_game = X2B/G,X3B_per_game = X3B/G) %>% 
  summarize(r = cor(X2B_per_game, X3B_per_game)) %>%
  pull(r)

# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)
# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg
# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()
# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()
# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
#slope(m) is the correlation coefficient (r) of the son and father heights times the sdev sons' heights (s_y) divided by sdev fathers' heights (s_x).
m <- r * s_y/s_x 
b <- mu_y - m*mu_x
# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)
# stratify son hights but stratified father heights
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)
# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x
# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y

#analyze a set of mother and daughter heights, also from GaltonFamilies
#Define female_heights, a set of mother and daughter heights sampled from GaltonFamilies
set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
#mean and sdev of both mother's daughters and cor between them
female_heights %>%
summarize(mean(mother), sd(mother), mean(daughter), sd(daughter), cor(mother,daughter))
#slope and intercept of regression line predicting daughter's heights given mother's heights
mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
#slope(m) is the correlation coefficient (r) of the son and father heights times the sdev sons' heights (s_y) divided by sdev fathers' heights (s_x).
m <- r * s_y/s_x 
b <- mu_y - m*mu_x
#percent of the variability in daughter heights is explained by the mother's height
r^2*100
#conditional expected value of her daughter's height given the mother's height=60
m*60+b

# Confounding
# find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope
# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope
# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))
# find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  summarize(slope = get_slope(BB_per_game, R_per_game))
bb_slope
# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  summarize(slope = get_slope(Singles_per_game, R_per_game))
singles_slope
# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))
# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)
# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)
# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))
# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 
# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)
# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 

# compute RSS for any pair of beta0 and beta1 in Galton's data
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}
# plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))
# fit regression line to predict son's height from father's height
fit <- lm(son ~ father, data = galton_heights)
fit
# summary statistics
summary(fit)
# Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
# Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)
# summary statistics
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef
lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))
# LSE can be strongly correlated
lse %>% summarize(cor(beta_0, beta_1))
# depends on how data is defined or transformed standardized for father h
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})
#changes the LSE correlation
cor(lse[1,], lse[2,]) 
# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")
# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)
# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()

#linear model for runs/game based on both basesonballs/game and hr/game
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
Teams_small %>% 
  mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game = HR/G) %>% 
  lm(R_per_game ~ BB_per_game + HR_per_game, data = .)
#plot a linear model with predictions and confidence intervals of son's heights
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")
#or piecemeal with the model, predictions, and full data as 3 plots on top of each other
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))
#Define female_heights, a set of mother and daughter heights sampled from GaltonFamilies, as follows:
set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits
female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
#fit a linear regression model prediction mother's h using daughter's h
fit <- lm(mother ~ daughter, data = female_heights)
fit$coef[2]
#predict the first mother's h with first daughter's h using the model
predict(fit)[1]
#create table of players with more than 100 plate appearances (pa) for 2002
#singles = hits less doubles, tripples, and home runs
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
#similar table for 1999-2001
bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
#number of players with mean_singles of 0.2 or more
sum(bat_99_01$mean_singles > 0.2)
#number of players with mean_bb of 0.2 or more
sum(bat_99_01$mean_bb > 0.2)
#join results into one table
bat_99_01vs02 <- inner_join(bat_99_01,bat_02)
#correlation between 02 and 99-01 singles rates
bat_99_01vs02 %>% summarize(r = cor(singles,mean_singles)) %>% pull(r)
#correlation between 02 and 99-01 bb rates
bat_99_01vs02 %>% summarize(r = cor(bb,mean_bb)) %>% pull(r)
#scatter plot mean_singles versus singles and mean_bb versus bb
bat_99_01vs02 %>% ggplot(aes(mean_singles, singles)) +
  geom_point() +
  geom_smooth(method = "lm")
bat_99_01vs02 %>% ggplot(aes(mean_bb, bb)) +
  geom_point() +
  geom_smooth(method = "lm")
#coefficient of mean_singles in an lm
lm(singles ~ mean_singles, data = bat_99_01vs02 )
#coefficient of mean_bbin an lm
lm(bb ~ mean_bb, data = bat_99_01vs02 )

# tibbles
# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)
# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))
# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef
# inspect a grouped tibble
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()
# inspect data frame and tibble
Teams
as_tibble(Teams)
# Note that the function was formerly called as.tibble()
# subsetting a data frame sometimes generates vectors
class(Teams[,20])
# subsetting a tibble always generates tibbles
class(as_tibble(Teams[,20]))
# pulling a vector out of a tibble
class(as_tibble(Teams)$HR)
# access a non-existing column in a data frame or a tibble
Teams$hr
as_tibble(Teams)$HR
# create a tibble with complex objects
tibble(id = c(1, 2, 3), func = c(mean, median, sd))
#do() fuction
# use do to fit a regression line to each HR stratum
dat %>%  
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))
# using do without a column name gives an error
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .))
# define a function to extract slope from lm
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}
# return the desired data frame
dat %>%  
  group_by(HR) %>%
  do(get_slope(.))
# not the desired output: a column containing data frames
dat %>%  
  group_by(HR) %>%
  do(slope = get_slope(.))
# data frames with multiple rows will be concatenated appropriately
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             estimate = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}
dat %>%  
  group_by(HR) %>%
  do(get_lse(.))
# use tidy to return lm estimates and related information as a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)
# add confidence intervals with tidy
tidy(fit, conf.int = TRUE)
# pipeline with lm, do, tidy
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)
# make ggplots
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()
# inspect with glance
glance(fit)

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

#assessment: tibbles, do, broom
#create the galton dataset using the code below:
library(tidyverse)
library(HistData)
data("GaltonFamilies")
#set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))
galton
#group by pair and count the number of each pair of parent child relationships
galton %>%
  group_by(pair) %>%
  summarize(n = n())
#calculate correlation coefficents for each pair
galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight))
#find biggest or smallest correlation
galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == max(cor))
galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == min(cor))
#use lm() and broom to fit regression lines, compute LSE, SE, Confidence intervals, and p-values for each pair
galton %>%  
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight")

#baseball
# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)
# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs
# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()
# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean
# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))
# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))
# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")
# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))
# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")
# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 
# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()
# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()
# linear programming for player selection
library(reshape2)
library(lpSolve)
players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 
# choose 9 players
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)
# stats on the players
my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))
# new stat OPS = BB/PA + (Singles+2Doubles+3Triples+4HomeRuns)/AB
# The code to create a table with player ID, their names, and their most played position:
  library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)
# The code to create a table with only the ROY award winners and add their batting statistics:
  ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")
# The code to keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons:
  ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)
# The code to use the spread function to have one column for the rookie and sophomore years batting averages:
  ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY
# The code to calculate the proportion of players who have a lower batting average their sophomore year:
  mean(ROY$sophomore - ROY$rookie <= 0)
# The code to do the similar analysis on all players that played the 2013 and 2014 seasons and batted more than 130 times (minimum to win Rookie of the Year):
  two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years
# The code to see what happens to the worst performers of 2013:
  arrange(two_years, `2013`)
# The code to see  the correlation for performance in two separate years:
  qplot(`2013`, `2014`, data = two_years)
summarize(two_years, cor(`2013`,`2014`))
#The code to use dslabs function rfalling_object to generate simulations of dropping balls:
  library(dslabs)
falling_object <- rfalling_object()
#The code to draw the trajectory of the ball:
  falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")
#The code to use the lm() function to estimate the coefficients:
  fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)
tidy(fit)
# The code to check if the estimated parabola fits the data:
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")
# The code to see the summary statistic of the regression:
  tidy(fit, conf.int = TRUE)
# Multivariate LM for effect of BB and HR on R in 1971
  Teams %>%
    filter(yearID == 1971) %>%
    lm(R ~ BB + HR, data = .) %>%
    tidy()
# Make a scatterplot of the estimate for the effect of BB on runs over time and add a trend line with confidence intervals.
  res <- Teams %>%
    filter(yearID %in% 1961:2018) %>%
    group_by(yearID) %>%
    do(tidy(lm(R ~ BB + HR, data = .))) %>%
    ungroup() 
  res %>%
    filter(term == "BB") %>%
    ggplot(aes(yearID, estimate)) +
    geom_point() +
    geom_smooth(method = "lm")
# Fit a linear model for the effect of year on the impact of BB with p value
  res %>%
    filter(term == "BB") %>%
    lm(estimate ~ yearID, data = .) %>%
    tidy()
  
#calculate average attendance, divide by the number of games played
  library(tidyverse)
  library(broom)
  library(Lahman)
  Teams_small <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(avg_attendance = attendance/G)
  # find regression line predicting attendance from R/G and take slope
  Teams_small %>% 
    mutate(R_per_game = R/G) %>% 
    lm(avg_attendance ~ R_per_game, data = .) %>% 
    .$coef %>%
    .[2]
  # find regression line predicting attendance from HR/G and take slope
  Teams_small %>% 
    mutate(HR_per_game = HR/G) %>% 
    lm(avg_attendance ~ HR_per_game, data = .) %>% 
    .$coef %>%
    .[2]
  # find regression line predicting attendance from Wins and take slope and intercept
  Teams_small %>% 
    lm(avg_attendance ~ W, data = .) %>% 
    .$coef
  # predict attendance with year
  Teams_small %>% 
    lm(avg_attendance ~ yearID, data = .) %>% 
    .$coef
  # correlation coefficient for runs per game and wins
  Teams_small %>% 
    mutate(R_per_game = R/G)  %>%
    summarize(cor(R_per_game,W))
  # correlation coefficent for home runs per game and wins
  Teams_small %>% 
    mutate(HR_per_game = HR/G)  %>%
    summarize(cor(HR_per_game,W))
  # stratify by wins/10 rounded to nearest int keeping only strata 5 through 10 with 20 or more data points
  dat <- Teams_small %>%
    mutate(W_strata = round(W/10)) %>%
    filter(W_strata >= 5 & W_strata <= 10)
  # how many observations in strata 8
  sum(dat$W_strata == 8)
  # calculate slope of regression line after stratifying by R per game
  dat %>%  
    group_by(W_strata) %>%
    summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(R/G))
  # calculate slope of regression line after stratifying by HR per game
  dat %>%  
    group_by(W_strata) %>%
    summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))
  # estimate effect of runs per game on avg attendance
  fit <- Teams_small %>% 
    mutate(R_per_game = R/G,
           HR_per_game = HR/G) %>%
    lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)
  tidy(fit) %>%
    filter(term == "R_per_game") %>%
    pull(estimate)
  # effect of home runs per game on average attendance
  tidy(fit) %>%
    filter(term == "HR_per_game") %>%
    pull(estimate)
  # estimate effect of number of wins in a season on average attendance
  tidy(fit) %>%
    filter(term == "W") %>%
    pull(estimate)
  #team averaged 4 R/G 1.2 HR/G and 80 W in a season
  # estimate average attendance in 2002
  predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002))
  # stimate average attendance in 1960
  predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960))
  # fid correlation between predicted and actual attendance
  newdata <- Teams %>%
    filter(yearID == 2002) %>%
    mutate(avg_attendance = attendance/G,
           R_per_game = R/G,
           HR_per_game = HR/G)
  preds <- predict(fit, newdata)
  cor(preds, newdata$avg_attendance)
  
#Correlation is not causation
#p-hacking is when a data set is picked based on the p-value generated
  # generate the Monte Carlo simulation
  N <- 25
  g <- 1000000
  sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))
  # calculate correlation between X,Y for each group
  res <- sim_data %>% 
    group_by(group) %>% 
    summarize(r = cor(x, y)) %>% 
    arrange(desc(r))
  res
  # plot points from the group with maximum correlation
  sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
    ggplot(aes(x, y)) +
    geom_point() + 
    geom_smooth(method = "lm")
  # histogram of correlation in Monte Carlo simulations
  res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")
  # linear regression on group with maximum correlation
  library(broom)
  sim_data %>% 
    filter(group == res$group[which.max(res$r)]) %>%
    do(tidy(lm(y ~ x, data = .)))
  # correlation caused by outliers
  # The Spearman correlation is calculated based on the ranks of data
  # simulate independent X, Y and standardize all except entry 23
  set.seed(1985)
  x <- rnorm(100,100,1)
  y <- rnorm(100,84,1)
  x[-23] <- scale(x[-23])
  y[-23] <- scale(y[-23])
  # plot shows the outlier
  qplot(x, y, alpha = 0.5)
  # outlier makes it appear there is correlation
  cor(x,y)
  cor(x[-23], y[-23])
  # use rank instead
  qplot(rank(x), rank(y))
  cor(rank(x), rank(y))
  # Spearman correlation with cor function
  cor(x, y, method = "spearman")
  # Another way association can be confused with causation is when the cause and effect are reversed.
  # With Galton data, father and son reversed he model was technically correct.Interpretation of the model is wrong though.
  # cause and effect reversal using son heights to predict father heights
  library(HistData)
  data("GaltonFamilies")
  GaltonFamilies %>%
    filter(childNum == 1 & gender == "male") %>%
    select(father, childHeight) %>%
    rename(son = childHeight) %>% 
    do(tidy(lm(father ~ son, data = .)))
  #If X and Y are correlated, we call Z a confounder if changes in Z causes changes in both X and Y.
  # UC-Berkeley admission data
  library(dslabs)
  data(admissions)
  admissions
  # percent men and women accepted
  admissions %>% group_by(gender) %>% 
    summarize(percentage = 
                round(sum(admitted*applicants)/sum(applicants),1))
  # test whether gender and admission are independent
  admissions %>% group_by(gender) %>% 
    summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
              not_admitted = sum(applicants) - sum(total_admitted)) %>%
    select(-gender) %>% 
    do(tidy(chisq.test(.)))
  # percent admissions by major
  admissions %>% select(major, gender, admitted) %>%
    spread(gender, admitted) %>%
    mutate(women_minus_men = women - men)
  # plot total percent admitted to major versus percent women applicants
  admissions %>% 
    group_by(major) %>% 
    summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
              percent_women_applicants = sum(applicants * (gender=="women")) /
                sum(applicants) * 100) %>%
    ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
    geom_text()
  # plot number of applicants admitted and not
  admissions %>%
    mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
    select(-applicants, -admitted) %>%
    gather(admission, number_of_students, -c("major", "gender")) %>%
    ggplot(aes(gender, number_of_students, fill = admission)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(. ~ major)
  admissions %>% 
    mutate(percent_admitted = admitted * applicants/sum(applicants)) %>%
    ggplot(aes(gender, y = percent_admitted, fill = major)) +
    geom_bar(stat = "identity", position = "stack")
  # condition on major and then look at differences
  admissions %>% ggplot(aes(major, admitted, col = gender, size = applicants)) + geom_point()
  # average difference by major
  admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))
  # Simpson’s Paradox happens when we see the sign of the correlation flip when comparing the entire dataset with specific strata. 
  
  #confounding men vs women awarded funding
  library(dslabs)
  data("research_funding_rates")
  research_funding_rates
  #two by two table of gender by award status
  two_by_two <- research_funding_rates %>% 
    select(-discipline) %>% 
    summarize_all(funs(sum)) %>%
    summarize(yes_men = awards_men, 
              no_men = applications_men - awards_men, 
              yes_women = awards_women, 
              no_women = applications_women - awards_women) %>%
    gather %>%
    separate(key, c("awarded", "gender")) %>%
    spread(gender, value)
  two_by_two
  #compute % of awarded by gender
  two_by_two %>% 
    mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
    filter(awarded == "yes") %>%
    pull(men,women)
  #chi-squared test on the two-by-two table
  two_by_two %>% select(-awarded) %>% chisq.test() %>% tidy() %>% pull(p.value)
  # create dataset with number of applications, awards, and success rate for each gender
  dat <- research_funding_rates %>% 
    mutate(discipline = reorder(discipline, success_rates_total)) %>%
    rename(success_total = success_rates_total,
           success_men = success_rates_men,
           success_women = success_rates_women) %>%
    gather(key, value, -discipline) %>%
    separate(key, c("type", "gender")) %>%
    spread(type, value) %>%
    filter(gender != "total")
  dat
  #plot the rates by gender stratafied by field
  dat %>% 
    ggplot(aes(discipline, success, size = applications, color = gender)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_point()
  