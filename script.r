##########################################################
# Global Variables
##########################################################
options(digits = 5)

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
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

save.image("movieLens.RData")

##########################################################
# Libraries needed
##########################################################

# Needed for cleaning of date
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(lubridate)

# Needed for custom scatter plot display
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
library(ggrepel)

# Needed for table display
if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")
library(tibble)

##########################################################
# Exploratory Analysis
##########################################################

# Checking data types and dimensions
class(edx)
dim(edx)[1]
dim(validation)[1]

dim(edx)[2]
str(edx)

# Checking unique values for user id, movie ids, titles, genres
n_distinct(edx$userId)
n_distinct(edx$movieId) 
n_distinct(edx$title) 

# Identified two movies with same title and release year, investigating further
duplicateMovieTitle <- edx %>% select(movieId, title) %>% unique() %>% 
  group_by(title) %>% 
  summarize(n=n()) %>%
  filter(n>1)

edx %>% filter(title==duplicateMovieTitle$title) %>%
  select(movieId, genres) %>%
  group_by(movieId, genres) %>% 
  summarize(n=n()) %>%
  unique()

# This is not an error but an actual situation.
#https://www.imdb.com/title/tt0407304
#https://www.imdb.com/title/tt0449040

# Extracting all unique genres
genres <- str_extract_all(unique(edx$genres), "[^|]+") %>%
  unlist() %>%
  unique()

n_distinct(genres) # Unique genres
n_distinct(edx$genres) # Unique genres combinations

##########################################################
# Data cleaning
##########################################################

# Convert timestamp to date format
edx <- edx %>% mutate(reviewDate = round_date(as_datetime(timestamp), unit = "day"))

# Extraction of movie release year
edx <- edx %>% mutate(title = str_trim(title)) %>%
  extract(title, c("shortTitle", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = FALSE) %>%
  mutate(year = as.integer(year)) %>%
  select(-shortTitle)

# Extraction of distance freom review date to movie releae year
edx <- edx %>%
  mutate(reviewDelay = year(reviewDate)-year)

save.image("movieLens.RData")

##########################################################
# Data visualisation
##########################################################

# Investigating the target variable - Rating distribution
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth=0.5, color=I("black")) +
  scale_y_continuous() +
  labs(title = "Ratings distribution", x = "Rating value", y = "Count")

# Investigating the predictor variables against the target variable
# Movie count by average rating
edx %>% group_by(movieId) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) +
  geom_histogram(bins=50, color=I("black")) +
  labs(x = "Mean rating", y = "Movie count")

# User count by average rating
edx %>% group_by(userId) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) +
  geom_histogram(bins=50, color = I("black")) +
  labs(x = "Mean rating", y = "User count")

# Number of ratings per user
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=50, color = I("black")) +
  scale_x_log10() +
  labs(x = "User", y = "Ratings")

# List the individual genres, average by genre and not by combination
genres <- str_extract_all(unique(edx$genres), "[^|]+") %>%
  unlist() %>%
  unique()

genres_df <- as.data.frame(genres)
names(genres_df) <- c("genre")

genres_df$n <- sapply(genres, function(g) {
  index <- genres_df$genre==g
  nrow(edx[str_detect(edx$genres, g)])
})

genres_df$meanRating <- sapply(genres, function(g) {
  index <- genres_df$genre==g
  mean(edx[str_detect(edx$genres, g)]$rating)
})

genres_df$sd <- sapply(genres, function(g) {
  index <- genres_df$genre==g
  sd(edx[str_detect(edx$genres, g)]$rating)
})

genres_df$se <- genres_df$sd / sqrt(genres_df$n)
genres_df <- genres_df %>% arrange(desc(n))


genres_df %>% filter(genre!="(no genres listed)") %>%
  mutate(genre = reorder(genre, meanRating)) %>%
  ggplot(aes(x = genre, y = meanRating, ymin=meanRating - 2*se, ymax=meanRating + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Genre", y = "Average Rating")

# Number of ratings per release year
edx %>% group_by(year) %>%
  summarise(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) +
  geom_point() +
  geom_smooth(formula='y~x', method='loess', span = 0.15) +
  labs(x = "Release Year", y = "Average Rating")

# Number of ratings per release year
edx %>% group_by(year) %>%
  summarise(n = n()) %>%
  ggplot(aes(year, n)) +
  geom_point() +
  scale_y_log10() +
  labs(x = "Release Year", y = "Rating Count")

# Average rating by review date
edx %>% group_by(reviewDate) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(reviewDate, mean_rating)) +
  geom_point() +
  geom_smooth(formula='y~x', method='loess', span = 0.15) +
  labs(x = "Review Date", y = "Average Rating")

# Average ratings per review delay
edx %>% group_by(reviewDelay) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(reviewDelay, mean_rating)) +
  geom_point() +
  labs(x = "Review Delay", y = "Average Rating")

# Number of ratings per review delay
edx %>% group_by(reviewDelay) %>%
  summarise(n = n()) %>%
  ggplot(aes(reviewDelay, n)) +
  geom_point() +
  scale_y_log10()+
  labs(x = "Review Delay", y = "Rating Count")

save.image("movieLens.RData")

##########################################################
# Data Modelling
##########################################################

# Evaluation metric: Root Mean Squared Error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm=TRUE))
}

# Split data in 80:20 train:test ratios
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
d.index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
d.train <- edx[-d.index]
d.test <- edx[d.index]

# Clean-up
rm(d.index)

# Calculate the overall average rating across all movies included in train set
mu_hat <- mean(d.train$rating)

# Model 01: Simple recommender based on mean.
rmse.simple <- RMSE(d.test$rating, mu_hat)

# Model 02: Adding movie effect (b_i)
avg.movies <- d.train %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu_hat))

predicted.b_i <- mu_hat + d.test %>%
  left_join(avg.movies, by = "movieId") %>%
  pull(b_i)

rmse.movie <- RMSE(d.test$rating, predicted.b_i)

# Model 03: Adding user effect (b_u)
avg.users <- d.train %>%
  left_join(avg.movies, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_hat - b_i))

predicted.b_u <- d.test %>%
  left_join(avg.movies, by="movieId") %>%
  left_join(avg.users, by="userId") %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

rmse.user <- RMSE(predicted.b_u, d.test$rating)

# Model 04: Adding genre combination effect (b_g)
avg.genres <- d.train %>%
  left_join(avg.movies, by = "movieId") %>%
  left_join(avg.users, by = "userId") %>%
  group_by(genres) %>%
  summarise(b_g = mean(rating - mu_hat - b_i - b_u))

predicted.b_g <- d.test %>%
  left_join(avg.movies, by = "movieId") %>%
  left_join(avg.users, by = "userId") %>%
  left_join(avg.genres, by = "genres") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  pull(pred)

rmse.genre <- RMSE(predicted.b_g, d.test$rating)

# Model 05: Adding movie release year effect (b_y)
avg.years <- d.train %>%
  left_join(avg.movies, by = "movieId") %>%
  left_join(avg.users, by = "userId") %>%
  left_join(avg.genres, by = "genres") %>%
  group_by(year) %>%
  summarise(b_y = mean(rating - mu_hat - b_i - b_u - b_g))

predicted.b_y <- d.test %>%
  left_join(avg.movies, by = "movieId") %>%
  left_join(avg.users, by = "userId") %>%
  left_join(avg.genres, by = "genres") %>%
  left_join(avg.years, by = "year") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_y) %>%
  pull(pred)

rmse.year <- RMSE(predicted.b_y, d.test$rating)

# Model 06: Adding review delay effect (b_d)
avg.delays <- d.train %>%
  left_join(avg.movies, by = "movieId") %>%
  left_join(avg.users, by = "userId") %>%
  left_join(avg.genres, by = "genres") %>%
  left_join(avg.years, by="year") %>%
  group_by(reviewDelay) %>%
  summarise(b_d = mean(rating - mu_hat - b_i - b_u - b_g))

predicted.b_d <- d.test %>%
  left_join(avg.movies, by = "movieId") %>%
  left_join(avg.users, by = "userId") %>%
  left_join(avg.genres, by = "genres") %>%
  left_join(avg.years, by = "year") %>%
  left_join(avg.delays, by = "reviewDelay") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_y + b_d) %>%
  pull(pred)

rmse.delay <- RMSE(predicted.b_d, d.test$rating)

# Model 07: Regularization of models.

# Generate a sequence of values for lambda ranging from 4 to 6 with 0.05 increments
inc <- 0.05
lambdas <- seq(4, 6, inc)

rmses <- sapply(lambdas, function(l){
  b_i <- d.train %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu_hat)/(n()+l))
  b_u <- d.train %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu_hat)/(n()+l))
  b_g <- d.train %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarise(b_g = sum(rating - b_i - b_u - mu_hat)/(n()+l))
  b_y <- d.train %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(year) %>%
    summarise(b_y = sum(rating - b_i - b_u - b_g - mu_hat)/(n()+l))
  b_d <- d.train %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="year") %>%
    group_by(reviewDelay) %>%
    summarise(b_d = sum(rating - b_i - b_u - b_g - mu_hat)/(n()+l))
  predicted_ratings <- d.test %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="year") %>%
    left_join(b_d, by="reviewDelay") %>%
    mutate(pred = mu_hat + b_i + b_u + b_g + b_y + b_d) %>%
    pull(pred)
  return(RMSE(predicted_ratings, d.test$rating))
})

# Get best Lambda and RMSE for regularized model
lambda <- lambdas[which.min(rmses)]
rmse.regularized <- min(rmses) 

# Plot Lambdas vs RMSE
d <- as.data.frame(lambdas)
d$rmses <- rmses
names(d) <- c("lambdas", "rmses")

ggplot(d, aes(lambdas, rmses)) +
  geom_point() +xlab("Lambda") + ylab("RMSE")+
  geom_label_repel(data=subset(d, lambdas ==lambda),aes(label = lambdas), color = 'blue',
                   size = 3.5, box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.3, "lines"))

save.image("movieLens.RData")

##########################################################
# Results
##########################################################

# Gather all results from models undertaken
rmse.results <- 
  tibble(Model = "Naive Prediction (Average)", RMSE = rmse.simple)
rmse.results <- bind_rows(
  rmse.results, tibble(Model = "+ Movie Effect",RMSE = rmse.movie))
rmse.results <- bind_rows(
  rmse.results, tibble(Model = "+ User Effect",RMSE = rmse.user))
rmse.results <- bind_rows(
  rmse.results, tibble(Model = "+ Genre Combination Effect",
                       RMSE = rmse.genre))
rmse.results <- bind_rows(
  rmse.results, tibble(Model = "+ Release Year Effect",
                       RMSE = rmse.year))
rmse.results <- bind_rows(
  rmse.results, tibble(Model = "+ Review Delay Effect",
                       RMSE = rmse.delay))
rmse.results <- bind_rows(
  rmse.results, tibble(Model = "+ Regularized ",
                       RMSE = rmse.regularized))
rmse.results


# Apply to validation (Final hold-out)

##########################################################
# Data cleaning on validation
##########################################################
# Convert timestamp into date format, removing time data
validation <- validation %>% mutate(reviewDate = round_date(as_datetime(timestamp), unit = "week"))

# Extraction of release year
validation <- validation %>% mutate(title = str_trim(title)) %>%
  extract(title, c("shortTitle", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = FALSE) %>%
  mutate(year = as.integer(year)) %>%
  select(-shortTitle)

# Identification of reviews submitted prior to movie release year
validation <- validation %>%
  mutate(reviewDelay = year(reviewDate)-year)

##########################################################
# Data modelling on validation
##########################################################
# Regularized model on mean with movie, user, genre combination, release year and review delay effects.
b_i <- edx %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu_hat)/(n()+lambda))
b_u <- edx %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - b_i - mu_hat)/(n()+lambda))
b_g <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres) %>%
  summarise(b_g = sum(rating - b_i - b_u - mu_hat)/(n()+lambda))
b_y <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  group_by(year) %>%
  summarise(b_y = sum(rating - b_i - b_u - b_g - mu_hat)/(n()+lambda))
b_d <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  group_by(reviewDelay) %>%
  summarise(b_d = sum(rating - b_i - b_u - b_g - mu_hat)/(n()+lambda))
predicted.final <- validation %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  left_join(b_d, by="reviewDelay") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_y + b_d) %>%
  pull(pred)

##########################################################
# Results on validation
##########################################################
rmse.valid <- RMSE(validation$rating, predicted.final)

##########################################################
# Save for use in markdown
##########################################################
save.image("movieLens.RData")
