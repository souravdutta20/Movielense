# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric((movieId)),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

set.seed(1, sample.kind="Rounding")

# To add year of movie rated and age of movies
library(lubridate)
#Convert Timestamp to year
movielens_timestamp <- mutate(movielens, year_rated = year(as_datetime(timestamp)))


head(movielens_timestamp)

#extracting the First Movie date
premier <- stringi::stri_extract(movielens_timestamp$title, regex = "(\\d{4})", comments = TRUE ) %>% as.numeric()

#Add the First Movie Date
movielens_with_title_dates <- movielens_timestamp %>% mutate(premier_date = premier)
head(movielens_with_title_dates)

#drop the timestamp
movielens_with_title_dates <- movielens_with_title_dates %>% select(-timestamp)



#looking at the dates - are they correct? Year between 1997 to 2018

movielens_with_title_dates %>% filter(premier_date > 2018) %>% group_by(movieId, title, premier_date) %>% summarize(n = n())

movielens_with_title_dates %>% filter(premier_date < 1900) %>% group_by(movieId, title, premier_date) %>% summarize(n = n())

#Fix the incorrect dates
movielens_with_title_dates[movielens_with_title_dates$movieId == "27266", "premier_date"] <- 2004
movielens_with_title_dates[movielens_with_title_dates$movieId == "671", "premier_date"] <- 1996
movielens_with_title_dates[movielens_with_title_dates$movieId == "2308", "premier_date"] <- 1973
movielens_with_title_dates[movielens_with_title_dates$movieId == "4159", "premier_date"] <- 2001
movielens_with_title_dates[movielens_with_title_dates$movieId == "5310", "premier_date"] <- 1985
movielens_with_title_dates[movielens_with_title_dates$movieId == "8864", "premier_date"] <- 2004
movielens_with_title_dates[movielens_with_title_dates$movieId == "1422", "premier_date"] <- 1997
movielens_with_title_dates[movielens_with_title_dates$movieId == "4311", "premier_date"] <- 1998
movielens_with_title_dates[movielens_with_title_dates$movieId == "5472", "premier_date"] <- 1972
movielens_with_title_dates[movielens_with_title_dates$movieId == "6290", "premier_date"] <- 2003
movielens_with_title_dates[movielens_with_title_dates$movieId == "6645", "premier_date"] <- 1971
movielens_with_title_dates[movielens_with_title_dates$movieId == "8198", "premier_date"] <- 1960
movielens_with_title_dates[movielens_with_title_dates$movieId == "8905", "premier_date"] <- 1992
movielens_with_title_dates[movielens_with_title_dates$movieId == "53953", "premier_date"] <- 2007

# Cross Checking

movielens_with_title_dates %>% filter(premier_date > 2018) %>% group_by(movieId, title, premier_date) %>% summarize(n = n())

movielens_with_title_dates %>% filter(premier_date < 1900) %>% group_by(movieId, title, premier_date) %>% summarize(n = n())


#Calculate the age of the movie

#Calculate the age of a movie
movielens_with_title_dates <- movielens_with_title_dates %>% mutate(age_of_movie = 2020 - premier_date, rating_date_range = year_rated - premier_date)
head(movielens_with_title_dates)
#year the movie was rated
year_avgs <- movielens_with_title_dates%>% group_by(year_rated) %>% summarize(avg_rating_by_year = mean(rating)) 
#age of movie
age_avgs <- movielens_with_title_dates %>% group_by(age_of_movie) %>% summarize(avg_rating_by_age = mean(rating)) 

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens_with_title_dates$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens_with_title_dates[-test_index,]
temp <- movielens_with_title_dates[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Data Validation
# number of rows and columns
dim(edx)
dim(validation)
# checking if there is any 0 in Ratings 
edx %>% filter(rating == 0) %>% tally()
# How many differnt movies in edx
n_distinct(edx$movieId)
# How many differnt users in edx
n_distinct(edx$userId)
#any missing value in edx
edx[!complete.cases(edx),]

# 
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")



edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

# Test set creation
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]
# First Model(movie rating)
mu_hat <- mean(train_set$rating)
mu_hat
## predict unknown ratings with mu_hat
rmse_movie_rating<- RMSE(test_set$rating, mu_hat)

# Creating a result table of RMSE
rmse_results <- tibble(method = "Same rating for all movies", RMSE = rmse_movie_rating)
rmse_results

# 2nd Model: Different movies are reted differently

mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
# variance of rating by movie
qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

## predict unknown ratings with b_i
#Root Mean Square Error Loss Function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2,na.rm=TRUE))
}

b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

#predict ratings in the test set 
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  .$pred
rmse_movie_effect<-RMSE(predicted_ratings, test_set$rating)

# updating result table of RMSE
rmse_results <- bind_rows (rmse_results,tibble(method = "Adjusted mean by Movie effect", RMSE = rmse_movie_effect))
rmse_results



# 3rd Model: Different users rated movies differently

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

qplot(b_u, data = user_avgs, bins = 10, color = I("black"))

#ajdust mean by user and movie effect 
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - b_i - mu))

#predict ratings in the test set 
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
rmse_user_effect<-RMSE(predicted_ratings, test_set$rating)

# updating result table of RMSE
rmse_results <- bind_rows (rmse_results,tibble(method = "Adjusted mean by Movie  & User effect", RMSE = rmse_user_effect))
rmse_results

train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i)) %>%
  ggplot(aes(userId, b_u)) +
  geom_point() +
  geom_smooth()

# is movie rating year significant for model improvement

edx %>% 
  group_by(year_rated) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year_rated, rating)) +
  geom_point() +
  geom_smooth()


year_rating_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(year_rated) %>%
  summarize(b_y= mean(rating - mu - b_i-b_u))

qplot(b_y, data = year_rating_avgs, bins = 10, color = I("black"))

# Do we need to consider age of movie

edx %>% 
  group_by(age_of_movie) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(age_of_movie, rating)) +
  geom_point() +
  geom_smooth()

#Graph looks more linear between age between 25 and 75 years

age_avgs <- edx %>% group_by(age_of_movie) %>% summarize(avg_rating_by_age = mean(rating)) 

age_between25_and_75 <- age_avgs %>% filter((age_of_movie > 25) & (age_of_movie < 75))
#plot the graph
age_between25_and_75 %>%
  ggplot(aes(age_of_movie, avg_rating_by_age)) +
  geom_point() + ggtitle("Movies between 25 and 75 years old vs average movie rating")+
  geom_smooth()

# R-square
summary(lm(avg_rating_by_age ~ age_of_movie, data = age_between25_and_75))

# Correlation

#Number of movie ratings per movie
n_movies_ratings <- edx %>% group_by(movieId) %>% summarize(n = n())

#Average Movie Rating for each movie
avg_movie_rat <- edx %>% group_by(movieId) %>% summarize(avg_m_r = mean(rating))

#Create correlation data
cor_dat <- edx %>% select(rating, movieId, userId, year_rated, age_of_movie, rating_date_range, premier_date) %>%
  left_join(n_movies_ratings, by = "movieId") %>%
  left_join(avg_movie_rat, by = 'movieId')
head(cor_dat)


corr_by_age_of_movie <- cor_dat %>% filter((age_of_movie >25) & (age_of_movie < 70))
head(corr_by_age_of_movie)


cor(data.frame(x = corr_by_age_of_movie$age_of_movie, y = corr_by_age_of_movie$avg_m_r))


age_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(age_of_movie) %>%
  summarize(b_a = mean(rating - mu - b_i-b_u))

qplot(b_a, data = age_avgs, bins = 10, color = I("black"))

#ajdust mean by user, movie effect and age of movie effect
b_a <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(age_of_movie) %>%
  summarize(b_a = mean(rating - b_i - mu - b_u))

#predict ratings in the test set 
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_a, by = "age_of_movie") %>%
  mutate(pred = mu + b_i + b_u + b_a) %>%
  .$pred
rmse_age_movie_effect<-RMSE(predicted_ratings, test_set$rating)

# updating result table of RMSE
rmse_results <- bind_rows (rmse_results,tibble(method = "Adjusted mean by Movie  & User effect & age of movie", RMSE = rmse_age_movie_effect))
rmse_results

#Root Mean Square Error Loss Function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2,na.rm=TRUE))
}
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas,function(l){
  
  #Calculate the mean of ratings from the edx training set
  mu <- mean(train_set$rating)
  
  #Adjust mean by movie effect and penalize low number on ratings
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  #ajdust mean by user and movie effect and penalize low number of ratings
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  #ajdust mean by user and movie effect and age of movie penalize low number of ratings
  b_a <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(age_of_movie) %>%
    summarize(b_a = sum(rating - b_i - mu - b_u)/(n()+l))
  
  #predict ratings in the test set to derive optimal penalty value 'lambda'
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_a, by = "age_of_movie") %>%
    mutate(pred = mu + b_i + b_u + b_a) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})


lambda <- lambdas[which.min(rmses)]
paste('Optimal RMSE of',min(rmses),'is achieved with Lambda',lambda)
qplot(lambdas, rmses)

# validation data 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2,na.rm=TRUE))
}
lambdas <- 4.5

RMSE_validation <- sapply(lambdas,function(l){
  
  
  #Calculate the mean of ratings 
  mu <- mean(train_set$rating)
  
  #Adjust mean by movie effect and penalize low number on ratings
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  #ajdust mean by user and movie effect and penalize low number of ratings
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  #ajdust mean by user and movie effect and age of movie penalize low number of ratings
  b_a <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(age_of_movie) %>%
    summarize(b_a = sum(rating - b_i - mu - b_u)/(n()+l))
  
  #predict ratings in the validation set to derive optimal penalty value 'lambda'
    predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_a, by = "age_of_movie") %>%
    mutate(pred = mu + b_i + b_u + b_a) %>%
    .$pred
  
  return(RMSE(predicted_ratings, validation$rating))
})
RMSE_validation