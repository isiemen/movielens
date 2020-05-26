library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(randomForest)
library(caret)
library(data.table)
library(viridis)
library(tidyr)
library(plyr)

options(digits = 6)
memory.limit(200000)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#only used for the first time to download the dataset
#dl <- tempfile()
#download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#access the data files locally:
ratings <- fread(text = gsub("::", "\t", readLines("ml-10M100K/ratings.dat")),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines( "ml-10M100K/movies.dat"), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

#Extract year from title, convert timestamp to date and year
movielens<-movielens%>%mutate(review_date = as.Date(as.POSIXct(timestamp, origin="1970-01-01")),
                              review_year = as.integer(format(review_date,"%Y")),
                              movie_year = as.integer(str_extract(str_sub(title, -5),"\\d{4}")),
                              n_years_release_review = as.numeric(review_year)-as.numeric(movie_year))

# Validation set will be 10% of MovieLens data
set.seed(1)
temp_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-temp_index,]
temp <- movielens[temp_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm( ratings, movies, temp_index, temp, movielens, removed)

# Create a test data set that is 10% of the edx dataset
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx <- edx[-test_index,]
test <- edx[test_index,]

#explore the data with various counts and summaries
dim(edx)
n_rows<-edx%>%dplyr::summarize(n=n())%>%pull(n)

edx%>%filter(rating==0)%>%dplyr::summarize(n=n())
edx%>%distinct(movieId)%>%dplyr::summarize(n=n())
edx%>%distinct(userId)%>%dplyr::summarize(n=n())
edx%>%distinct(genres)%>%dplyr::summarize(n=n())

sum(str_count(edx$genres,"Drama"))
sum(str_count(edx$genres,"Comedy"))
sum(str_count(edx$genres,"Thriller"))
sum(str_count(edx$genres,"Romance"))

edx%>%group_by(movieId,title)%>%dplyr::summarize(n=n())%>%arrange(desc(n))
edx%>%group_by(rating)%>%dplyr::summarize(n=n())%>%arrange(desc(n))

#Remove NaN values from edx, test, and validation datasets
edx<-edx[complete.cases(edx), ]
test<-test[complete.cases(test), ]
validation<-validation[complete.cases(validation), ]

#Create a list of unique words in the genres column
genre_list<-str_split(edx$genres,pattern="\\|",simplify = TRUE)
genre_list<-as.data.frame(genre_list)%>%
  gather(genre_list)

genre_list<-genre_list%>%
  distinct(value)

#number of reviews and average rating per genre
genre_summary<-function(x){
  tab<-edx%>%mutate(match=str_count(edx$genres,x))%>%filter(match==1)
  n<-sum(tab$match)
  avg_rating<-mean(tab$rating)
  data.frame(n=n,rating=avg_rating,genre=x)
}
genre_tab<-rbind.fill(apply(genre_list,FUN=genre_summary,MARGIN = 1))%>%
  arrange(desc(n))
genre_tab

#Scatterplot the Genre vs Rating
genre_tab%>%ggplot(aes(x=rating,y=n,color=genre, label = genre))+
  geom_point()+
  geom_text( nudge_x = 0.07)+
  ggtitle("Average Rating and Total Reviews by Genre")+
  xlab("Average Rating")+
  ylab("Total Reviews")+
  theme(legend.position = "none")

# 93.6% of movies list one of the top 4 genres.
top_4_genre_count<-edx%>%
  filter(str_count(genres,"Drama")==1 |
           str_count(genres,"Comedy")==1 |
           str_count(genres,"Action")==1 |
           str_count(genres,"Thriller")==1)%>%
  dplyr::summarize(n=n())%>%
  pull(n)

top_4_genre_count/n_rows

#histogram of user reviews per top 4 most common genres
edx%>%filter(str_count(genres,"Drama")==1)%>%
  group_by(userId)%>%
  dplyr::summarise(avg_rating = mean(rating))%>%
  ggplot(aes(avg_rating))+
  geom_histogram()+
  xlab("Average Rating")+
  ylab("Number of Users")+
  ggtitle("Average Rating Users give Drama Movies")+
  theme_minimal()

edx%>%filter(str_count(genres,"Comedy")==1)%>%
  group_by(userId)%>%
  dplyr::summarise(avg_rating = mean(rating))%>%
  ggplot(aes(avg_rating))+
  geom_histogram()+
  xlab("Average Rating")+
  ylab("Number of Users")+
  ggtitle("Average Rating Users give Comedy Movies")+
  theme_minimal()

edx%>%filter(str_count(genres,"Action")==1)%>%
  group_by(userId)%>%
  dplyr::summarise(avg_rating = mean(rating))%>%
  ggplot(aes(avg_rating))+
  geom_histogram()+
  xlab("Average Rating")+
  ylab("Number of Users")+
  ggtitle("Average Rating Users give Action Movies")+
  theme_minimal()

edx%>%filter(str_count(genres,"Thriller")==1)%>%
  group_by(userId)%>%
  dplyr::summarise(avg_rating = mean(rating))%>%
  ggplot(aes(avg_rating))+
  geom_histogram()+
  xlab("Average Rating")+
  ylab("Number of Users")+
  ggtitle("Average Rating Users give Thriller Movies")+
  theme_minimal()

#boxplot of top 12 users and their ratings for top 4 genres
top_12_users<-edx%>%group_by(userId)%>%
  dplyr::summarize(n = n())%>%
  arrange(desc(n))%>%
  top_n(12)

edx_genre<-edx%>%
  filter(userId %in% top_12_users$userId,
         str_count(genres,"Drama")==1 |
           str_count(genres,"Comedy")==1 |
           str_count(genres,"Action")==1 |
           str_count(genres,"Thriller")==1)%>%
  mutate(Drama=case_when(str_count(genres,"Drama")==1 ~ rating, TRUE ~ 0),
         Comedy = case_when(str_count(genres,"Comedy")==1 ~ rating, TRUE ~ 0),
         Action=case_when(str_count(genres,"Action")==1 ~ rating, TRUE ~ 0),
         Thriller=case_when(str_count(genres,"Thriller")==1 ~ rating, TRUE ~ 0))%>%
  gather(top_genre,genre_rating,c("Drama","Comedy","Action","Thriller"))%>%
  filter(genre_rating!=0)


edx_genre%>%ggplot(aes(top_genre,genre_rating, fill=top_genre))+
  geom_boxplot()+
  facet_wrap(.~userId)+
  geom_boxplot(alpha=0.05)+
  theme(legend.position = "none",axis.text.x = element_text(angle = 75, hjust = 1))+
  ggtitle("Genre Ratings per User ID")+
  ylab("Movie Ratings")+
  xlab("Genre")

#Line graph of average movie rating per movie release year
edx%>%mutate(movie_year = as.integer(movie_year))%>%
  group_by(movie_year)%>%
  dplyr::summarize(avg_rating = mean(rating))%>%
  ggplot(aes(movie_year,avg_rating))+
  geom_line()+ 
  geom_hline(aes(yintercept = mu_rating, col = "red"))+
  geom_text(aes(1950,mu_rating,label = "Overall Average", vjust = 1))+
  ggtitle("Average Movie Rating per Year of Movie Release")+
  xlab("Year Movie was Released")+
  ylab("Average Rating")+
  theme_minimal()+
  theme(legend.position = "none")

#Line graph of number of ratings per year
edx%>%mutate(movie_year = as.integer(movie_year))%>%
  group_by(movie_year)%>%
  dplyr::summarize(n = n())%>%
  ggplot(aes(movie_year,n))+
  geom_line()+  
  ggtitle("Number of Ratings per Year")+
  xlab("Year Movie was Released")+
  ylab("Number of Ratings")+
  theme_minimal()

#See which movie release year had the most reviews
edx%>%mutate(movie_year = as.integer(movie_year))%>%
  group_by(movie_year)%>%
  dplyr::summarize(n = n())%>%
  filter(n == max(n))

top10_review_1995<-edx%>%
  filter(movie_year ==1995)%>%
  group_by(title)%>%
  dplyr::summarize(n = n())%>%
  arrange(desc(n))%>%
  top_n(10)

top10_review_1995

#boxplot of rating for movie release year 1995
edx%>%filter(title %in% top10_review_1995$title)%>%
  mutate(title=reorder(title,rating, FUN=median))%>%
  ggplot(aes(title,rating, fill = title))+
  geom_boxplot(alpha=0.05)+
  theme_minimal()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 75, hjust = 1))+
  ggtitle("Top 10 Most Reviewed Movies in 1995")+
  xlab("Movie Title")+
  ylab("Movie Rating")

#Plot average rating vs number of ratings
edx%>%group_by(movieId,title)%>%
  dplyr::summarize(n=n(),
                   avg_rating = mean(rating))%>%
  arrange(desc(n))%>%
  ggplot(aes(n,avg_rating, color = avg_rating))+
  geom_point()+
  geom_smooth(color = "black", alpha = 0.5, method = "lm")+  
  ggtitle("Average Rating and Total Reviews")+
  xlab("Number of Reviews")+
  ylab("Average Rating")+
  theme(legend.position = "none")

#Plot userId vs Rating
edx%>%group_by(userId)%>%
  dplyr::summarize(avg_rating = mean(rating))%>%
  ggplot(aes(avg_rating))+
  geom_histogram(bins=10)+
  ggtitle("Average Rating Given by User")+
  xlab("Average Rating")+
  ylab("Number of Users")

#Plot movieId vs Rating
edx%>%group_by(movieId)%>%
  dplyr::summarize(avg_rating = mean(rating))%>%
  ggplot(aes(avg_rating))+
  geom_histogram(bins=10)+
  ggtitle("Average Rating Given by Movie")+
  xlab("Average Rating")+
  ylab("Number of Movies")

#Plot userId vs number of reviews
edx%>%group_by(userId)%>%
  dplyr::summarize(n = n(),
                   avg_rating = mean(rating))%>%
  ggplot(aes(avg_rating, n))+
  geom_point()+
  ggtitle("Average Rating and Total Reviews by User")+
  xlab("Average Rating")+
  ylab("Number of Reviews")+
  theme(legend.position = "none")

#Line graph of avg_ratings per number of years since movie was released
edx%>%group_by(n_years_release_review)%>%
  dplyr::summarize(avg_rating = mean(rating))%>%
  ggplot(aes(n_years_release_review,avg_rating))+
  geom_line()+  
  ggtitle("Average Movie Ratings vs. Number of Years Since Movie was Released")+
  xlab("Years since Movie was Released")+
  ylab("Average Rating")+
  theme_minimal()

#Line graph of number of ratings per number of years since movie was released
edx%>%group_by(n_years_release_review)%>%
  dplyr::summarize(n = n())%>%
  ggplot(aes(n_years_release_review,n))+
  geom_line()+  
  ggtitle("Number of Movie Ratings vs. Number of Years Since Movie was Released")+
  xlab("Years since Movie was Released")+
  ylab("Number of ratings")+
  theme_minimal()

#Plot movie ratings overtime for the top 10 movies with the most reviews
top_10_movies<-edx%>%group_by(title)%>%
  dplyr::summarize(n = n())%>%
  arrange(desc(n))%>%
  top_n(10)

movies_year<-edx%>%filter(title %in% top_10_movies$title)%>%
  mutate(year = year(review_date))%>%
  group_by(year, title)%>%
  dplyr::summarize(avg_rating = mean(rating))

movies_year%>%
  ggplot(aes(year,avg_rating,group = title, color = title, label = title))+
  geom_line()+
  ggtitle("Movie Ratings Over Time")+
  xlab("Year")+
  ylab("Rating")+
  theme_minimal()+
  scale_color_viridis(discrete = TRUE, option = "D")

#Add genre rating for top 4 genres to the edx dataframe
edx<-edx%>%filter(str_count(genres,"Drama")==1 |
                    str_count(genres,"Comedy")==1 |
                    str_count(genres,"Action")==1 |
                    str_count(genres,"Thriller")==1)%>%
  mutate(Drama=case_when(str_count(genres,"Drama")==1 ~ rating, TRUE ~ 0),
         Comedy = case_when(str_count(genres,"Comedy")==1 ~ rating, TRUE ~ 0),
         Action=case_when(str_count(genres,"Action")==1 ~ rating, TRUE ~ 0),
         Thriller=case_when(str_count(genres,"Thriller")==1 ~ rating, TRUE ~ 0))

#RMSE calculation to evaluate predictions
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Establish the baseline bias predictors for the rating. For example, the mean rating for all movies, etc.
#Baseline 1: the average rating for all movies
mu_rating<-mean(edx$rating) 

#Basline 2: the movie bias. Calculate difference between a specific movie's rating and the average rating
movie_avgs <- edx %>%  # calulate the movie bias
  group_by(movieId) %>% 
  dplyr::summarize(movie_bias = mean(rating - mu_rating))

#Baseline 3: the user bias: calculate the difference between a specific user's average rating and the overall average rating
user_avgs <- edx %>%  # calulate the user bias
  group_by(userId) %>% 
  dplyr::summarize(user_bias = mean(rating - mu_rating))


#Baseline 4: Number of Reviews bias: The number of reviews effects the average rating
n_ratings<-edx%>%
  group_by(movieId)%>%
  dplyr::summarize(n=n(),
                   avg_rating = mean(rating))

fit_lm_n_rating<-lm(avg_rating ~ n,data=n_ratings)
slope_n<-summary(fit_lm_n_rating)$coef[2,1]
intercept_n<-summary(fit_lm_n_rating)$coef[1,1]

n_ratings<-n_ratings%>%
  dplyr::mutate(y_hat_nratings = intercept_n+(n*slope_n),
                n_ratings_bias = mean(y_hat_nratings-mu_rating))

#Baseline 5: The year the movie was released effects the average rating
movie_year_avgs <- edx %>%
  group_by(movie_year) %>% 
  dplyr::summarize(movie_year_bias = mean(rating - mu_rating))
movie_year_avgs

#Baseline 6: User-Genre Bias
drama_avgs <-edx%>%
  group_by(userId)%>%
  filter(Drama!=0)%>%
  dplyr::summarize(drama_bias = mean(Drama-mu_rating))

comedy_avgs <-edx%>%
  group_by(userId)%>%
  filter(Comedy!=0)%>%
  dplyr::summarize(comedy_bias = mean(Comedy-mu_rating))

action_avgs <-edx%>%
  group_by(userId)%>%
  filter(Action!=0)%>%
  dplyr::summarize(action_bias = mean(Action-mu_rating))

thriller_avgs <-edx%>%
  group_by(userId)%>%
  filter(Thriller!=0)%>%
  dplyr::summarize(thriller_bias = mean(Thriller-mu_rating),)

genre_avgs<-edx%>%
  left_join(drama_avgs, by = 'userId')%>%
  left_join(comedy_avgs, by = 'userId')%>%
  left_join(action_avgs, by = 'userId')%>%
  left_join(thriller_avgs, by = 'userId')%>%
  mutate(Drama = case_when(Drama == 0 ~ 0, TRUE ~ 1),
         Comedy = case_when(Comedy == 0 ~ 0, TRUE ~ 1),
         Action = case_when(Action == 0 ~ 0, TRUE ~ 1),
         Thriller = case_when(Thriller == 0 ~ 0, TRUE ~ 1),
         drama_bias = Drama*drama_bias,
         comedy_bias = Comedy*comedy_bias,
         action_bias = Action*action_bias,
         thriller_bias = Thriller*thriller_bias,
         genre_bias = (drama_bias + comedy_bias + action_bias + thriller_bias)/(Drama+Comedy+Action+Thriller))%>%
  select(userId, movieId, drama_bias, comedy_bias,action_bias,thriller_bias, genre_bias)


#Determine the predicted rating with each bias being added to the previous bias
predicted_ratings <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(movie_year_avgs, by='movie_year')%>%
  left_join(n_ratings, by='movieId') %>%
  left_join(genre_avgs, by = c('userId','movieId')) %>%
  select(rating, movieId, userId, movie_bias, user_bias, movie_year_bias, n_ratings_bias,
         genre_bias, drama_bias, comedy_bias,action_bias,thriller_bias, y_hat_nratings)%>%
  mutate(yhat_mu_rating = mu_rating,
         yhat_movie_bias = movie_bias+mu_rating,
         yhat_user_bias = user_bias+mu_rating,
         yhat_movie_year_bias = movie_year_bias + mu_rating,
         yhat_genre_bias = genre_bias + mu_rating,
         yhat = genre_bias + movie_year_bias+n_ratings_bias+user_bias+movie_bias+mu_rating,
         yhat_limits = case_when(yhat>=5 ~ 5, yhat<=0 ~ 0, TRUE ~yhat),
         yhat_gmu_bias = genre_bias+user_bias*0.25+movie_bias+mu_rating)

head(predicted_ratings)
edx<-edx[complete.cases(predicted_ratings), ]
predicted_ratings<-predicted_ratings[complete.cases(predicted_ratings), ]


#RMSE calculations for each yhat column
base_rmse <- RMSE(edx$rating,mu_rating) 
movie_bias_rmse <- RMSE(edx$rating, predicted_ratings$yhat_movie_bias) 
user_bias_rmse <- RMSE(edx$rating, predicted_ratings$yhat_user_bias) 
nratings_bias_rmse <- RMSE(edx$rating, predicted_ratings$y_hat_nratings) 
movie_year_bias_rmse <- RMSE(edx$rating, predicted_ratings$yhat_movie_year_bias)
genre_bias_rmse <-RMSE(edx$rating, predicted_ratings$yhat_genre_bias)
cumulative_bias_rmse <- RMSE(edx$rating, predicted_ratings$yhat_limits) 
gmu_bias_rmse<-RMSE(edx$rating, predicted_ratings$yhat_gmu_bias)

#store the RMSE results in a data frame
rmse_results <- bind_rows(data_frame(method = "Average Rating", RMSE = base_rmse), 
                          data_frame(method="Movie Bias Model", RMSE = movie_bias_rmse ),
                          data_frame(method="User Bias Model",RMSE = user_bias_rmse ),
                          data_frame(method="Number of Ratings Bias Model",RMSE = nratings_bias_rmse ),
                          data_frame(method="Movie Year Bias",RMSE = movie_year_bias_rmse ),
                          data_frame(method = "User Genre Bias", RMSE = genre_bias_rmse),
                          data_frame(method="Cumulative Bias Model",RMSE = cumulative_bias_rmse ),
                          data_frame(method="Movie, User, and Genre Bias Model",RMSE = gmu_bias_rmse ))
rmse_results
#Create a new dataframe containing only the movieId, userId, rating, year values and bias values
head(predicted_ratings)
edx_bias<-predicted_ratings%>%
  select('rating','movie_bias','user_bias','n_ratings_bias', 'movie_year_bias','drama_bias','comedy_bias',
         'thriller_bias','action_bias','genre_bias')

edx_bias<-edx_bias[complete.cases(edx_bias), ] #remove NaNs
dim(edx_bias)

#create a random forest model on a random sample of the training data to do a quick check of processing time, evaluate variable importance, and to select Ntrees
set.seed(1) 
rf_index <- createDataPartition(y = edx_bias$rating, times = 1, p = 0.002, list = FALSE)
x<-as.matrix(edx_bias[rf_index,2:10])
y<-as.matrix(edx_bias[rf_index,1])
fit_rf<-randomForest(x,as.vector(y), ntree = 300) 
varImp(fit_rf)

#plot the random forest fit to see where the trees converge
plot(fit_rf, main = "Random Forest Model")

#remove n_ratings_bias to improve processing time
head(edx_bias)
edx_bias<-edx_bias[,-4]
#fit a random forest model on a random sample using 10% of the training data because of RAM/memory issues. Number of trees = 150
set.seed(1) 
rf_index <- createDataPartition(y = edx_bias$rating, times = 1, p = 0.1, list = FALSE)
x<-as.matrix(edx_bias[rf_index,2:9])
y<-as.matrix(edx_bias[rf_index,1])
fit_rf<-randomForest(x,as.vector(y), ntree= 50)
varImp(fit_rf, scale = TRUE)
fit_rf
plot(fit_rf)

#Convert test data set into the same format as edx_bias and then use for calculating the final predictions and RMSE
test<-test%>%filter(str_count(genres,"Drama")==1 |
                      str_count(genres,"Comedy")==1 |
                      str_count(genres,"Action")==1 |
                      str_count(genres,"Thriller")==1)%>%
  mutate(Drama=case_when(str_count(genres,"Drama")==1 ~ rating, TRUE ~ 0),
         Comedy = case_when(str_count(genres,"Comedy")==1 ~ rating, TRUE ~ 0),
         Action=case_when(str_count(genres,"Action")==1 ~ rating, TRUE ~ 0),
         Thriller=case_when(str_count(genres,"Thriller")==1 ~ rating, TRUE ~ 0),
         movie_year = as.integer(str_extract(str_sub(title, -5),"\\d{4}")))

test_movieId<-test%>%
  group_by(movieId) %>% 
  dplyr::summarize(movie_bias = mean(rating - mu_rating))

test_userId <- test %>%
  group_by(userId) %>% 
  dplyr::summarize(user_bias = mean(rating - mu_rating))

movie_year_avgs <- test %>%
  group_by(movie_year) %>% 
  dplyr::summarize(movie_year_bias = mean(rating - mu_rating))

drama_avgs <-test%>%
  group_by(userId)%>%
  filter(Drama!=0)%>%
  dplyr::summarize(drama_bias = mean(Drama-mu_rating))

comedy_avgs <-test%>%
  group_by(userId)%>%
  filter(Comedy!=0)%>%
  dplyr::summarize(comedy_bias = mean(Comedy-mu_rating))

action_avgs <-test%>%
  group_by(userId)%>%
  filter(Action!=0)%>%
  dplyr::summarize(action_bias = mean(Action-mu_rating))

thriller_avgs <-test%>%
  group_by(userId)%>%
  filter(Thriller!=0)%>%
  dplyr::summarize(thriller_bias = mean(Thriller-mu_rating))

test_genre<-test%>%
  left_join(drama_avgs, by = 'userId')%>%
  left_join(comedy_avgs, by = 'userId')%>%
  left_join(action_avgs, by = 'userId')%>%
  left_join(thriller_avgs, by = 'userId')%>%
  mutate(Drama = case_when(Drama == 0 ~ 0, TRUE ~ 1),
         Comedy = case_when(Comedy == 0 ~ 0, TRUE ~ 1),
         Action = case_when(Action == 0 ~ 0, TRUE ~ 1),
         Thriller = case_when(Thriller == 0 ~ 0, TRUE ~ 1),
         drama_bias = Drama*drama_bias,
         comedy_bias = Comedy*comedy_bias,
         action_bias = Action*action_bias,
         thriller_bias = Thriller*thriller_bias,
         genre_bias = (drama_bias + comedy_bias + action_bias + thriller_bias)/(Drama+Comedy+Action+Thriller))%>%
  select(userId, movieId, drama_bias, comedy_bias,action_bias,thriller_bias, genre_bias)


test_bias <- test %>% 
  left_join(test_movieId, by='movieId') %>%
  select(rating, movieId, userId, movie_bias, movie_year)%>%
  left_join(test_userId, by='userId') %>%
  select(rating, movieId, userId, movie_bias, user_bias, movie_year)%>%
  left_join(movie_year_avgs, by = 'movie_year')%>%
  left_join(test_genre, by=c('userId','movieId'))%>%
  select(rating, movie_bias, user_bias, movie_year_bias, drama_bias, comedy_bias,
         action_bias,thriller_bias,genre_bias)

test_bias<-test_bias[complete.cases(test_bias), ] #remove NaNs
head(test_bias)

#Create predictions and evaluate on the accuracy and RMSE
x<-as.matrix(test_bias[,2:9])
y<-as.matrix(test_bias[,1])
y_hat <- predict(fit_rf, x, type = "class")
y_hat_round <- round_any(y_hat,0.5) 
rf_test_rmse <- RMSE(as.numeric(y_hat),as.numeric(y))
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "RF model Test Data", RMSE = rf_test_rmse))

rf_test_rmse

#view y_hat_round distribution.
as.data.frame(y_hat_round)%>%
  ggplot(aes(y_hat_round))+
  geom_histogram()+
  ggtitle("Distribution of Predicted Ratings in Test Data")+
  xlab("Predicted Rating")


#Convert validation data set into the same format as edx_bias and then use for calculating the final predictions and RMSE
validation<-validation%>%filter(str_count(genres,"Drama")==1 |
                                  str_count(genres,"Comedy")==1 |
                                  str_count(genres,"Action")==1 |
                                  str_count(genres,"Thriller")==1)%>%
  mutate(Drama=case_when(str_count(genres,"Drama")==1 ~ rating, TRUE ~ 0),
         Comedy = case_when(str_count(genres,"Comedy")==1 ~ rating, TRUE ~ 0),
         Action=case_when(str_count(genres,"Action")==1 ~ rating, TRUE ~ 0),
         Thriller=case_when(str_count(genres,"Thriller")==1 ~ rating, TRUE ~ 0))

val_movieId<-validation%>%
  group_by(movieId) %>% 
  dplyr::summarize(movie_bias = mean(rating - mu_rating))

val_userId <- validation %>%
  group_by(userId) %>% 
  dplyr::summarize(user_bias = mean(rating - mu_rating))

movie_year_avgs <- validation %>%
  group_by(movie_year) %>% 
  dplyr::summarize(movie_year_bias = mean(rating - mu_rating))

drama_avgs <-validation%>%
  group_by(userId)%>%
  filter(Drama!=0)%>%
  dplyr::summarize(drama_bias = mean(Drama-mu_rating))

comedy_avgs <-validation%>%
  group_by(userId)%>%
  filter(Comedy!=0)%>%
  dplyr::summarize(comedy_bias = mean(Comedy-mu_rating))

action_avgs <-validation%>%
  group_by(userId)%>%
  filter(Action!=0)%>%
  dplyr::summarize(action_bias = mean(Action-mu_rating))

thriller_avgs <-validation%>%
  group_by(userId)%>%
  filter(Thriller!=0)%>%
  dplyr::summarize(thriller_bias = mean(Thriller-mu_rating))

val_genre<-validation%>%
  left_join(drama_avgs, by = 'userId')%>%
  left_join(comedy_avgs, by = 'userId')%>%
  left_join(action_avgs, by = 'userId')%>%
  left_join(thriller_avgs, by = 'userId')%>%
  mutate(Drama = case_when(Drama == 0 ~ 0, TRUE ~ 1),
         Comedy = case_when(Comedy == 0 ~ 0, TRUE ~ 1),
         Action = case_when(Action == 0 ~ 0, TRUE ~ 1),
         Thriller = case_when(Thriller == 0 ~ 0, TRUE ~ 1),
         drama_bias = Drama*drama_bias,
         comedy_bias = Comedy*comedy_bias,
         action_bias = Action*action_bias,
         thriller_bias = Thriller*thriller_bias,
         genre_bias = (drama_bias + comedy_bias + action_bias + thriller_bias)/(Drama+Comedy+Action+Thriller))%>%
  select(userId, movieId, drama_bias, comedy_bias,action_bias,thriller_bias, genre_bias)

val_bias <- validation %>% 
  left_join(val_movieId, by='movieId') %>%
  select(rating, movieId, userId, movie_bias, movie_year)%>%
  left_join(val_userId, by='userId') %>%
  select(rating, movieId, userId, movie_bias, user_bias, movie_year)%>%
  left_join(movie_year_avgs, by = 'movie_year')%>%
  left_join(val_genre, by=c('userId','movieId'))%>%
  select(rating, movie_bias, user_bias, movie_year_bias, drama_bias, comedy_bias,
         action_bias,thriller_bias,genre_bias)

val_bias<-val_bias[complete.cases(val_bias), ] #remove NaNs
head(val_bias)

#Create predictions and evaluate on the accuracy and RMSE
x<-as.matrix(val_bias[,2:9])
y<-as.matrix(val_bias[,1])
y_hat <- predict(fit_rf, x, type = "class") 
rf_val_rmse <- RMSE(as.numeric(y),as.numeric(y_hat))
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "RF model Validation Data", RMSE = rf_val_rmse))
rmse_results

#plot y vs yhat
y_yhat<-as.data.frame(y)%>%
  mutate(y_hat=y_hat)

y_yhat[1:200,]%>%
  ggplot(aes(y[1:200],y_hat[1:200]))+
  geom_point()+
  xlab("Actual Rating")+
  ylab("Predicted Rating")+
  ggtitle("Predicted vs Actual Ratings")

