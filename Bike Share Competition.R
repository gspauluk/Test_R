#Code for analyzing Bike Sharing System

#Libraries I Need
library(tidyverse)
library(DataExplorer)
library(caret)
library(lubridate)

#read in data
bike.train <- read.csv("C:\\Users\\gspsk\\Downloads\\train.csv")
bike.test <- read.csv("C:\\Users\\gspsk\\Downloads\\test.csv")
bike <- bind_rows(train = bike.train, test = bike.test, .id = "id")

##Drop casual and registered
bike <- bike %>% select(-casual, -registered)

##Feature Engineering (what features do I really want to use)
bike$month <- month(bike$datetime) %>% as.factor()
bike$season <- as.factor(bike$season)
bike$holiday <- as.factor(bike$holiday)
bike$hour <- hour(bike$datetime) %>% as.factor()

#Exploratory Plots
qplot(1:nrow(bike.train), bike.train$count, geom = "point")

ggplot(data = bike.train, aes(x = datetime, y = count, color = as.factor(month(datetime)))) +
  geom_point()

plot_missing(bike)

plot_correlation(bike, type = "continuous", 
                 cor_args = list(use = 'pairwise.complete.obs'))

ggplot(data = bike, aes(x = hour, y = count)) +
  geom_boxplot()

ggplot(data = bike, aes(x = season, y = count)) +
  geom_boxplot()


##Dummy variable encoding - one-hot encoding
dummyVars(count ~ season, data = bike, sep = "_") %>% 
  predict(bike) %>% as.data.frame() %>% 
  bind_cols(bike %>% select(-season), .)

## Target encoding
bike$season <- lm(count ~ season, data = bike) %>% 
  predict(., newdata = bike %>% select(-count))



## Fit some models

bike.model <- train(form = count ~ season + holiday + atemp + weather + hour,
                    data = bike %>% filter(id == 'train'),
                    method = "ranger",
                    tuneLength = 8, 
                    trControl = trainControl(
                      method = "repeatedcv", 
                      number = 10,
                      repeats = 2))

#plotting model
plot(bike.model)

#creating submission
preds <- predict(bike.model, newdata = bike %>% filter(id == "test"))
submission <- data.frame(datetime = bike %>% filter(id == "test") %>% pull(datetime),
                         count = preds)
write.csv(x = submission, file = "./MyFirstSubmission.csv", row.names = FALSE)

#--------------------------------------------------------------------------------------

#model 2
bike2.model <- train(form = count ~ season + holiday + atemp + weather + hour,
                    data = bike %>% filter(id == 'train'),
                    method = "ranger",
                    tuneLength = 11, 
                    trControl = trainControl(
                      method = "repeatedcv", 
                      number = 6,
                      repeats = 2))

#plotting model
plot(bike2.model)

#creating submission
preds2 <- predict(bike2.model, newdata = bike %>% filter(id == "test"))
submission2 <- data.frame(datetime = bike %>% filter(id == "test") %>% pull(datetime),
                         count = preds2)
write.csv(x = submission2, file = "./MySecondSubmission.csv", row.names = FALSE)

#------------------------------------------------------------------------------------

#model 3

#feature engineering, changing bike count to log count
bike$count <- log1p(bike$count)

bike3.model <- train(form = count ~ season + holiday + atemp + weather + hour,
                     data = bike %>% filter(id == 'train'),
                     method = "ranger",
                     tuneLength = 6, 
                     trControl = trainControl(
                       method = "repeatedcv", 
                       number = 8,
                       repeats = 2))

#plotting model
plot(bike3.model)

#creating submission
preds3 <- expm1(predict(bike3.model, newdata = bike %>% filter(id == "test")))

submission3 <- data.frame(datetime = bike %>% filter(id == "test") %>% pull(datetime),
                          count = preds3)
write.csv(x = submission3, file = "./MyThirdSubmission.csv", row.names = FALSE)

