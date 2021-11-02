# tt_10_26.R
# Author: Wyatt Clegg, October 2021
# Tidy Tuesday Analysis, October 26


# Define Libraries & Functions --------------------------------------------
library(tidyverse)
library(tidymodels)
library(xgboost)

# Read In Data ------------------------------------------------------------
ultra_rankings <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv') %>% 
  mutate(nationality_Abb = if_else(nationality %in% c("USA", "FRA", "GBR", "JPN", "ESP", "CHN", "CAN"), nationality,
                                   "Other"))
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')


# EDA ---------------------------------------------------------------------
ggplot(race %>% filter(participation != "solo", distance > 100),
       aes(x = date, y = distance, col = elevation_gain)) + 
  geom_point() 
race %>% filter(participation != "solo", distance < 10) %>% 
  as.data.frame

ggplot(ultra_rankings %>% drop_na(gender),
       aes(x = time_in_seconds, y = rank, col = nationality_Abb)) + 
  geom_point(size = 0.3) + facet_wrap(~gender + nationality_Abb) + theme_bw() +
  scale_color_brewer(palette = "Set1")

finalData <- ultra_rankings %>% 
  drop_na(age, gender, time_in_seconds) %>% 
  left_join(race, by = c("race_year_id")) %>% 
  drop_na(elevation_gain, distance) %>% 
  mutate(time_in_minutes = time_in_seconds / 60,
         time_in_hours = time_in_minutes / 60) %>% 
  filter(distance > 0)

run_split <- initial_split(finalData, prop = 0.95)
train_data <- training(run_split)
test_data <- testing(run_split)


# Fit Models --------------------------------------------------------------
basePlot <- ggplot(train_data, aes(y = time_in_minutes, x = distance, col = elevation_gain)) +
  geom_point() + facet_wrap(~gender)


cor(train_data[, c("age", "elevation_gain", "distance")])
fit1 <- lm(time_in_minutes ~ age + gender*elevation_gain*distance,
           data = train_data)

newData <- as.data.frame(expand_grid(elevation_gain = c(0, 5000, 1000),
            distance = seq(0, 200, length.out = 100),
            gender = c("M", "W"),
            age = c(18, 20, 21, 30, 40, 60, 80))) %>% 
  mutate(group = paste0("Sex: ", gender, " Age:", age, 
                        "Elevation: ", elevation_gain))

newData$time_in_minutes <- predict(fit1, newdata = newData)

basePlot + geom_line(data = newData, mapping = aes(grouping = group))

summary(fit1)
predictedErrors <- predict(fit1, new_data = test_data) - test_data$time_in_seconds
sqrt(mean(predictedErrors^2))

# Get Rid of Age
fit2 <- lm(time_in_minutes ~ gender*elevation_gain*distance,
           data = train_data)

newData <- as.data.frame(expand_grid(elevation_gain = c(0, 5000, 1000),
                                     distance = seq(0, 200, length.out = 100),
                                     gender = c("M", "W"))) %>% 
  mutate(group = paste0("Sex: ", gender, #" Age:", age, 
                        "Elevation: ", elevation_gain))

newData$time_in_minutes <- predict(fit2, newdata = newData)

basePlot + geom_line(data = newData, mapping = aes(grouping = group))

summary(fit2)
predictedErrors <- predict(fit2, newdata = test_data) - test_data$time_in_seconds
sqrt(mean(predictedErrors^2)) # So this model stinks at predicting

# xgBoost -----------------------------------------------------------------

xtrain <- train_data[, c("age", #"gender", "nationality", 
                         "distance",
                         "elevation_gain", "aid_stations", 
                         "participants")]
xtrain[, "gender"] <- as.numeric(train_data$gender == "M")
xtrain <- cbind(xtrain, 
                model.matrix(~-1 + nationality, data = train_data))
xtrain <- data.matrix(xtrain)

m1_xgb <-
  xgboost(
    data = xtrain,
    label = train_data$time_in_minutes,
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .25
  ) 


importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.tree(model = m1_xgb, trees = 0:2)
