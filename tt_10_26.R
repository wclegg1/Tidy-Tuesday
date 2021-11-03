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

finalData <- ultra_rankings %>% 
  drop_na(age, gender, time_in_seconds) %>% 
  left_join(race, by = c("race_year_id")) %>% 
  drop_na(elevation_gain, elevation_loss, distance) %>% 
  mutate(time_in_minutes = time_in_seconds / 60,
         time_in_hours = time_in_minutes / 60,
         netElevation = elevation_gain - abs(elevation_loss),
         elevationRatio = elevation_gain / elevation_loss) %>% 
  group_by(nationality) %>% mutate(nationN = n()) %>% ungroup %>% 
  mutate(nationality_Abb = if_else(nationN > 1000, 
                                   nationality, "Other")) %>% 
  filter(distance > 0)

# EDA ---------------------------------------------------------------------
racePlot <- ggplot(finalData,
       aes(x = date, y = distance, col = elevation_gain)) + 
  geom_jitter() + 
  labs(x = "Date", y = "Distance", title = "Races") + 
  scale_color_continuous(type = "viridis") + 
  theme_bw()

race2Plot <- ggplot(finalData,
                    aes(y = distance, 
                        x = aid_stations, 
                        col = elevation_gain)) + 
  geom_jitter(size = 0.3) + 
  labs(x = "", y = "Distance", title = "Races") + 
  scale_color_continuous(type = "viridis") + 
  theme_bw()

nationalityPlot <- ggplot(finalData,
       aes(x = time_in_seconds / (60 * 60),
           y = distance * elevation_gain,
           col = gender)) + 
  geom_point(size = 0.3) + facet_wrap(~nationality_Abb) + 
  theme_bw() + scale_color_brewer(palette = "Set1", name = "Gender") +
  labs(x = "Time in Hours", y = "Distance x Elevation")

run_split <- initial_split(finalData, prop = 0.95)
train_data <- training(run_split)
test_data <- testing(run_split)


# Table 1 -----------------------------------------------------------------
finalData %>% group_by(nationality_Abb) %>% tally %>% 
  arrange(desc(n))

table1 <- finalData %>% 
  mutate(year = str_sub(date, 1, 4)) %>% 
  group_by(year) %>% 
  summarise(N = n(), nRunners = length(unique(runner)),
            nRaces = length(unique(race)),
            nRaceCities = length(unique(city)),
            nRaceCountries = length(unique(country)),
            nParticipants = paste0(median(participants),
                                   " (", quantile(participants, 
                                                 probs = 0.25),
                                   ", ", quantile(participants,
                                                  probs = 0.75), 
                                   ")"),
            nAidStations = paste0(median(aid_stations),
                                   " (", quantile(aid_stations, 
                                                 probs = 0.25),
                                   ", ", quantile(aid_stations,
                                                  probs = 0.75), 
                                   ")"),
            nMales = paste0(sum(gender == "M"), " (",
                            round(100 * mean(gender == "M"), 
                                  digits = 1), ")"),
            nFemales = paste0(sum(gender == "W"), " (",
                           round(100 * mean(gender == "W"),
                                 digits = 1), ")"),
            age = paste0(median(age), " (",
                         paste(quantile(age, 
                                        probs = c(0.25, 0.75)),
                               collapse = ", "), ")"))


# Fit Models --------------------------------------------------------------
basePlot <- ggplot(train_data, 
                   aes(y = time_in_minutes / 60, x = distance, 
                       col = elevation_gain)) +
  geom_point(size = 0.5) + facet_wrap(~gender) + 
  scale_color_continuous(type = "viridis")

cor(train_data[, c("age", "elevation_gain", 
                   "distance", "age", "aid_stations")]) 
# Collinearity may be a problem between elevation & distance
colPlot <- ggplot(finalData, aes(x = distance, y = elevation_gain)) + 
  geom_point() + theme_bw() + 
  labs(x = "Distance", y = "Elevation Gain", 
       title = "Collinearity Plot")

fit1 <- lm(time_in_minutes ~ aid_stations + #rank + 
               age + 
               gender + elevation_gain*distance ,
             data = train_data)
newData <- 
  as.data.frame(expand_grid(elevation_gain = c(0, 5000, 10000),
            distance = seq(0, 200, length.out = 100),
            aid_stations = c(13),
            gender = c("M", "W"),
            age = mean(train_data$age))) %>% 
  mutate(group = paste0("Sex: ", gender, " Age:", age, 
                        "Net Elevation: ", elevation_gain,
                        "Aid Stations: ", aid_stations))
predObject <- predict(fit1, newdata = newData, se = TRUE)
newData$time_in_minutes <- predObject$fit
newData$upper <- predObject$fit + 2 * predObject$se.fit
newData$low <- predObject$fit - 2 * predObject$se.fit


fit1Plot <- basePlot + 
  geom_ribbon(data = newData, 
              aes(grouping = group, ymin = low / 60, 
                  ymax = upper / 60,
                  fill = elevation_gain),
              alpha = 0.3) +
  geom_line(data = newData %>% arrange(group, distance),
            aes(grouping = group)) + 
  geom_line(data = newData %>% arrange(group, distance),
            aes(grouping = group, y = upper / 60), lty = 2,
            col = "white") + 
  geom_line(data = newData %>% arrange(group, distance),
            aes(grouping = group, y = low / 60), lty = 2,
            col  = "white") +
  theme_bw() + 
  labs(x = "Distance in km", y = "Time in Hours", 
       title = "Best Fit for Ultra Runner Data")


summary(fit1)
predictedErrors <- predict(fit1, new_data = test_data) - 
  test_data$time_in_minutes
RPMSE <- sqrt(mean(predictedErrors^2))

train_data$yhat <- predict(fit1)
train_data$resid <- residuals(fit1)
train_data <- train_data %>% 
  mutate(oddy = if_else(nationality %in% c("CHI", "ARG"),
                        nationality, "Other"))
pvrPlot <- ggplot(train_data, aes(x = yhat, y = resid)) + 
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  geom_point() + facet_wrap(~nationality_Abb) + 
  theme_bw()

# Test Hypotheses ---------------------------------------------------------
# Test at 5k, Marathon, and 195 Length
testData <- 
  as.data.frame(expand_grid(elevation_gain = c(0, 5000, 10000),
                            distance = c(3, 26.2, 195),
                            aid_stations = c(13),
                            gender = c("M", "W"),
                            age = mean(train_data$age))) %>% 
  mutate(group = paste0("Sex: ", gender, " Age:", age, 
                        "Net Elevation: ", elevation_gain,
                        "Aid Stations: ", aid_stations))
testData$yhat <- predict(fit1, newdata = testData)
testData$lower <- predict(fit1, interval = "confidence",
                          newdata = testData)[, 2]
testData$upper <- predict(fit1, interval = "confidence",
                          newdata = testData)[, 3]
testData %>% select(-group, -age, -aid_stations) %>% 
  pivot_wider(names_from = gender, values_from = lower:yhat) %>% 
  mutate(percDiff = (yhat_W - yhat_M) / yhat_M) %>% as.data.frame
# Test # of Runners
# Test Multiple Race participation
multiData <- finalData %>% 
  mutate(Year = as.numeric(str_sub(date, 1, 4))) 
multiData %>% 
  group_by(Year, runner) %>% 
  summarise(N = n()) %>% 
  group_by(Year) %>% 
  summarise(N2 = n(), pMultiple = mean(N > 1))

multiYearData <- data.frame(Year = unique(multiData$Year), 
           pMulti = sapply(unique(multiData$Year), 
                           function(i) {
  multiData %>% filter(Year <= i) %>% 
    group_by(runner) %>% summarise(N = n()) %>% 
    summarise(pMultiple = mean(N > 1)) %>% unlist
}))

multiYearData %>% 
  arrange(Year)
