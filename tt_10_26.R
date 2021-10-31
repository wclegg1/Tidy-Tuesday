# tt_10_26.R
# Author: Wyatt Clegg, October 2021
# Tidy Tuesday Analysis, October 26


# Define Libraries & Functions --------------------------------------------
library(tidyverse)
library(tidymodels)

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

finalData <- ultra_rankings %>% drop_na(gender) %>% 
  left_join(race, by = c("race_year_id"))


# Fit Models --------------------------------------------------------------

fit1 <- lm(time_in_seconds ~ age + gender + elevation_gain, data = finalData)
