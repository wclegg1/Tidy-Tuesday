# tt_10_26.R
# Author: Wyatt Clegg, October 2021
# Tidy Tuesday Analysis, October 26


# Define Libraries & Functions --------------------------------------------
library(tidyverse)

# Read In Data ------------------------------------------------------------
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')


# EDA ---------------------------------------------------------------------
ggplot(race %>% filter(participation != "solo", distance > 100),
       aes(x = date, y = distance, col = elevation_gain)) + 
  geom_point() 
race %>% filter(participation != "solo", distance < 10) %>% 
  as.data.frame

