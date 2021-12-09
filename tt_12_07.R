# tt_12_07.R
# Author: Wyatt Clegg, December 2021


# Define Libraries & Functions --------------------------------------------
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Read In Data ------------------------------------------------------------
spiders <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')
spidersClean <- spiders %>% 
  mutate(continent = NULL)

# Exploratory Data Analysis -----------------------------------------------
spiders %>% distinct(family, genus) %>% group_by(family) %>% 
  summarise(N = n()) %>% arrange(desc(N))
spiders %>% distinct(genus, species) %>% group_by(genus) %>% 
  summarise(N = n()) %>% arrange(desc(N))
spiders %>% distinct(species, subspecies) %>% group_by(species) %>% 
  summarise(N = n()) %>% arrange(desc(N))
gitcreds::gitcreds_set()
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

sumTable <- spiders %>% group_by(author) %>% 
  summarise(firstYear = min(year, na.rm = TRUE),
            nFamily = length(unique(family)), 
            nGenus = length(unique(genus)),
            nSpecies = length(unique(species)),
            nSubSpecies = length(unique(subspecies))) %>% 
  arrange(desc(nFamily), desc(nGenus), desc(nSpecies), firstYear,
          desc(nSubSpecies))

ggplot(sumTable %>% 
         pivot_longer(nFamily:nSubSpecies, 
                      names_to = "Statistic", 
                      values_to = "Value"),
       aes(x = firstYear, y = Value, col = Statistic)) + 
  geom_point() + facet_wrap(~Statistic, scales = "free_y")

