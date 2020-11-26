library(tidyverse)
library(knitr)
library(kableExtra)

install.packages("pacman")

library(pacman)

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')


data(msleep)

Tabla <- msleep %>% 
  group_by(vore) %>% 
  summarise_at("sleep_total", .funs = list(Mean= mean, SD = sd)) %>%
  dplyr::filter(!is.na(vore)) %>% 
  arrange(desc(Mean))






















