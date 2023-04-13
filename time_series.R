library(tidyr)
library(dplyr)
library(ggplot2)

air_data <- read.csv("data/annecy_airquality.csv")

air_data %>% 
  filter(city == "Annecy") %>%
  arrange(date) %>%
  select(dioxyde_azote) %>%
  ts(start = c(2015, 1), end = c(2022, 12), frequency = 12) -> x
