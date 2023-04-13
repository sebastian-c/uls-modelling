library(dplyr)
library(ggplot2)

air_data <- read.csv("data/annecy_airquality.csv")

air_data %>%
  pivot_longer(cols = PM10:monoxyde_azote) ->
  plot_data

ggplot(plot_data, aes(x = as.Date(date), y = value)) +
  geom_line() +
  facet_grid(vars(name), vars(city), scales = "free")
