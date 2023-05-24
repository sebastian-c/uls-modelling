library(dplyr)
library(ggplot2)
library(tidyr)

air_data <- read.csv("data/annecy_airquality.csv")

air_data %>%
  pivot_longer(cols = PM10:monoxyde_azote) ->
  plot_data

ggplot(plot_data, aes(x = as.Date(date), y = value)) +
  geom_line() +
  facet_grid(vars(name), vars(city), scales = "free")

### plot each polluant separately

#### Dioxyde_azote

air_data %>%
  pivot_longer(cols = dioxyde_azote) ->
  plot_data

ggplot(plot_data, aes(x = as.Date(date), y = value)) +
  geom_line() +
  labs(x = "Date", y = "Dioxyde_azote value", title = "Dioxyde_azote values over time")

facet_grid(vars(name), vars(city), scales = "free")

#### Monoxyde_azote

air_data %>%
  pivot_longer(cols = monoxyde_azote) ->
  plot_data

ggplot(plot_data, aes(x = as.Date(date), y = value)) +
  geom_line() +
  labs(x = "Date", y = "Monoxyde_azote value", title = "Monoxyde_azote values over time")

facet_grid(vars(name), vars(city), scales = "free")

#### PM10

air_data %>%
  pivot_longer(cols = PM10) ->
  plot_data

ggplot(plot_data, aes(x = as.Date(date), y = value)) +
  geom_line() +
  labs(x = "Date", y = "PM10 value", title = "PM10 values over time")

facet_grid(vars(name), vars(city), scales = "free")
