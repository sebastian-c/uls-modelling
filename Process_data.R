library(tidyverse)

dir.create("data", recursive = TRUE, showWarnings = FALSE)

raw_data <- read_csv("raw_data/annecy_airquality.csv")

# convert millisecond timestamp to date

raw_data %>% 
  mutate(date = as.Date(as.POSIXct(date/1000, origin="1970-01-01"))) ->
  raw_data

raw_data %>%
  group_by(date = floor_date(date, "month"), city, station, pollutant_name) %>%
  summarise(pollutant_level = mean(pollutant_level, na.rm = TRUE)) ->
  agg_data

agg_data %>%
  pivot_wider(id_cols = date:station, names_from = pollutant_name, values_from = pollutant_level) ->
  wide_data


write_csv(wide_data, "data/annecy_airquality.csv")
