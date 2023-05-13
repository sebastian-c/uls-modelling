#####################################################################
###dioxyde_azote study###

library(dplyr)
library(ggplot2)
library(tidyr)

air_data <- read.csv("data/annecy_airquality.csv")


# Convert date column to numeric format and remove hyphens
air_data$date_num <- as.numeric(gsub("-", "", air_data$date))
air_data$date <- as.Date(air_data$date)

# Encode station
# Create a new column "station_encoded" with values 1 for "FR33203" and 2 for "FR20062"
air_data$station_encoded <- ifelse(air_data$station == "FR33203", 1, ifelse(air_data$station == "FR20062", 2, NA))

# Select the dioxyde_azote
dioxyde_azote <- air_data[, c("date_num", "station_encoded", "dioxyde_azote")]

# Summary statistics
summary(dioxyde_azote$dioxyde_azote)

library(urca)

# Run ADF test on dioxyde_azote time series
adf_test <- ur.df(dioxyde_azote$dioxyde_azote, type = "drift", selectlags = "AIC")

# Print ADF test results
summary(adf_test)
#################################
library(stats)

# Fit a stationary ARMA(1,1) model

air_data %>% 
  filter(city == "Annecy") %>%
  arrange(date) %>%
  select(dioxyde_azote) %>%
  ts(start = c(2015, 1), end = c(2022, 12), frequency = 12) -> date

model <- arima(date, order = c(1,0,1), include.mean = TRUE, method = "CSS")
#summary(model)

library(forecast)
auto_model <- auto.arima(date)
auto_model

########################################################################################

plot(decompose(date))









