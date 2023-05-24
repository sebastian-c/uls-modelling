################################################################################
####### dioxyde_azote study  #######

############  NO2_Annecy  ###########

library(stats)
# Data selection and transformation

air_data %>% 
  filter(city == "Annecy") %>%
  arrange(date) %>%
  select(dioxyde_azote) %>%
  ts(start = c(2015, 1), end = c(2022, 12), frequency = 12) -> NO2_Annecy

plot(NO2_Annecy)

# Decompose the data

## Checking the trend
library(urca)
### Run ADF test on NO2_Annecy time series
adf_test_1 <- ur.df(NO2_Annecy, type = "drift", selectlags = "AIC")

# Print ADF test results
summary(adf_test_1)

### Run KPSS test on NO2_Annecy time series
kpss_test_1 <- ur.kpss(NO2_Annecy)

# Print the results
print(kpss_test_1)
summary(kpss_test_1)

# Decompose the time series to extract trend, seasonal, and residual components
decomposed <- decompose(NO2_Annecy)

# Extract the trend component
trend <- decomposed$trend

# Detrend the original time series by subtracting the trend component
detrended <- NO2_Annecy - trend

# Plot the detrended series
plot(detrended, main = "Detrended NO2_Annecy")

# Align the time series
common_range <- time(window(NO2_Annecy, start = start(detrended), end = end(detrended)))
NO2_Annecy_aligned <- window(NO2_Annecy, start = start(common_range), end = end(common_range))
detrended_aligned <- window(detrended, start = start(common_range), end = end(common_range))

# Create a data frame with aligned data
common_data <- data.frame(
  Date = as.Date(as.numeric(common_range), origin = "1970-01-01"),
  NO2_Annecy = as.numeric(NO2_Annecy_aligned),
  Detrended = as.numeric(detrended_aligned)
)

# Remove missing values from the data frame
common_data <- na.omit(common_data)

# Plot the detrended series
plot(detrended_aligned, main = "Detrended NO2_Annecy")

# Plot the original and detrended data
ggplot(common_data, aes(Date)) +
  geom_line(aes(y = NO2_Annecy, color = "Original")) +
  geom_line(aes(y = Detrended, color = "Detrended")) +
  labs(title = "Original and Detrended NO2_Annecy") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()

# Remove missing values from detrended data
detrended_clean <- na.omit(detrended)

# Perform Augmented Dickey-Fuller (ADF) test
adf_test_det <- ur.df(detrended_clean, type = "drift")

# Print the ADF test results
summary(adf_test_det)

## Checking the seasonality

# Apply seasonal differencing
seasonal_diff <- diff(NO2_Annecy, lag = 12)

# Plot the seasonally differenced series
plot(seasonal_diff, main = "Seasonally Differenced NO2_Annecy")

######## Calculate rolling mean and rolling std and plot them with data######################

library(zoo)

# Calculate rolling mean with window size 5
rolling_mean <- rollmean(NO2_Annecy, k = 5, fill = NA)

# Calculate rolling standard deviation with window size 5
rolling_std <- rollapply(NO2_Annecy, width = 5, FUN = sd, fill = NA)

# Plot the original series, rolling mean, and rolling standard deviation
plot(NO2_Annecy, type = "l", col = "blue", ylab = "Value", main = "Time Series with Rolling Mean and Rolling Standard Deviation")
lines(rolling_mean, col = "red")
lines(rolling_std, col = "green")

# Add legend
legend("topright", legend = c("Original", "Rolling Mean", "Rolling Std"), col = c("blue", "red", "green"), lty = 1)

#################

# Calculate rolling mean with window size 5
rolling_mean <- rollmean(NO2_Annecy, k = 5, fill = NA)

# Calculate rolling standard deviation with window size 5
rolling_std <- rollapply(NO2_Annecy, width = 5, FUN = sd, fill = NA)

##### plot of diferenced data, rolling mean and rolling std
# Plot the original series, rolling mean, and rolling standard deviation
plot(seasonal_diff, type = "l", col = "blue", ylab = "Value", main = "Time Series with Rolling Mean and Rolling Standard Deviation")
lines(rolling_mean, col = "red")
lines(rolling_std, col = "green")

# Add legend
legend("topright", legend = c("Original", "Rolling Mean", "Rolling Std"), col = c("blue", "red", "green"), lty = 1)

##### plot of trended data, rolling mean and rolling std
# Plot the original series, rolling mean, and rolling standard deviation
plot(detrended_aligned, type = "l", col = "blue", ylab = "Value", main = "Time Series with Rolling Mean and Rolling Standard Deviation")
lines(rolling_mean, col = "red")
lines(rolling_std, col = "green")

# Add legend
legend("topright", legend = c("Original", "Rolling Mean", "Rolling Std"), col = c("blue", "red", "green"), lty = 1)

#################

# Perform ADF test on the seasonally differenced series
adf_test_seas <- ur.df(seasonal_diff, type = "drift")

# Print the ADF test results
summary(adf_test_seas)

# Decompose the data
decompose(NO2_Annecy)$seasonal
decompose(NO2_Annecy)$trend
decompose(NO2_Annecy)$random

# Modeling and Prediction
model<-HoltWinters(NO2_Annecy)
previsions<-predict(model, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

#  Case 1: Beta=False
model1<- HoltWinters(NO2_Annecy, beta = FALSE, gamma = TRUE)
previsions1<-predict(model1, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

#  Case 2: gamma=FALSE 	
model2<- HoltWinters(NO2_Annecy, beta = TRUE, gamma = FALSE)
previsions2<-predict(model2, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

# Case 3: beta=false & gamma=false
model3<- HoltWinters(NO2_Annecy, beta = FALSE, gamma = FALSE)
previsions3<-predict(model3, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

library(forecast)
auto_model <- auto.arima(NO2_Annecy)
auto_model
previsions4 <- forecast(auto_model, h = 12, level = 0.95)

#SARIMA model
sarima_model <- auto.arima(NO2_Annecy, seasonal = TRUE)
previsions5 <- forecast(sarima_model, h=12, level = 0.95)

# Plot previsions
par(mfrow = c(2, 2))  # Create a 2x2 grid of plots


plot(model, previsions, main = "Holt-Winters Model 1")
plot(model1, previsions, main = "Holt-Winters Model 2 (Beta=False)")
plot(model2, previsions, main = "Holt-Winters Model 3 (Gamma=False)")
plot(model3, previsions, main = "Holt-Winters Model 4 (beta=false & gamma=false)")
plot(previsions4, main = "Auto.arima model")
plot(previsions5, main = "Sarima model")
par(mfrow = c(1,1))

## Choice between models

HW_1_SSE <- HoltWinters(NO2_Annecy)$SSE 
HW_2_SSE <- HoltWinters(NO2_Annecy, beta = FALSE, gamma = TRUE)$SSE
HW_3_SSE <- HoltWinters(NO2_Annecy,beta = TRUE, gamma = FALSE)$SSE
HW_4_SSE <- HoltWinters(NO2_Annecy, beta = FALSE, gamma = FALSE)$SSE

valid <- NO2_Annecy

# Generate the forecasts for the validation set
forecast_valid <- forecast(auto_model, h = length(valid))$mean

# Trim the forecasted values to match the length of the validation set
forecast_adjusted <- forecast_valid[1:length(valid)]

# Remove missing values from forecasted values and validation set
forecast_adjusted <- forecast_adjusted[!is.na(forecast_adjusted)]
valid <- valid[!is.na(valid)]

# Calculate the SSE for the auto.arima model
sse_auto <- sum((forecast_adjusted - valid)^2)

# Display the SSE
print(paste("SSE for the auto.arima model:", sse_auto))

# SSE for SARIMA
actual_values <- NO2_Annecy[(length(NO2_Annecy) - 11):length(NO2_Annecy)]
sse_sarima <- sum((previsions5$mean - actual_values)^2)
cat("Sum of Squared Errors (SSE):", sse_sarima, "\n")

########### Residuals##########

# Calculate residuals for Holt-Winters Model 
residuals_hw <- residuals(model)

# Calculate residuals for Holt-Winters Model 1
residuals_hw_1 <- residuals(model1)

# Calculate residuals for Holt-Winters Model 2
residuals_hw_2 <- residuals(model2)

# Calculate residuals for Holt-Winters Model 3
residuals_hw_3 <- residuals(model3)

# Calculate residuals for the auto.arima model
residuals_auto <- forecast_adjusted - valid

# Calculate residuals for the sarima model
residuals_sarima <- residuals(sarima_model)

# Plot residuals
par(mfrow = c(2, 2))  # Create a 2x2 grid of plots

plot(residuals_hw, main = "Residuals - Holt-Winters Model 1")
plot(residuals_hw_1, main = "Residuals - Holt-Winters Model 2")
plot(residuals_hw_2, main = "Residuals - Holt-Winters Model 3")
plot(residuals_hw_3, main = "Residuals - Holt-Winters Model 4")
plot(residuals_auto, main = "Residuals - auto.arima model")
plot(residuals_sarima, main = "Residuals - sarima model")
par(mfrow = c(1, 1))

####### Calculate other metrics########

# Calculate metrics

mae1 <- mean(abs(residuals_hw))
mae2 <- mean(abs(residuals_hw_1))
mae3 <- mean(abs(residuals_hw_2))
mae4 <- mean(abs(residuals_hw_3))
mae_auto <- mean(abs(residuals_auto))
mae_sarima <- mean(abs(residuals_sarima))

#Calculate BIC and AIC parameters

# Calculate SSE for each Holt-Winters model
#HW_1_SSE <- HoltWinters(NO2_Annecy)$SSE
#HW_2_SSE <- HoltWinters(NO2_Annecy, beta = FALSE, gamma = TRUE)$SSE
#HW_3_SSE <- HoltWinters(NO2_Annecy, beta = TRUE, gamma = FALSE)$SSE
#HW_4_SSE <- HoltWinters(NO2_Annecy, beta = FALSE, gamma = FALSE)$SSE

# Calculate the number of parameters for each model
HW_1_params <- 3  # Assuming alpha, beta, and gamma are included
HW_2_params <- 2  # Assuming alpha and gamma are included
HW_3_params <- 2  # Assuming alpha and beta are included
HW_4_params <- 1  # Assuming only alpha is included

# Calculate the number of observations
n <- length(NO2_Annecy)

# Calculate the AIC for each model
HW_1_AIC <- n * log(HW_1_SSE/n) + 2 * HW_1_params
HW_2_AIC <- n * log(HW_2_SSE/n) + 2 * HW_2_params
HW_3_AIC <- n * log(HW_3_SSE/n) + 2 * HW_3_params
HW_4_AIC <- n * log(HW_4_SSE/n) + 2 * HW_4_params
sarima_AIC <- AIC(sarima_model)

# Calculate the BIC for each model
HW_1_BIC <- n * log(HW_1_SSE/n) + log(n) * HW_1_params
HW_2_BIC <- n * log(HW_2_SSE/n) + log(n) * HW_2_params
HW_3_BIC <- n * log(HW_3_SSE/n) + log(n) * HW_3_params
HW_4_BIC <- n * log(HW_4_SSE/n) + log(n) * HW_4_params
sarima_BIC <- BIC(sarima_model)

# Calculate the number of parameters
arima_params <- length(coef(auto_model))

# Calculate the number of observations
n <- length(NO2_Annecy)

# Calculate the AIC
arima_AIC <- n * log(sse_auto/n) + 2 * arima_params

# Calculate the BIC
arima_BIC <- n * log(sse_auto/n) + log(n) * arima_params

# Print the AIC and BIC for the auto.arima model
cat("Auto-arima AIC:", arima_AIC, "BIC:", arima_BIC, "\n")

library(knitr)
# Create a data frame of the model parameters
model_params <- data.frame(Model = c("Holt-Winters NO2_Annecy", "Holt-Winters NO2_Annecy (Beta = FALSE, Gamma = TRUE)", "Holt-Winters NO2_Annecy (Beta = TRUE, Gamma = FALSE)", "Holt-Winters NO2_Annecy (Beta = FALSE, Gamma = FALSE)", "Auto ARIMA NO2_Annecy", "SARIMA NO2_Annecy"),
                           MAE = c(mae1, mae2, mae3, mae4, mae_auto, mae_sarima), 
                           SSE = c(HW_1_SSE, HW_2_SSE, HW_3_SSE, HW_4_SSE, sse_auto, sse_sarima),
                           AIC = c(HW_1_AIC, HW_2_AIC, HW_3_AIC, HW_4_AIC, arima_AIC, sarima_AIC),
                           BIC = c(HW_1_BIC, HW_2_BIC, HW_3_BIC, HW_4_BIC, arima_BIC, sarima_BIC)
)

# Print the table
kable(model_params, align = "c")


########################################################################################
############  NO2_Lyon  ###########

# Data selection and transformation

air_data %>% 
  filter(city == "Lyon") %>%
  arrange(date) %>%
  select(dioxyde_azote) %>%
  ts(start = c(2015, 1), end = c(2022, 12), frequency = 12) -> NO2_Lyon

plot(NO2_Lyon)
plot(decompose(NO2_Lyon))

# Decompose the data

## Checking the trend
library(urca)
### Run ADF test on NO2_Lyon time series
adf_test_2 <- ur.df(NO2_Lyon, type = "drift", selectlags = "AIC")

# Print ADF test results
summary(adf_test_2)

### Run KPSS test on NO2_Lyon time series
kpss_test_2 <- ur.kpss(NO2_Lyon)

# Print the results
print(kpss_test_2)
summary(kpss_test_2)


# Decompose the data
decompose(NO2_Lyon)$seasonal
decompose(NO2_Lyon)$trend
decompose(NO2_Lyon)$random

# Prediction
model_1<-HoltWinters(NO2_Lyon)
previsions_1<-predict(model_1, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

#  Case 1: Beta=False
model_2<- HoltWinters(NO2_Lyon, beta = FALSE, gamma = TRUE)
previsions_2<-predict(model_2, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

#  Case 2: gamma=FALSE 	
model_3<- HoltWinters(NO2_Lyon, beta = TRUE, gamma = FALSE)
previsions_3<-predict(model_3, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

# Case 3: beta=false & gamma=false
model_4<- HoltWinters(NO2_Lyon, beta = FALSE, gamma = FALSE)
previsions_4<-predict(model_4, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

# Auto-arima model
library(forecast)
auto_model_2 <- auto.arima(NO2_Lyon)
previsions_5 <- forecast(auto_model_2, h = 12, level = 0.95)

#SARIMA model
sarima_model_2 <- auto.arima(NO2_Lyon, seasonal = TRUE)
previsions_6 <- forecast(sarima_model_2, h=12, level = 0.95)

# Plot previsions
par(mfrow = c(2, 2))  # Create a 2x2 grid of plots


plot(model_1, previsions_1, main = "Holt-Winters Model 1")
plot(model_2, previsions_2, main = "Holt-Winters Model 2 (Beta=False)")
plot(model_3, previsions_3, main = "Holt-Winters Model 3 (Gamma=False)")
plot(model_4, previsions_4, main = "Holt-Winters Model 4 (beta=false & gamma=false)")
plot(previsions_5, main = "Auto.arima model")
plot(previsions_6, main = "Sarima model")

# Reset the plotting layout
par(mfrow = c(1, 1))

## Choice between models

HW_1_2_SSE<- HoltWinters(NO2_Lyon)$SSE 
HW_2_2_SSE<- HoltWinters(NO2_Lyon, beta = FALSE, gamma = TRUE)$SSE
HW_3_2_SSE<- HoltWinters(NO2_Lyon,beta = TRUE, gamma = FALSE)$SSE
HW_4_2_SSE<- HoltWinters(NO2_Lyon, beta = FALSE, gamma = FALSE)$SSE

valid <- NO2_Lyon
# Generate forecasts using the auto.arima model
forecast_valid <- forecast(auto_model_2, h = length(valid))

# Extract the forecasted values
forecast_values <- forecast_valid$mean

# Trim the forecasted values to match the length of the validation set
forecast_adjusted <- forecast_values[1:length(valid)]

# Remove missing values from forecasted values and validation set
forecast_adjusted <- forecast_adjusted[!is.na(forecast_adjusted)]
valid <- valid[!is.na(valid)]

# Calculate the SSE
sse_auto_2 <- sum((forecast_adjusted - valid)^2, na.rm = TRUE)

# Display the SSE
print(paste("SSE for the auto.arima model:", sse_auto_2))

# SSE for SARIMA
actual_values <- NO2_Lyon[(length(NO2_Lyon) - 11):length(NO2_Lyon)]
sse_sarima_2 <- sum((previsions5$mean - actual_values)^2)
cat("Sum of Squared Errors (SSE):", sse_sarima_2, "\n")


########### Residuals##########

# Calculate residuals for Holt-Winters Model 
residuals_hw_1_2 <- residuals(model_1)

# Calculate residuals for Holt-Winters Model 1
residuals_hw_2_2 <- residuals(model_2)

# Calculate residuals for Holt-Winters Model 2
residuals_hw_3_2 <- residuals(model_3)

# Calculate residuals for Holt-Winters Model 3
residuals_hw_4_2 <- residuals(model_4)

# Calculate residuals for the auto.arima model
residuals_auto_2 <- forecast_adjusted - valid

# Calculate residuals for the sarima model
residuals_sarima_2 <- residuals(sarima_model_2)

# Plot residuals
par(mfrow = c(2, 2))  # Create a 2x2 grid of plots


plot(residuals_hw_1_2, main = "Residuals - Holt-Winters Model 1")
plot(residuals_hw_2_2, main = "Residuals - Holt-Winters Model 2")
plot(residuals_hw_3_2, main = "Residuals - Holt-Winters Model 3")
plot(residuals_hw_4_2, main = "Residuals - Holt-Winters Model 4")
plot(residuals_auto_2, main = "Residuals - auto.arima model")
plot(residuals_sarima_2, main = "Residuals - Sarima model")

# Reset the plotting layout
par(mfrow = c(1, 1))

####### Calculate other metrics########

# Calculate metrics

mae1_2 <- mean(abs(residuals_hw_1_2))
mae2_2 <- mean(abs(residuals_hw_2_2))
mae3_2 <- mean(abs(residuals_hw_3_2))
mae4_2 <- mean(abs(residuals_hw_4_2))
mae_auto_2 <- mean(abs(residuals_auto_2))
mae_sarima_2 <- mean(abs(residuals_sarima_2))

# Calculate the number of parameters for each model
HW_1_2_params <- 3  # Assuming alpha, beta, and gamma are included
HW_2_2_params <- 2  # Assuming alpha and gamma are included
HW_3_2_params <- 2  # Assuming alpha and beta are included
HW_4_2_params <- 1  # Assuming only alpha is included

# Calculate the number of observations
n <- length(NO2_Lyon)

# Calculate the AIC for each model
HW_1_2_AIC <- n * log(HW_1_2_SSE/n) + 2 * HW_1_2_params
HW_2_2_AIC <- n * log(HW_2_2_SSE/n) + 2 * HW_2_2_params
HW_3_2_AIC <- n * log(HW_3_2_SSE/n) + 2 * HW_3_2_params
HW_4_2_AIC <- n * log(HW_4_2_SSE/n) + 2 * HW_4_2_params
sarima_AIC_2 <- AIC(sarima_model_2)

# Calculate the BIC for each model
HW_1_2_BIC <- n * log(HW_1_2_SSE/n) + log(n) * HW_1_2_params
HW_2_2_BIC <- n * log(HW_2_2_SSE/n) + log(n) * HW_2_2_params
HW_3_2_BIC <- n * log(HW_3_2_SSE/n) + log(n) * HW_3_2_params
HW_4_2_BIC <- n * log(HW_4_2_SSE/n) + log(n) * HW_4_2_params
sarima_BIC_2 <- BIC(sarima_model_2)

# Calculate the number of parameters
arima_params_2 <- length(coef(auto_model_2))

# Calculate the number of observations
n <- length(NO2_Lyon)

# Calculate the AIC
arima_AIC_2 <- n * log(sse_auto_2/n) + 2 * arima_params_2

# Calculate the BIC
arima_BIC_2 <- n * log(sse_auto_2/n) + log(n) * arima_params_2

# Print the AIC and BIC for the auto.arima model
cat("Auto-arima AIC:", arima_AIC_2, "BIC:", arima_BIC_2, "\n")

library(knitr)
# Create a data frame of the model parameters
model_params_2 <- data.frame(Model = c("Holt-Winters NO2_Lyon", "Holt-Winters NO2_Lyon (Beta = FALSE, Gamma = TRUE)", "Holt-Winters NO2_Lyon (Beta = TRUE, Gamma = FALSE)", "Holt-Winters NO2_Lyon (Beta = FALSE, Gamma = FALSE)", "Auto ARIMA NO2_Lyon", "SARIMA NO2_Lyon"),
                             MAE = c(mae1_2, mae2_2, mae3_2, mae4_2, mae_auto_2, mae_sarima_2), 
                             SSE = c(HW_1_2_SSE, HW_2_2_SSE, HW_3_2_SSE, HW_4_2_SSE, sse_auto_2, sse_sarima_2),
                             AIC = c(HW_1_2_AIC, HW_2_2_AIC, HW_3_2_AIC, HW_4_2_AIC, arima_AIC_2, sarima_AIC_2),
                             BIC = c(HW_1_2_BIC, HW_2_2_BIC, HW_3_2_BIC, HW_4_2_BIC, arima_BIC_2, sarima_BIC_2)
)

# Print the table
kable(model_params_2, align = "c")

####################################################################################
#######    Monoxyde_azote study    #######

#############   NO_Annecy   ############

# Data selection and transformation

air_data %>% 
  filter(city == "Annecy") %>%
  arrange(date) %>%
  select(monoxyde_azote) %>%
  ts(start = c(2015, 1), end = c(2022, 12), frequency = 12) -> NO_Annecy

plot(NO_Annecy)
plot(decompose(NO_Annecy))

# Decompose the data

## Checking the trend
library(urca)
### Run ADF test on NO_Annecy time series
adf_test_1 <- ur.df(NO_Annecy, type = "drift", selectlags = "AIC")

# Print ADF test results
summary(adf_test_1)

### Run KPSS test on NO_Annecy time series
kpss_test_1 <- ur.kpss(NO_Annecy)

# Print the results
print(kpss_test_1)
summary(kpss_test_1)


## Decompose the data
decompose(NO_Annecy)$seasonal
decompose(NO_Annecy)$trend
decompose(NO_Annecy)$random


# First case: Full model
model_1_A<- HoltWinters(NO_Annecy)
previsions_1_A<-predict(model_1_A, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

# Second case: beta=false
model_2_A<- HoltWinters(NO_Annecy, beta = FALSE, gamma = TRUE)
previsions_2_A<-predict(model_2_A, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

# Third case: gamma=false
model_3_A<- HoltWinters(NO_Annecy, beta = TRUE, gamma = FALSE)
previsions_3_A<-predict(model_3_A, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

# Fourth case: beta=false & gamma=false
model_4_A<- HoltWinters(NO_Annecy, beta = FALSE, gamma = FALSE)
previsions_4_A<-predict(model_4_A, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

#Auto-arima model
library(forecast)
auto_model <- auto.arima(NO_Annecy)
previsions_5_A <- forecast(auto_model, h = 12, level = 0.95)

#SARIMA model
sarima_model <- auto.arima(NO_Annecy, seasonal = TRUE)
previsions_6_A <- forecast(sarima_model, h=12, level = 0.95)

# Plot models with predictions
par(mfrow= c(2,2))
plot(model_1_A, previsions_1_A, main = "Holt-Winters Model 1")
plot(model_2_A, previsions_2_A, main = "Holt-Winters Model 2 (Beta=False)")
plot(model_3_A, previsions_3_A, main = "Holt-Winters Model 3 (Gamma=False)")
plot(model_4_A, previsions_4_A, main = "Holt-Winters Model 4 (beta=false & gamma=false)")
plot(previsions_5_A, main = "Auto.arima model")
plot(previsions_6_A, main = "Sarima model")
par(mfrow = c(1, 1))

# Generate forecast object for diagnostic plots
forecast_object <- forecast(auto_model)

# Generate specific diagnostic plots
par(mfrow = c(2, 2))

plot(forecast_object$residuals)  # Time series plot of residuals

# Histogram of residuals with KDE curve and N(0,1) curve
hist(forecast_object$residuals, main = "Histogram of Residuals",
     xlab = "Residuals", ylab = "Frequency", freq = FALSE)
lines(density(forecast_object$residuals), col = "red", lwd = 2)  # KDE curve
curve(dnorm(x, mean = 0, sd = 1), col = "blue", lwd = 2, add = TRUE)  # N(0,1) curve
legend("topleft", c("KDE Curve", "N(0,1) Curve"), col = c("red", "blue"), lwd = 2)


# Generate linear trend of samples from N(0,1)
set.seed(123)  # Set seed for reproducibility
trend <- seq(-3, 3, length.out = length(forecast_object$residuals))  # Linear trend values

# Plot QQ plot with linear trend
qqnorm(forecast_object$residuals)
points(sort(trend), sort(forecast_object$residuals), col = "green", pch = 20)

# Compute autocorrelation values
acf_values <- acf(forecast_object$residuals)
# Plot correlogram
plot(acf_values, main = "Correlogram")


## Choice between models

HW_1_SSE <- HoltWinters(NO_Annecy)$SSE 
HW_2_SSE <- HoltWinters(NO_Annecy, beta = FALSE, gamma = TRUE)$SSE
HW_3_SSE <- HoltWinters(NO_Annecy,beta = TRUE, gamma = FALSE)$SSE
HW_4_SSE <- HoltWinters(NO_Annecy, beta = FALSE, gamma = FALSE)$SSE

valid <- NO_Annecy
# Generate forecasts using the auto.arima model
forecast_valid <- forecast(auto_model, h = length(valid))

# Extract the forecasted values
forecast_values <- forecast_valid$mean

# Trim the forecasted values to match the length of the validation set
forecast_adjusted <- forecast_values[1:length(valid)]

# Remove missing values from forecasted values and validation set
forecast_adjusted <- forecast_adjusted[!is.na(forecast_adjusted)]
valid <- valid[!is.na(valid)]

# Calculate the SSE
sse_auto <- sum((forecast_adjusted - valid)^2, na.rm = TRUE)

# Display the SSE
print(paste("SSE for the auto.arima model:", sse_auto))

# SSE for SARIMA
actual_values <- NO_Annecy[(length(NO_Annecy) - 11):length(NO_Annecy)]
sse_sarima <- sum((previsions_6_A$mean - actual_values)^2)
cat("Sum of Squared Errors (SSE):", sse_sarima, "\n")


########### Residuals##########

# Calculate residuals for Holt-Winters Model 
residuals_hw <- residuals(model_1_A)

# Calculate residuals for Holt-Winters Model 1
residuals_hw_1 <- residuals(model_2_A)

# Calculate residuals for Holt-Winters Model 2
residuals_hw_2 <- residuals(model_3_A)

# Calculate residuals for Holt-Winters Model 3
residuals_hw_3 <- residuals(model_4_A)

# Calculate residuals for the auto.arima model
residuals_auto <- forecast_adjusted - valid

# Calculate residuals for the sarima model
residuals_sarima <- residuals(sarima_model)

# Plot residuals
par(mfrow = c(2, 2))  # Create a 2x2 grid of plots

plot(residuals_hw, main = "Residuals - Holt-Winters Model 1")
plot(residuals_hw_1, main = "Residuals - Holt-Winters Model 2")
plot(residuals_hw_2, main = "Residuals - Holt-Winters Model 3")
plot(residuals_hw_3, main = "Residuals - Holt-Winters Model 4")
plot(residuals_auto, main = "Residuals - auto.arima model")
plot(residuals_sarima, main = "Residuals - Sarima model")

# Reset the plotting layout
par(mfrow = c(1, 1))

# Calculate other metrics
mae1 <- mean(abs(residuals_hw))
mae2 <- mean(abs(residuals_hw_1))
mae3 <- mean(abs(residuals_hw_2))
mae4 <- mean(abs(residuals_hw_3))
mae_auto <- mean(abs(residuals_auto))
mae_sarima <- mean(abs(residuals_sarima))

#Calculate BIC and AIC parameters

# Calculate the number of parameters for each model
HW_1_params <- 3  # Assuming alpha, beta, and gamma are included
HW_2_params <- 2  # Assuming alpha and gamma are included
HW_3_params <- 2  # Assuming alpha and beta are included
HW_4_params <- 1  # Assuming only alpha is included

# Calculate the number of observations
n <- length(NO_Annecy)

# Calculate the AIC for each model
HW_1_AIC <- n * log(HW_1_SSE/n) + 2 * HW_1_params
HW_2_AIC <- n * log(HW_2_SSE/n) + 2 * HW_2_params
HW_3_AIC <- n * log(HW_3_SSE/n) + 2 * HW_3_params
HW_4_AIC <- n * log(HW_4_SSE/n) + 2 * HW_4_params
sarima_AIC <- AIC(sarima_model)

# Calculate the BIC for each model
HW_1_BIC <- n * log(HW_1_SSE/n) + log(n) * HW_1_params
HW_2_BIC <- n * log(HW_2_SSE/n) + log(n) * HW_2_params
HW_3_BIC <- n * log(HW_3_SSE/n) + log(n) * HW_3_params
HW_4_BIC <- n * log(HW_4_SSE/n) + log(n) * HW_4_params
sarima_BIC <- BIC(sarima_model)

# Calculate the number of parameters
arima_params <- length(coef(auto_model))

# Calculate the number of observations
n <- length(NO_Annecy)

# Calculate the AIC
arima_AIC <- n * log(sse_auto/n) + 2 * arima_params

# Calculate the BIC
arima_BIC <- n * log(sse_auto/n) + log(n) * arima_params

# Print the AIC and BIC for the auto.arima model
cat("Auto-arima AIC:", arima_AIC, "BIC:", arima_BIC, "\n")


# Create a data frame of the model parameters
model_params_1 <- data.frame(Model = c("Holt-Winters", "Holt-Winters (Beta = FALSE, Gamma = TRUE)", "Holt-Winters (Beta = TRUE, Gamma = FALSE)", "Holt-Winters (Beta = FALSE, Gamma = FALSE)", "Auto ARIMA", "SARIMA model"),
                             MAE = c(mae1, mae2, mae3, mae4, mae_auto, mae_sarima),
                             SSE = c(HW_1_SSE, HW_2_SSE, HW_3_SSE, HW_4_SSE, sse_auto, sse_sarima ),
                             AIC = c(HW_1_AIC, HW_2_AIC, HW_3_AIC, HW_4_AIC, arima_AIC, sarima_AIC ),
                             BIC = c(HW_1_BIC, HW_2_BIC, HW_3_BIC, HW_4_BIC, arima_BIC, sarima_BIC)
)
library(knitr)
# Print the table
kable(model_params_1, align = "c")


#############   NO_Lyon   ############

# Data selection and transformation

air_data %>% 
  filter(city == "Lyon") %>%
  arrange(date) %>%
  select(monoxyde_azote) %>%
  ts(start = c(2015, 1), end = c(2022, 12), frequency = 12) -> NO_Lyon

plot(NO_Lyon)
plot(decompose(NO_Lyon))

# Decompose the data

## Checking the trend

### Run ADF test on NO_Lyon time series
adf_test_2 <- ur.df(NO_Lyon, type = "drift", selectlags = "AIC")

# Print ADF test results
summary(adf_test_2)

### Run KPSS test on NO_Lyon time series
kpss_test_2 <- ur.kpss(NO_Lyon)

# Print the results
print(kpss_test_2)
summary(kpss_test_2)

## Decompose the data
decompose(NO_Lyon)$seasonal
decompose(NO_Lyon)$trend
decompose(NO_Lyon)$random


## Modeling and Prediction

# First case: Full model

model_1<- HoltWinters(NO_Lyon)
previsions_1<- predict(model_1, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

# Second case: beta=false
model_2<- HoltWinters(NO_Lyon, beta = FALSE, gamma = TRUE)
previsions_2<-predict(model_2, n.ahead = 12, prediction.interval = TRUE, level = 0.95)


# Third case: gamma=false
model_3<- HoltWinters(NO_Lyon, beta = TRUE, gamma = FALSE)
previsions_3<-predict(model_3, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

# Fourth case: beta=false & gamma=false
model_4<- HoltWinters(NO_Lyon, beta = FALSE, gamma = FALSE)
previsions_4<-predict(model_4, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

#Auto-arima model
library(forecast)
auto_model <- auto.arima(NO_Lyon)
previsions_5 <- forecast(auto_model, h = 12, level = 0.95)

#SARIMA model
sarima_model <- auto.arima(NO_Lyon, seasonal = TRUE)
previsions_6 <- forecast(sarima_model, h=12, level = 0.95)

par(mfrow= c(2,2))
plot(model_1, previsions_1, main = "Holt-Winters Model 1")
plot(model_2, previsions_2, main = "Holt-Winters Model 2 (Beta=False)")
plot(model_3, previsions_3, main = "Holt-Winters Model 3 (Gamma=False)")
plot(model_4, previsions_4, main = "Holt-Winters Model 4 (beta=false & gamma=false)")
plot(previsions_5, main = "Auto.arima model")
plot(previsions_6, main = "Sarima model")
par(mfrow= c(1,1))

## Choice between models

HW_1_SSE <- HoltWinters(NO_Lyon)$SSE 
HW_2_SSE <- HoltWinters(NO_Lyon, beta = FALSE, gamma = TRUE)$SSE
HW_3_SSE <- HoltWinters(NO_Lyon,beta = TRUE, gamma = FALSE)$SSE
HW_4_SSE <- HoltWinters(NO_Lyon, beta = FALSE, gamma = FALSE)$SSE

valid <- NO_Lyon
# Generate forecasts using the auto.arima model
forecast_valid <- forecast(auto_model, h = length(valid))

# Extract the forecasted values
forecast_values <- forecast_valid$mean

# Trim the forecasted values to match the length of the validation set
forecast_adjusted <- forecast_values[1:length(valid)]

# Remove missing values from forecasted values and validation set
forecast_adjusted <- forecast_adjusted[!is.na(forecast_adjusted)]
valid <- valid[!is.na(valid)]

# Calculate the SSE
sse_auto <- sum((forecast_adjusted - valid)^2, na.rm = TRUE)

# Display the SSE
print(paste("SSE for the auto.arima model:", sse_auto))

# SSE for SARIMA
actual_values <- NO_Lyon[(length(NO_Lyon) - 11):length(NO_Lyon)]
sse_sarima <- sum((previsions_6$mean - actual_values)^2)
cat("Sum of Squared Errors (SSE):", sse_sarima, "\n")

########### Residuals##########

# Calculate residuals for Holt-Winters Model 
residuals_hw <- residuals(model_1)

# Calculate residuals for Holt-Winters Model 1
residuals_hw_1 <- residuals(model_2)

# Calculate residuals for Holt-Winters Model 2
residuals_hw_2 <- residuals(model_3)

# Calculate residuals for Holt-Winters Model 3
residuals_hw_3 <- residuals(model_4)

# Calculate residuals for the auto.arima model
residuals_auto <- forecast_adjusted - valid

# Calculate residuals for the sarima model
residuals_sarima <- residuals(sarima_model)

# Plot residuals
par(mfrow = c(2, 2))  # Create a 2x2 grid of plots

plot(residuals_hw, main = "Residuals - Holt-Winters Model 1")
plot(residuals_hw_1, main = "Residuals - Holt-Winters Model 2")
plot(residuals_hw_2, main = "Residuals - Holt-Winters Model 3")
plot(residuals_hw_3, main = "Residuals - Holt-Winters Model 4")
plot(residuals_auto, main = "Residuals - auto.arima model")
plot(residuals_sarima, main = "Residuals - Sarima model")
par(mfrow = c(1, 1))

# Calculate other metrics
mae1 <- mean(abs(residuals_hw))
mae2 <- mean(abs(residuals_hw_1))
mae3 <- mean(abs(residuals_hw_2))
mae4 <- mean(abs(residuals_hw_3))
mae_auto <- mean(abs(residuals_auto))
mae_sarima <- mean(abs(residuals_sarima))

#Calculate BIC and AIC parameters

# Calculate the number of parameters for each model
HW_1_params <- 3  # Assuming alpha, beta, and gamma are included
HW_2_params <- 2  # Assuming alpha and gamma are included
HW_3_params <- 2  # Assuming alpha and beta are included
HW_4_params <- 1  # Assuming only alpha is included

# Calculate the number of observations
n <- length(NO_Lyon)

# Calculate the AIC for each model
HW_1_AIC <- n * log(HW_1_SSE/n) + 2 * HW_1_params
HW_2_AIC <- n * log(HW_2_SSE/n) + 2 * HW_2_params
HW_3_AIC <- n * log(HW_3_SSE/n) + 2 * HW_3_params
HW_4_AIC <- n * log(HW_4_SSE/n) + 2 * HW_4_params
sarima_AIC <- AIC(sarima_model)

# Calculate the BIC for each model
HW_1_BIC <- n * log(HW_1_SSE/n) + log(n) * HW_1_params
HW_2_BIC <- n * log(HW_2_SSE/n) + log(n) * HW_2_params
HW_3_BIC <- n * log(HW_3_SSE/n) + log(n) * HW_3_params
HW_4_BIC <- n * log(HW_4_SSE/n) + log(n) * HW_4_params
sarima_BIC <- BIC(sarima_model)


# Calculate the number of parameters
arima_params <- length(coef(auto_model))

# Calculate the number of observations
n <- length(NO_Lyon)

# Calculate the AIC
arima_AIC <- n * log(sse_auto/n) + 2 * arima_params

# Calculate the BIC
arima_BIC <- n * log(sse_auto/n) + log(n) * arima_params

# Print the AIC and BIC for the auto.arima model
cat("Auto-arima AIC:", arima_AIC, "BIC:", arima_BIC, "\n")


# Create a data frame of the model parameters
model_params_2 <- data.frame(Model = c("Holt-Winters", "Holt-Winters (Beta = FALSE, Gamma = TRUE)", "Holt-Winters (Beta = TRUE, Gamma = FALSE)", "Holt-Winters (Beta = FALSE, Gamma = FALSE)", "Auto ARIMA", "SARIMA model"),
                             MAE = c(mae1, mae2, mae3, mae4, mae_auto, mae_sarima),
                             SSE = c(HW_1_SSE, HW_2_SSE, HW_3_SSE, HW_4_SSE, sse_auto, sse_sarima),
                             AIC = c(HW_1_AIC, HW_2_AIC, HW_3_AIC, HW_4_AIC, arima_AIC, sarima_AIC),
                             BIC = c(HW_1_BIC, HW_2_BIC, HW_3_BIC, HW_4_BIC, arima_BIC, sarima_BIC)
)
library(knitr)
# Print the table
kable(model_params_2, align = "c")
