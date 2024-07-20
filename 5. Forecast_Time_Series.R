
df <- ATESTADOS_2019_2023

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(forecast)
library(timetk)
library(caret)

# Ensure dplyr is loaded correctly
if ("package:dplyr" %in% search()) {
  detach("package:dplyr", unload = TRUE)
}
library(dplyr)

# Read the data from the Excel file
df <- read_excel("ATESTADOS_2019_2023.xlsx", sheet = "Sheet")

# Check the structure of the data
print(head(df))

# Prepare the data for time series analysis
df <- df %>%
  mutate(START_DATE = as.Date(DATA_INICIO),
         year_month = floor_date(START_DATE, "month")) %>%
  group_by(year_month) %>%
  summarise(total_minutes = sum(TOTAL_MINUTOS_ATESTADO, na.rm = TRUE))

# Check the structure of the prepared data
print(head(df))

# Transform the data into a time series
ts_data <- ts(df$total_minutes, start = c(2019, 1), frequency = 12)

# Split the data into training and testing sets
horizon <- 12
train_ts_data <- window(ts_data, end = c(2022, 12 - horizon))
test_ts_data <- window(ts_data, start = c(2023, 1))

# Ensure train and test data lengths
print(length(train_ts_data))
print(length(test_ts_data))

# Exploratory Data Analysis (EDA)
autoplot(ts_data) +
  ggtitle("Total Minutes of Absence per Month") +
  xlab("Year") + ylab("Total Minutes")

ggseasonplot(ts_data, period = "year") +
  ylab("Total Minutes") +
  ggtitle("Seasonal Plot: Total Minutes of Absence per Month")

gglagplot(ts_data)
ggAcf(ts_data)
ggtsdisplay(ts_data)


# Time Series Decomposition
ts_data %>% decompose(type = "additive") %>% autoplot()
ts_data %>% decompose(type = "multiplicative") %>% autoplot()

# Function to calculate metrics
calculate_metrics <- function(forecast, actual) {
  rmse <- RMSE(forecast, actual)
  mae <- MAE(forecast, actual)
  mape <- MAPE(forecast, actual)
  return(data.frame(RMSE = rmse, MAE = mae, MAPE = mape))
}

# SES

library(forecast)
library(ggplot2)

# Ajustando o modelo SES
library(forecast)
library(ggplot2)

# Ajustando o modelo SES e fazendo previsões
mod_SES <- ses(train_ts_data, h = horizon)
forecast_SES <- forecast(mod_SES)
autoplot(forecast_SES) +  autolayer(fitted(mod_SES), series = "Fitted") +   autolayer(test_ts_data, series = "Actual") +   xlab("Year") + ylab("Total Minutes") + guides(colour = guide_legend(title = "Series"))
mod_SES_res <- residuals(mod_SES)
checkresiduals(mod_SES_res)
prev_SES <- data.frame(predicted = forecast_SES$mean, actual = test_ts_data)
result_SES <- calculate_metrics(prev_SES$predicted, prev_SES$actual)


# Holt-Winters Additive
mod_HWA <- HoltWinters(train_ts_data)
forecast_HWA <- forecast(mod_HWA, h = horizon)
autoplot(forecast_HWA) + autolayer(fitted(mod_HWA)) + autolayer(test_ts_data) + xlab("Year") + ylab("Total Minutes")
mod_HWA_res <- residuals(mod_HWA)
checkresiduals(mod_HWA_res)
prev_HWA <- data.frame(predicted = forecast_HWA$mean, actual = test_ts_data)
result_HWA <- calculate_metrics(prev_HWA$predicted, prev_HWA$actual)

# Holt-Winters Multiplicative
mod_HWM <- HoltWinters(train_ts_data, seasonal = "multiplicative")
forecast_HWM <- forecast(mod_HWM, h = horizon)
autoplot(forecast_HWM) + autolayer(fitted(mod_HWM)) + autolayer(test_ts_data) + xlab("Year") + ylab("Total Minutes")
mod_HWM_res <- residuals(mod_HWM)
checkresiduals(mod_HWM_res)
prev_HWM <- data.frame(predicted = forecast_HWM$mean, actual = test_ts_data)
result_HWM <- calculate_metrics(prev_HWM$predicted, prev_HWM$actual)

# Auto.ARIMA
model_ARIMA <- auto.arima(train_ts_data)
forecast_ARIMA <- forecast(model_ARIMA, h = horizon)
autoplot(forecast_ARIMA) + autolayer(test_ts_data) + xlab("Year") + ylab("Total Minutes")
mod_ARIMA_res <- residuals(model_ARIMA)
checkresiduals(mod_ARIMA_res)
prev_ARIMA <- data.frame(predicted = forecast_ARIMA$mean, actual = test_ts_data)
result_ARIMA <- calculate_metrics(prev_ARIMA$predicted, prev_ARIMA$actual)

# Custom SARIMA
model_SARIMA <- Arima(train_ts_data, order=c(1,0,2), seasonal=c(0,1,2), include.drift = TRUE)
forecast_SARIMA <- forecast(model_SARIMA, h = horizon)
autoplot(forecast_SARIMA) + autolayer(test_ts_data) + xlab("Year") + ylab("Total Minutes")
mod_SARIMA_res <- residuals(model_SARIMA)
checkresiduals(mod_SARIMA_res)
prev_SARIMA <- data.frame(predicted = forecast_SARIMA$mean, actual = test_ts_data)
result_SARIMA <- calculate_metrics(prev_SARIMA$predicted, prev_SARIMA$actual)

# Compile results into a table
results <- data.frame(
  Model = c("SES","Holt-Winters Additive", "Holt-Winters Multiplicative", "Auto.ARIMA", "Custom SARIMA"),
  RMSE = c(result_SES$RMSE, result_HWA$RMSE, result_HWM$RMSE, result_ARIMA$RMSE, result_SARIMA$RMSE),
  MAE = c(result_SES$MAE,result_HWA$MAE, result_HWM$MAE, result_ARIMA$MAE, result_SARIMA$MAE),
  MAPE = c(result_SES$MAPE,result_HWA$MAPE, result_HWM$MAPE, result_ARIMA$MAPE, result_SARIMA$MAPE)
)

print(results)

##########################################################################################################################################
