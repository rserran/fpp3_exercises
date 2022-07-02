# Time Series - Modeltime
# Source: https://analyticslog.com/blog/2021/7/07/r-modeltime-package-build-compare-multiple-time-series-forecast-models-quickly

# load packages
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
library(timetk)
library(modeltime)
library(lubridate)

# read dataset (us_retail_employment)
library(fpp3)
data("us_employment")

us_retail_employment <- us_employment %>%
     filter(year(Month) >= 1990, Title == "Retail Trade") %>%
     select(-Series_ID, -Title) %>% 
     mutate(Month = as.Date(Month)) %>% 
     as_tibble() %>% 
     rename(date = Month, value = Employed)

us_retail_employment

# plot time series
us_retail_employment %>% 
     plot_time_series(date, value)

# split data into training /test
splits <- us_retail_employment %>% 
     filter(date >= '1990-01-01' & date < '2018-10-01') %>% 
     time_series_split(
          assess     = "12 months", 
          cumulative = TRUE
)

# actual data
data <- us_retail_employment %>% 
     filter(date >= '2017-10-01' & date <= '2018-09-01')

# plot training and test ts
splits %>%
     tk_time_series_cv_plan() %>%
     plot_time_series_cv_plan(date, value)

# FORECAST ----

library(prophet)
library(forecast)

# * AUTO ARIMA ----

model_arima <- arima_reg() %>%
     set_engine("auto_arima") %>%
     fit(value ~ date, training(splits))

model_arima

# * Prophet ----

model_prophet <- prophet_reg(
     mode="regression",
     seasonality_weekly = "auto",
     seasonality_yearly = "auto",
     seasonality_daily = "auto"
) %>%
     set_engine("prophet") %>%
     fit(value ~ date, training(splits))

model_prophet

# AUTO ETS Model
model_ets <- exp_smoothing()%>%
     set_engine("ets") %>%
     fit(value ~ date,training(splits))

model_ets

# Snaive model
model_snaive <- naive_reg() %>%
     set_engine("snaive") %>%
     fit(value~date,training(splits))

model_snaive

# MODELTIME COMPARE ----

# * Modeltime Table ----
model_tbl <- modeltime_table(
     model_arima,
     model_prophet,
     model_ets,
     model_snaive
)

model_tbl

# * Calibrate ----
calib_tbl <- model_tbl %>%
     modeltime_calibrate(testing(splits))

# * Accuracy ----
calib_tbl %>% modeltime_accuracy()

# * Test Set Visualization ----
calib_tbl %>%
     modeltime_forecast(
          new_data    = testing(splits),
          actual_data = data,
          conf_interval = 0
     ) %>%
     plot_modeltime_forecast()
