# FPP3

# Chapter 10 - Dynamic regression models ----

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)
theme_set(theme_minimal())

# 10.2 - Regression with ARIMA errors using `fable` 

# load dataset
data("us_change")
us_change

# plot time series
us_change |>
     pivot_longer(c(Consumption, Income),
                  names_to = "var", values_to = "value") |>
     ggplot(aes(x = Quarter, y = value)) +
     geom_line() +
     facet_grid(vars(var), scales = "free_y") +
     labs(title = "US consumption and personal income",
          y = "Quarterly % change")

# ARIMA
fit <- us_change |>
     model(ARIMA(Consumption ~ Income))

report(fit)

# residuals
fit %>% 
     residuals(type = 'innovation') %>% 
     gg_tsdisplay(.resid, plot_type = 'partial') + 
     labs(title = 'ARIMA errors')

# Ljung-Box test
augment(fit) |>
     features(.innov, ljung_box, dof = 3, lag = 12)

# 10.3 - Forecasting

us_change_future <- new_data(us_change, 8) |>
     mutate(Income = mean(us_change$Income))

forecast(fit, new_data = us_change_future) |>
     autoplot(us_change) +
     labs(y = "Percentage change")

# 10.6 - Lagged predictors

insurance |>
     pivot_longer(Quotes:TVadverts) |>
     ggplot(aes(x = Month, y = value)) +
     geom_line() +
     facet_grid(vars(name), scales = "free_y") +
     labs(y = "", title = "Insurance advertising and quotations")

# it various lagged ARIMA models
fit <- insurance |> 
     
     # Restrict data so models use same fitting period
     mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) |> 
     
     # Estimate models
     model(
          lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
          lag1 = ARIMA(Quotes ~ pdq(d = 0) +
                            TVadverts + lag(TVadverts)),
          lag2 = ARIMA(Quotes ~ pdq(d = 0) +
                            TVadverts + lag(TVadverts) +
                            lag(TVadverts, 2)),
          lag3 = ARIMA(Quotes ~ pdq(d = 0) +
                            TVadverts + lag(TVadverts) +
                            lag(TVadverts, 2) + lag(TVadverts, 3))
     )

glance(fit)

# lag1 model has the lowest AICc

fit_best <- insurance |>
     model(ARIMA(Quotes ~ pdq(d = 0) +
                      TVadverts + lag(TVadverts)))

report(fit_best)

# forecast (TVadvverts = 8)
insurance_future <- new_data(insurance, 20) |>
     mutate(TVadverts = 8)

fit_best |>
     forecast(insurance_future) |>
     autoplot(insurance) +
     labs(
          y = "Quotes",
          title = "Forecast quotes with future advertising set to 8"
     )

# define function to calculate forecast based on `TVaderts` as input
forecast_quotes <- function(period = 20, TVadverts = 8){
     
     # calculate Quotes forecast
     insurance_future <- new_data(insurance, period) |> 
          mutate(TVadverts = TVadverts)
     
     fit_best |>
          forecast(insurance_future) |> 
          autoplot(insurance) +
          labs(
               y = "Quotes",
               title = "Forecast quotes with future advertising set to 8"
          )
     
     forecast_quotes_tvads <- fit_best |>
          forecast(insurance_future) %>% 
          select(.model, Month, Quotes, .mean, TVadverts)
     
     return(forecast_quotes_tvads)
}

forecast_tvads_6 <- forecast_quotes(period = 20, TVadverts = 6)
forecast_tvads_7 <- forecast_quotes(period = 20, TVadverts = 7)
forecast_tvads_8 <- forecast_quotes(period = 20, TVadverts = 8)
forecast_tvads_9 <- forecast_quotes(period = 20, TVadverts = 9)
forecast_tvads_10 <- forecast_quotes(period = 20, TVadverts = 10)

forecast_tvads_6
forecast_tvads_7
forecast_tvads_8
forecast_tvads_9
forecast_tvads_10
