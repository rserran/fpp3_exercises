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
