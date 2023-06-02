# Chapter 9 Exercise 9.11.7

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)
theme_set(theme_minimal())

# Australia air passengers dataset
aus_airpassengers

# plot time series
aus_airpassengers %>% 
     autoplot(Passengers) + 
     geom_smooth(method = 'loess', se = FALSE, color = 'steelblue') + 
     labs(title = 'Australia Total Number of Passengers (millions)')

# KPSS test
aus_airpassengers %>% 
     features(Passengers, unitroot_kpss)

# differencing time series
aus_airpassengers %>% 
     mutate(Passengers = Passengers %>% 
                 difference(1) %>%
                 difference(1)) %>%
     autoplot(Passengers) + 
     labs(title = 'Australia Total Number of Passengers (change)')

aus_airpassengers %>% 
     mutate(Passengers = Passengers %>% 
                 difference(1) %>% 
                 difference(1)
            ) %>% 
     features(Passengers, unitroot_kpss)

# a. Use ARIMA() to find an appropriate ARIMA model. What model was selected. 
# Check that the residuals look like white noise. Plot forecasts for the 
# next 10 periods.

fit_1 <- aus_airpassengers |> 
     model(ARIMA(Passengers))

report(fit_1)
tidy(fit_1)

# residuals plots
fit_1 %>% 
     gg_tsresiduals()

# forecasts for the next 10 years (periods)
fit_1 |> forecast(h=10) |>
     autoplot(aus_airpassengers) +
     labs(y = "Number of Passengers (millions)", 
          title = "Australia Total Number of Passengers")

# b. Write the model in terms of the backshift operator.

# c. Plot forecasts from an ARIMA(0,1,0) model with drift and compare these to part a.
fit_2 <- aus_airpassengers |> 
     model(ARIMA(Passengers ~ 1 + pdq(0, 1, 0)))

report(fit_2)
tidy(fit_2)

# plot forecasts (h = 10)
fit_2 |> forecast(h=10) |>
     autoplot(aus_airpassengers) +
     labs(y = "Number of Passengers (millions)", 
          title = "Australia Total Number of Passengers")

# d. Plot forecasts from an ARIMA(2,1,2) model with drift and compare these to 
# parts a and c. Remove the constant and see what happens.
fit_3 <- aus_airpassengers |> 
     model(ARIMA(Passengers ~ 1 + pdq(2, 1, 2)))

report(fit_3)
tidy(fit_3)

# plot forecasts (h = 10)
fit_3 |> forecast(h=10) |>
     autoplot(aus_airpassengers) +
     labs(y = "Number of Passengers (millions)", 
          title = "Australia Total Number of Passengers")

# Arima without constant
fit_3_constant <- aus_airpassengers %>% 
     as.ts() %>% 
     forecast::Arima(c(2, 1, 2), include.constant = TRUE)

fit_3_constant

# auto.arima
aus_airpassengers %>% 
     as.ts() %>% 
     forecast::auto.arima()

# e. Plot forecasts from an ARIMA(0,2,1) model with a constant. What happens?
fit_4 <- aus_airpassengers |> 
     model(ARIMA(Passengers ~ 1 + pdq(0, 2, 1)))

report(fit_4)
tidy(fit_4)

# plot forecasts (h = 10)
fit_4 |> forecast(h=10) |>
     autoplot(aus_airpassengers) +
     labs(y = "Number of Passengers (millions)", 
          title = "Australia Total Number of Passengers")
