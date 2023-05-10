# FPP3

# Chapter 8 - Exponential Smoothing ----

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)
# theme_set(theme_minimal())

## Exercise 8.8.5 ----

# filter Country == 'Italy' from `global_economy`
italy_economy <- global_economy %>% 
     filter(Country == 'Italy')

# a. Plot the Exports series and discuss the main features of the data.

# plot `Exports`
p1 <- italy_economy %>% 
     autoplot(Exports) + 
     geom_smooth(method = 'loess', se = FALSE, color = 'blue') + 
     labs(y = 'Exports (% of GDP)', 
          title = 'Exports: Italy')

ggplotly(p1)

# STL decomposition method
dcmp <- italy_economy %>%
     model(stl = STL(Exports))

components(dcmp)

# plot STL decomposition
components(dcmp) %>% 
     autoplot()

# Trend component is present, but not uniform. No seasonality componenet detected.

# b. Use an ETS(A,N,N) model to forecast the series and plot the forecasts.

fit_ets_1 <- italy_economy %>% 
     model(ETS(Exports ~ error('A') + trend('N') + season('N')))

fit_ets_1 %>% 
     forecast(h = 5) %>% 
     autoplot(italy_economy, level = NULL) + 
     labs(y = 'Exports (% of GDP)', 
          title = 'Exports: Italy') + 
     guides(colour = guide_legend(title = 'Forecast'))

# c. Compute the RMSE values for the training data

fit_ets_1 %>% 
     accuracy() %>% 
     select(Country, .model, .type, RMSE)

# estimate parameters
tidy(fit_ets_1)

# d. Compare the results with an ETS(A,A,N) model.
# e. Compare the forecasts from both models. Which is the best?

fit_ets_2 <- italy_economy %>% 
     model(ETS(Exports ~ error('A') + trend('A') + season('N')))

fit_ets_2 %>% 
     forecast(h = 5) %>% 
     autoplot(italy_economy, level = NULL) + 
     labs(y = 'Exports (% of GDP)', 
          title = 'Exports: Italy') + 
     guides(colour = guide_legend(title = 'Forecast'))

# f. Calculate a 95% prediction interval for the first forecast for each model, 
# using the RMSE values and assuming normal errors

# first model ETS(A,N,N)
fit_ets_1 %>% 
     forecast(h = 1) %>% 
     hilo(level = 95)

# second model ETS(A,A,N)
fit_ets_2 %>% 
     forecast(h = 1) %>% 
     hilo(level = 95)

# using RMSE to calculate the 95% prediction interval
# Source: https://dtkaplan.github.io/SDS-book/mean-square-error.html#:~:text=The%20RMSE%20provides%20an%20operational,function%20output%20minus%20the%20RMSE.

RMSE_1 <- fit_ets_1 %>% 
     accuracy() %>% 
     pull(RMSE)

fit_ets_1 %>% 
     forecast(h = 1) %>% 
     mutate(conf_lo = .mean - 2 * RMSE_1, 
            conf_hi = .mean + 2 * RMSE_1)

RMSE_2 <- fit_ets_2 %>% 
     accuracy() %>% 
     pull(RMSE)

fit_ets_2 %>% 
     forecast(h = 1) %>% 
     mutate(conf_lo = .mean - 2 * RMSE_2, 
            conf_hi = .mean + 2 * RMSE_2)
