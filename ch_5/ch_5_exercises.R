# FPP3

# Chapter 5 - The Forecaster's Toolbox ----

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)
theme_set(theme_minimal())

## Exercise 5.11.1 ----

# produce forecasts for various time series

# Australian clay bricks production
bricks <- aus_production %>% 
     select(Quarter, Bricks) %>% 
     na.omit()

bricks %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE) + 
     labs(title = "Australian Clay Brick Production", 
          y = "Units (million)")

# Fit the models
bricks_fit <- bricks %>%
     model(
          `Naïve` = NAIVE(Bricks),
          `Seasonal naïve` = SNAIVE(Bricks), 
          Drift = RW(Bricks ~ drift())
     )

# Generate forecasts for 10 quarters
bricks_fc <- bricks_fit %>% 
     forecast(h = 10)

bricks_fc

# plot time series with forecasts
bricks_fc %>% 
     autoplot(bricks, level = NULL) + 
     labs(
          y = "Millions",
          title = "Forecasts for Australian Clay Bricks Production"
     ) +
     guides(colour = guide_legend(title = "Forecast"))

# Australia household wealth
household_wealth_Australia <- hh_budget %>% 
     select(Country, Year, Wealth) %>% 
     filter(Country == 'Australia')

household_wealth_Australia %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE) + 
     labs(
          y = 'Percentage of net disposable income', 
          title = 'Australia Household Wealth'
     )

# Fit the models
household_fit <- household_wealth_Australia %>%
     model(
          `Naïve` = NAIVE(Wealth),
          Drift = RW(Wealth ~ drift())
     )

# SNAIVE model is not appropriate, since the model does not exhibit a
# seasonal pattern

# Generate forecasts for 10 quarters
household_fc <- household_fit %>% 
     forecast(h = 10)

# plot time series with forecasts
household_fc %>% 
     autoplot(household_wealth_Australia, level = NULL) + 
     labs(
          y = "Percentage of net disposable income",
          title = "Forecasts for Australia Household Wealth"
     ) +
     guides(colour = guide_legend(title = "Forecast"))

## Exercise 5.11.3 ----

# residuals diagnostics

# Extract data of interest
recent_production <- aus_production %>%
     filter(year(Quarter) >= 1992)

# plot time series
recent_production %>% 
     autoplot(Beer) + 
     geom_smooth(method = 'loess', se = FALSE) + 
     labs(title = "Australian Beer Production", 
          y = "Megaliters")

# STL decomposition method
dcmp <- recent_production %>%
     model(stl = STL(Beer))

components(dcmp)

# plot STL decomposition
components(dcmp) %>% 
     autoplot()

# Define and estimate a model
fit <- recent_production %>% 
     model(SNAIVE(Beer))

# Look at the residuals
fit %>% 
     gg_tsresiduals()

# Look a some forecasts
fit %>% 
     forecast() %>% 
     autoplot(recent_production)

# There is a strong autocorrelation in lag 4 as shown in the ACF plot.

## Exercise 5.11.4 ----

# repeat exercise 5.11.3 with Australian exports series from 'global_economy' and
# the Bricks series from 'aus_production'

# Australian exports
aus_exports <- global_economy %>% 
     select(Country, Year, Exports) %>% 
     filter(Country == 'Australia')

# plot time series
aus_exports %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE) + 
     labs(title = "Australian Exports", 
          y = "% GDP")

# STL decomposition method
dcmp <- aus_exports %>%
     model(stl = STL(Exports))

components(dcmp)

# plot STL decomposition
components(dcmp) %>% 
     autoplot()

# Define and estimate a model
fit <- aus_exports %>% 
     model(NAIVE(Exports))

# Look at the residuals
fit %>% 
     gg_tsresiduals()

# Look a some forecasts
fit %>% 
     forecast() %>% 
     autoplot(aus_exports)

# Australian clay bricks production
bricks %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE) + 
     labs(title = "Australian Clay Brick Production", 
          y = "Units (million)")

# STL decomposition method
dcmp <- bricks %>%
     model(stl = STL(Bricks))

components(dcmp)

# plot STL decomposition
components(dcmp) %>% 
     autoplot()

# Define and estimate a model
fit <- bricks %>% 
     model(SNAIVE(Bricks))

# Look at the residuals
fit %>% 
     gg_tsresiduals()

# Look a some forecasts
fit %>% 
     forecast() %>% 
     autoplot(bricks)
