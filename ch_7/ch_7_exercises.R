# FPP3

# Chapter 7 - Time series regression models ----

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)
theme_set(theme_minimal())

## Exercise 7.10.1 ----

# Victoria half-hour electricity demand for January 2014
vic_elec_jan14 <- vic_elec %>% 
     filter(yearmonth(Time) == yearmonth("2014 Jan")) %>% 
     index_by(Date = as_date(Time)) %>% 
     summarise(
          Demand = sum(Demand), 
          Temperature = max(Temperature) # select maximum temperature
     )

vic_elec_jan14    

# plot the time series
vic_elec_jan14 %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE, color = 'steelblue')

# time regression model
fit_vic_elec_jan14 <- vic_elec_jan14 %>% 
     model(tslm = TSLM(Demand ~ Temperature))

report(fit_vic_elec_jan14)

# Temperature variable is highly significant.
# Adjusted R-squared = 0.7757

# residual plot
fit_vic_elec_jan14 %>% 
     gg_tsresiduals()

# residuals normality
innovation_residuals <- augment(fit_vic_elec_jan14) %>% 
     pull(.innov)

# plot innovation residuals
augment(fit_vic_elec_jan14) %>% 
     select(Date, .innov) %>% 
     ggplot(aes(Date, .innov)) + 
     geom_line(linewidth = 0.5) + 
     geom_point(size = 2) + 
     geom_smooth(method = 'lm', se = FALSE, color = 'steelblue')

# Shapiro-Wilk test
shapiro.test(innovation_residuals)

# Ljung-Box test
augment(fit_vic_elec_jan14) %>% 
     features(.innov, ljung_box, lag = 14)

# even though the plots indicate a trend in the innovation residuals and
# left skewness in the residuals histogram, the Shapiro-Wilk normality test
# and Ljung-Box test indicate normality and uniform variance.

# forecast next day with maximum temperature = 15 degrees Celsius
fcst_vic_elec_jan14_15 <- vic_elec_jan14 %>% 
     model(tslm = TSLM(Demand ~ Temperature)) %>% 
     forecast(
          new_data(vic_elec_jan14, 1) %>% 
               mutate(Temperature = 15)
     ) 

fcst_vic_elec_jan14_15 %>% 
     autoplot()

fcst_vic_elec_jan14_15$Demand[1]

# Source: https://robjhyndman.com/hyndsight/fable/
# 80% prediction intervals
hilo(fcst_vic_elec_jan14_15, level = 80)

# 95% prediction intervals
hilo(fcst_vic_elec_jan14_15, level = 95)

# forecast next day with maximum temperature = 35 degrees Celsius
vic_elec_jan14 %>% 
     model(tslm = TSLM(Demand ~ Temperature)) %>% 
     forecast(
          new_data(vic_elec_jan14, 1) %>% 
               mutate(Temperature = 35)
     ) %>% 
     autoplot()

# compare to actual demand for Feb 1, 2014
vic_elec %>% 
     filter(yearmonth(Time) == yearmonth("2014 Feb")) %>% 
     index_by(Date = as_date(Time)) %>% 
     summarise(
          Demand = sum(Demand), 
          Temperature = max(Temperature) # select maximum temperature
     ) %>% 
     slice(1)

# forecast demand for maximum temperature = 15 degrees Celsius --> 151,398 (mean)
# forecast demand for maximum temperature = 35 degrees Celsius --> 274,484 (mean)
# actual demand for Feb 1, 2014 --> 241,283 with maximum temperature of 29.2 degrees Celsius

# plot demand vs temperature for all data in `vic_elec`, aggregated to daily total 
# demand and maximum temperature.
# vic_elec %>%
#      select(Date, Demand, Temperature) %>% 
#      mutate(Demand = scale(Demand), 
#             Temperature = scale(Temperature)) %>% 
#      pivot_longer(c(Demand, Temperature), names_to = 'Series') %>%
#      autoplot(value)

vic_elec %>% 
     select(Date, Demand, Temperature) %>% 
     index_by(Date) %>% 
     summarise(
          Demand = sum(Demand), 
          Temperature = max(Temperature) # select maximum temperature
     ) %>% 
     mutate(
          Demand = scale(Demand), 
          Temperature = scale(Temperature)
     ) %>% 
     pivot_longer(c(Demand, Temperature), names_to = 'Series') %>%
     autoplot(value) + 
     labs(x = NULL, 
          y = 'Value', 
          title = 'Victoria 2014 Electricity Daily Demand vs. Maximum Temperature (scaled)')
