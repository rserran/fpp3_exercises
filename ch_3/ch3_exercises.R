# Forecasting: Principles and Practice (3rd ed.) ----
# Chapter 3 - Time Series Decomposition ----

## 3.7 - Exercises (selected) ----

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)
theme_set(theme_minimal())

### 3.7.3 ----

# Why is a Box-Cox transformation unhelpful for the 'canadian_gas' data?

data("canadian_gas")

# plot time series
canadian_gas %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE, color = 'blue')

# Box-Cox transformation
lambda_gas <- canadian_gas %>%
     features(Volume, features = guerrero) %>% 
     pull(lambda_guerrero)

lambda_gas

canadian_gas %>% 
     autoplot(box_cox(Volume, lambda = lambda_gas)) + 
     geom_smooth(method = 'loess', se = FALSE, color = 'blue') + 
     expand_limits(y = c(0, 20)) + 
     labs(y = "Box-Cox transformed - Gas Volume")

# create function to plot 'canadian_gas'
canadian_gas_plot <- function(data, lambda = 1){
     
     data %>% 
          autoplot(box_cox(Volume, lambda = lambda)) + 
          geom_smooth(method = 'loess', se = FALSE, color = 'blue') + 
          expand_limits(y = c(0, 20)) + 
          labs(y = "Box-Cox transformed - Gas Volume")
}

canadian_gas %>% 
     canadian_gas_plot(lambda = 0.5)

# create slider GUI for lambda
library(manipulate)

manipulate(
     canadian_gas %>% 
          canadian_gas_plot(lambda = x), 
     x = slider(-1, 2, initial = 1, step = 0.1)
)

# Answer: Box-Cox transformation does not transform the seasonal variation uniformly.
# See 3.1 - Transformations and adjustments - Mathematical transformations

### 3.7.5 ----

# Find and appropiate Box-Cox transformation to stabilize the variance

# tobacco from 'aus_production'
data("aus_production")

tobacco <- aus_production %>% 
     select(Quarter, Tobacco)

# plot time series
tobacco %>% 
     autoplot() + 
     expand_limits(y = c(0, 1e4)) + 
     geom_smooth(method = 'loess', se = FALSE, color = 'blue')

lambda_tobacco <- tobacco %>%
     features(Tobacco, features = guerrero) %>% 
     pull(lambda_guerrero)

lambda_tobacco

tobacco %>% 
     autoplot(box_cox(Tobacco, lambda = lambda_tobacco)) + 
     geom_smooth(method = 'loess', se = FALSE, color = 'blue') + 
     expand_limits(y = c(0, 1e4)) + 
     labs(y = "Box-Cox transformed - Tobacco Production")

# economy passengers between Melbourne and Sydney from 'ansett'
data("ansett")

mel_syd_economy_tsbl <- ansett %>% 
     filter(Airports == 'MEL-SYD' & Class == 'Economy') %>% 
     select(Week, Passengers)

g1 <- mel_syd_economy_tsbl %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE, color = 'blue')

ggplotly(g1)

# From week W34 to W40, the passengers count is 0. Why? 
# Answer: Air traffic numbers are in thousands, and divided into first class, 
# business class and economy class. There was a major pilots' industrial dispute 
# during the data period resulting in some weeks with zero traffic.
# Source: https://rdrr.io/cran/fpp/man/melsyd.html

lambda_ans <- mel_syd_economy_tsbl %>%
     features(Passengers, features = guerrero) %>% 
     pull(lambda_guerrero)

lambda_ans

mel_syd_economy_tsbl %>% 
     autoplot(box_cox(Passengers, lambda = lambda_ans)) + 
     geom_smooth(method = 'loess', se = FALSE, color = 'blue') + 
     labs(y = "Box-Cox transformed - Passenger Traffic (MEL-SYD)")

# pdestrian counts at Southern Cross Station from 'pedestrian'
data("pedestrian")

ped_SCS_count_tbl <- pedestrian %>% 
     filter(Sensor == 'Southern Cross Station') %>% 
     as_tibble() %>% 
     select(Date, Count)

# daily pedestrian count
ped_SCS_count_tbl %>% 
     group_by(Date) %>% 
     summarise(count = sum(Count)) %>% 
     as_tsibble() %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE, color = 'blue')

# weekly pedestrian count
ped_weekly_count_tsbl <- ped_SCS_count_tbl %>% 
     mutate(Week = yearweek(Date)) %>% 
     group_by(Week) %>% 
     summarise(count = sum(Count)) %>% 
     as_tsibble()

ped_weekly_count_tsbl %>% 
     autoplot() + 
     expand_limits(y = c(0, 1e5)) + 
     geom_smooth(method = 'loess', se = FALSE, color = 'blue')

lambda_ped <- ped_weekly_count_tsbl %>%
     features(count, features = guerrero) %>% 
     pull(lambda_guerrero)

lambda_ped

ped_weekly_count_tsbl %>% 
     autoplot(box_cox(count, lambda = lambda_ped)) + 
     geom_smooth(method = 'loess', se = FALSE, color = 'blue') + 
     expand_limits(y = c(0, 1e5)) + 
     labs(y = "Box-Cox transformed - Pedestrian Count (Southern Cross Station)")

### 3.7.9 ----

# a.
# STL decomposition
# Trend: positive and steady (not cyclical)
# Seasonal (year): uniform, but variance seems to increase in the latter years
# Remainder: random around the mean until year 1991, where a significant fluctuation
# is noticed with negative and significant increase in variance.
# Monthly seasonal: May, Jul, Sep and Dec show a positive trend. Mar, Aug and Nov
# show a negative trend. Lowest mean value --> Aug, highest mean value --> Dec.

# b.
# The 1991/1992 recession period is noticeable in the remainder component.

### 3.7.10 ----

# Canadian gas production (Jan 1960 - Feb 2005)

# a.
# plot time series
canadian_gas %>% 
     autoplot()

# subseries plot
canadian_gas %>% 
     gg_subseries()

# seasonality
canadian_gas %>% 
     gg_season()

# b. STL decomposition
dcmp <- canadian_gas %>%
     model(stl = STL(Volume))

components(dcmp)

components(dcmp) %>% 
     autoplot()

# c. 
# In the year from 1960 - 1973, the curve shape is smooth. However, since 1973, 
# the pattern is more erratic and the curve depression between Jun thru Aug is 
# more pronounced.

# d. 
# seasonally adjusted component
components(dcmp) %>% 
     select(Month, season_adjust) %>% 
     autoplot()

# e. 
# X-11 method (multiplicative decomposition)
library(seasonal)
x11_dcmp <- canadian_gas %>%
     model(x11 = X_13ARIMA_SEATS(Volume ~ x11())) %>%
     components()

x11_dcmp %>% 
     select(Month, season_adjust) %>% 
     autoplot() %>% 
     labs(title = "Seasonally Adjusted - Canadian Gas Production using X-11")

components(dcmp) %>% 
     select(Month, season_adjust) %>% 
     rename(season_adjust_component = season_adjust) %>% 
     add_column(x11_dcmp$season_adjust) %>% 
     rename(season_adjust_X11 = `x11_dcmp$season_adjust`) %>% 
     
     # plot seasonal adjusted component and X-11 seasonal adjusted component
     ggplot(aes(x = Month)) + 
     geom_line(aes(y = season_adjust_component,
                   colour = "Seasonally Adjusted")) + 
     geom_line(aes(y = season_adjust_X11, colour = "X11")) + 
     scale_colour_manual(
          values = c("#0072B2", "#D55E00"),
          breaks = c("Seasonally Adjusted", "X11")
     ) + 
     labs(y = "Gas Production (billions cubic meters)",
          title = "Canadian Gas Production (Jan 1960 - Feb 2005)")
