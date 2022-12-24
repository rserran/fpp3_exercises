# FPP3

# Chapter 2 - Time Series Graphics

# Exercise 2.10.9

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)
theme_set(theme_minimal())

# `Bricks` from `aus_production` time series
bricks <- aus_production %>% 
     select(Quarter, Bricks) %>% 
     filter(year(Quarter) < 2005)

bricks

# autoplot()
bricks %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE, color = 'steelblue')
     labs(title = "Australian Clay Brick Production", 
          y = "Units (million)")

# gg_season()
g1 <- bricks %>% 
     gg_season() + 
     labs(title = "Seasonal Plot: Australian Clay Brick Production", 
          y = "Units (millions)")

g1

# interactive plot
ggplotly(g1)

# gg_subseries()
bricks %>% 
     gg_subseries() + 
     labs(title = "Subseries Plot: Australian Clay Brick Production", 
          y = "Units (millions)")

# gg_lag()
bricks %>% 
     gg_lag()

bricks %>% 
     gg_lag(geom = "point")

# ACF()
bricks %>% 
     ACF() %>% 
     autoplot()

# STL decomposition (Chapter 3)
dcmp <- bricks %>% 
     model(stl = STL(Bricks))

components(dcmp)

components(dcmp) %>% 
     autoplot()

# timetk
library(timetk)

bricks_tbl <- bricks %>%
     as_tibble() %>%
     mutate(Quarter = as.Date(Quarter)) %>%
     filter(Quarter < '2005-01-01')

bricks_tbl

# plot time series
bricks_tbl %>% 
     plot_time_series(Quarter, Bricks)

# ACF diagnostics
bricks_tbl %>% 
     plot_acf_diagnostics(Quarter, Bricks)

# seasonality
p1 <- bricks_tbl %>% 
     plot_seasonal_diagnostics(
          .date_var = Quarter, 
          .value = Bricks, 
          .feature_set = c('quarter', 'year'), 
          .interactive = FALSE
     ) + 
     
     # rotate x axis labels to 45 degree
     theme(axis.text.x = element_text(
          angle = 45, vjust = 0.5, hjust=0.5)
     )

p1

# interactive plot
ggplotly(p1)

# anomalies diagnostics
bricks_tbl %>% 
     plot_anomaly_diagnostics(
          .date_var = Quarter, 
          .value = Bricks, 
          .alpha = 0.1, 
          .max_anomalies = 0.03
     )

# getting anomaly data points in a tibble
bricks_tbl %>% 
     tk_anomaly_diagnostics(
          .date_var = Quarter, 
          .value = Bricks, 
          .alpha = 0.1, 
          .max_anomalies = 0.03
     ) %>% 
     filter(anomaly == 'Yes')

# seasonal decomposition
bricks_tbl %>% 
     plot_stl_diagnostics(
          .date_var = Quarter, 
          .value = Bricks
     )

# heteroskedasticity (variance is not uniform thru the time series) test
library(lmtest)

lm_model <- lm(Bricks ~ as.numeric(Quarter), data = bricks_tbl)

bptest(lm_model, data = bricks_tbl)

# test results indicate that heteroskedasticity (null hypothesis) is not present

# time series regression plot
bricks_tbl %>% 
     plot_time_series_regression(
          .date_var = Quarter, 
          .formula = log(Bricks) ~ as.numeric(Quarter) + 
               quarter(Quarter) + 
               year(Quarter), 
          .show_summary = TRUE
     )

# Australia Quarterly GDP
# Source: 

aus_gdp_quarterly <- read_rds('./ch_2/australia_gdp_quarterly_historical.rds')
aus_gdp_quarterly

# plot Australia GDP quarterly time series
aus_gdp_quarterly %>% 
     plot_time_series(date, Value)

# plot Australia GDP quarterly percent change
aus_gdp_quartely_std_tbl <- aus_gdp_quarterly %>% 
     mutate(Value_lag = lag_vec(Value, lag = 1)) %>% 
     drop_na() %>% 
     mutate(Value_perc_chg = ((Value - Value_lag) / Value) * 100) %>% 
     mutate(Value_std = standardize_vec(Value_perc_chg))

aus_gdp_quartely_std_tbl %>% 
     plot_time_series(date, Value_std, .title = 'Australia GDP Quarterly (normalized)')

# join with bricks
bricks_log_std_tbl <- bricks_tbl %>% 
     mutate(Bricks_log = log(Bricks), 
            Bricks_std = standardize_vec(Bricks_log))

bricks_log_std_tbl %>% 
     plot_time_series(Quarter, Bricks_std, .title = 'Australia Bricks Production (Box-Cox transformation, normalized)')

# no transformation join
bricks_gdp_joined_tbl <- bricks_tbl %>% 
     left_join(aus_gdp_quarterly, by = c('Quarter' = 'date')) %>% 
     drop_na() %>% 
     rename(date = Quarter, GDP = Value)

# plot cross correlation
bricks_gdp_joined_tbl %>% 
     plot_acf_diagnostics(
          date, 
          Bricks, 
          .ccf_vars = GDP
     )

# join transform tbls
bricks_gdp_std_joined_tbl <- bricks_log_std_tbl %>% 
     select(date = Quarter, Bricks = Bricks_std) %>% 
     left_join(
          aus_gdp_quartely_std_tbl %>% 
               select(date, GDP = Value_std)
     ) %>% 
     drop_na()

# plot Bricks and GDP time series
bricks_gdp_std_joined_tbl %>% 
     pivot_longer(-date) %>%
     plot_time_series(date, value, name, .smooth = FALSE)

# plot cross correlation
bricks_gdp_std_joined_tbl %>% 
     plot_acf_diagnostics(
          date, 
          Bricks, 
          .ccf_vars = GDP
     )

# plot time series regression
bricks_gdp_std_joined_tbl %>% 
     tk_augment_lags(
          .value = Bricks, 
          .lags = c(1, 4, 8)
     ) %>% 
     drop_na() %>% 
     plot_time_series_regression(
          .date_var = date, 
          .formula = Bricks ~ as.numeric(date) + 
               as.factor(quarter(date)) + 
               year(date) + 
               GDP, 
          
          # add lags = c(1, 4, 8)
               # Bricks_lag1 + 
               # Bricks_lag4 + 
               # Bricks_lag8, 
          .show_summary = TRUE
     )

# US Gasoline
us_gasoline

us_gasoline_month <- us_gasoline %>% 
     mutate(Month = yearmonth(Week)) %>% 
     as_tibble() %>% 
     select(Month, Barrels) %>% 
     group_by(Month) %>% 
     summarise(Barrels = sum(Barrels)) %>% 
     ungroup() %>% 
     as_tsibble(index = "Month")

us_gasoline_month

# year U.S. gasoline supply
g1 <- us_gasoline %>% 
     as_tibble() %>% 
     mutate(Year = year(Week)) %>% 
     select(Year, Barrels) %>% 
     group_by(Year) %>% 
     summarise(Barrels = sum(Barrels)) %>% 
     ungroup() %>% 
     as_tsibble(index = "Year") %>% 
     autoplot() + 
     labs(title = "U.S. Finished Motor Gasoline Supplied by Year", 
          y = "Units (millions of barrels)")

ggplotly(g1)

# autoplot()
us_gasoline %>% 
     autoplot() + 
     labs(title = "U.S. Finished Motor Gasoline Supplied", 
          y = "Units (millions of barrels per day)")

us_gasoline_month %>% 
     autoplot() + 
     labs(title = "U.S. Finished Motor Gasoline Supplied by Month", 
          y = "Units (millions of barrels)")

# gg_season()
us_gasoline %>% 
     gg_season(labels = "right") + 
     labs(title = "Seasonal Plot: U.S. Finished Motor Gasoline Supplied", 
          y = "Units (millions of barrels per day)")

us_gasoline_month %>% 
     gg_season(labels = "right") + 
     labs(title = "Seasonal Plot: U.S. Finished Motor Gasoline Supplied by Month", 
          y = "Units (millions of barrels)")

# gg_subseries()
us_gasoline %>% 
     gg_subseries() + 
     labs(title = "Subseries Plot: U.S. Finished Motor Gasoline Supplied", 
          y = "Units (millions of barrels per day)")

us_gasoline_month %>% 
     gg_subseries() + 
     labs(title = "Subseries Plot: U.S. Finished Motor Gasoline Supplied by Month", 
          y = "Units (millions of barrels)")
# gg_lag()
us_gasoline %>% 
     gg_lag(geom = "point")

us_gasoline_month %>% 
     gg_lag(geom = "point")

# ACF()
us_gasoline %>% 
     ACF() %>% 
     autoplot()

# ACF()
us_gasoline_month %>% 
     ACF() %>% 
     autoplot()
