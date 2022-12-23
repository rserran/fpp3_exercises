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

# time series regression plot
bricks_tbl %>% 
     plot_time_series_regression(
          .date_var = Quarter, 
          .formula = log(Bricks) ~ as.numeric(Quarter) + 
               quarter(Quarter) + 
               year(Quarter), 
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
