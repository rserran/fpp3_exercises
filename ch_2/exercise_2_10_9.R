# FPP3

# Chapter 2 - Time Series Graphics

# Exercise 2.10.9

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)

# `Bricks` from `aus_production` time series
bricks <- aus_production %>% 
     select(Quarter, Bricks) %>% 
     filter(year(Quarter) < 2005)

bricks

# autoplot()
bricks %>% 
     autoplot() + 
     labs(title = "Australian Clay Brick Production", 
          y = "Units (million)")

# gg_season()
g1 <- bricks %>% 
     gg_season() + 
     labs(title = "Seasonal Plot: Australian Clay Brick Production", 
          y = "Units (millions)")

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

# bricks_tbl <- bricks %>% 
#      as_tibble() %>% 
#      mutate(Quarter = as.Date(Quarter)) %>% 
#      filter(Quarter < '2005-01-01')
# 
# bricks_ts <- ts(bricks_tbl$Bricks, frequency = 4, start = c(1956, 1, 1))
# 
# bricks_ts

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
