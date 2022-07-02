# FPP3

# Chapter 2 - Time Series Graphics

# Exercise 2.10.12

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)

# Google closing stock prices
dgoog <- gafa_stock %>% 
     filter(Symbol == "GOOG", year(Date) >= 2018) %>%
     mutate(trading_day = row_number()) %>%
     update_tsibble(index = trading_day, regular = TRUE) %>%
     mutate(diff = difference(Close))

dgoog

# Questions #1 - Why the need to re-index the tsibble?
# Answer: Date has gaps. Index must be unique and continuous.

# plot diff
dgoog %>% 
     autoplot(diff)

# ACF
dgoog %>% 
     ACF(diff) %>% 
     autoplot()
