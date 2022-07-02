# FPP3

# Chapter 2 - Time Series Graphics

# Exercise 2.10.11

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)

# Australian monthly pig slaughters time series
pigs_monthly_tsbl <- aus_livestock %>% 
     filter(Animal == "Pigs" & State == "Victoria")

pigs_monthly_tsbl

# filter between 1990 and 1995
pigs_1990_1995_tsbl <- pigs_monthly_tsbl %>% 
     mutate(Month = as.Date(Month)) %>% 
     filter(Month >= "1990-01-01" & Month < "1996-01-01") %>% 
     mutate(Month = yearmonth(Month))

pigs_1990_1995_tsbl

# plot time series
pigs_1990_1995_tsbl %>% 
     autoplot()

# ACF
pigs_1990_1995_tsbl %>% 
     ACF() %>% 
     autoplot()


# expand the original filter from 1988 to 1997
pigs_1988_1997_tsbl <- pigs_monthly_tsbl %>% 
     mutate(Month = as.Date(Month)) %>% 
     filter(Month >= "1988-01-01" & Month < "1998-01-01") %>% 
     mutate(Month = yearmonth(Month))

pigs_1988_1997_tsbl %>% 
     autoplot()

# ACF
pigs_1988_1997_tsbl %>% 
     ACF() %>% 
     autoplot()

# Question #1 - White noise --> refer to 2.9
# Question #2 - ACF illustrates more pronounced lags (stronger autocorrelation)
