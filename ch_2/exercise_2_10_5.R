# FPP3

# Chapter 2 - Time Series Graphics

# Exercise 2.10.5

# load packages
suppressMessages(library(tidyverse))
library(readxl)
library(fpp3)
library(plotly)

# tsibble::tourism
tourism

# read data
tourism <- read_excel('./ch_2/tourism.xlsx') %>% 
     mutate(Quarter = yearquarter(Quarter)) %>% 
     as_tsibble(
          index = "Quarter", 
          key = c("Region", "State", "Purpose")
     )

tourism

# 'Trips' maximum average by 'Region' and 'Purpose'
tourism %>% 
     select(Quarter, Region, Purpose, Trips) %>% 
     features(Trips, features = list(mean = mean)) %>% 
     slice_max(mean)

# create new tsibble that combines 'Region' and 'Purpose', and just has total
# trips by 'State'
total_trips_state_tsbl <- tourism %>% 
     select(Quarter, State, Trips) %>% 
     group_by(State) %>% 
     summarise(total_trips = sum(Trips))

total_trips_state_tsbl

g <- total_trips_state_tsbl %>% 
     autoplot()

ggplotly(g)
