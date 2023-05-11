# Chapter 9 - Tourism dataset

# Exercise: For the `tourism` dataset, compute the total number of trips and 
# find an appropriate differencing (after transformation if necessary) to 
# obtain stationary data.

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)
theme_set(theme_minimal())

# tourism dataset
tourism

# compute total trips by quarter
tourism_trips <- tourism %>% 
     select(Quarter, Trips) %>% 
     dplyr::summarise(total_trips = sum(Trips))

p1 <- tourism_trips %>% 
     autoplot(total_trips) + 
     geom_smooth(method = 'loess', se = FALSE, color = 'steelblue') + 
     labs(title = 'Australia Total Tourism Trips')

ggplotly(p1)

# log transformation
tourism_trips %>% 
     autoplot(
          log(total_trips)
          )

# Box-Cox transform
lambda <- tourism_trips %>%
     features(total_trips, features = guerrero) %>% 
     pull(lambda_guerrero)

lambda

tourism_trips %>% 
     autoplot(
          box_cox(total_trips, lambda =  lambda)
     )

# seasonal differencing
tourism_trips %>% 
     autoplot(
          log(total_trips) %>% difference(4) %>% difference(1)
     )

# KPSS test
tourism_trips %>% 
     mutate(total_trips = log(total_trips) %>% 
                 difference(4) %>% 
                 difference(1)
            ) %>% 
     features(total_trips, unitroot_kpss)

# ADF test
library(tseries)

tourism_trips %>% 
     mutate(total_trips = log(total_trips) %>% 
                 difference(4) %>% 
                 difference(1)
     ) %>% 
     drop_na() %>% 
     pull(total_trips) %>% 
     adf.test()
