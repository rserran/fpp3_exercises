# Forecasting: Principles and Practice (3rd ed.)
# Chapter 4 - 4.6 Exercises
# Source: https://otexts.com/fpp3/feast-exercises.html

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(tsibble)
library(tsibbledata)
library(plotly)

# Exercise 4.6.1 ----

# load dataset
data(PBS)
PBS

# Write a function to compute the mean and standard deviation of a time series and 
# apply it to `PBS`

time_series_mean_sd <- function(data) {
     
     data %>% 
          features(Scripts, features = list(mean = mean, sd = sd))
}

PBS %>% 
     time_series_mean_sd()

# max mean series
max_mean_scripts_vec <- PBS %>% 
     time_series_mean_sd() %>% 
     slice_max(mean) %>% 
     as.character()

# plot maximum mean time series
PBS %>% 
     filter(Concession == max_mean_scripts_vec[1] & 
                 Type == max_mean_scripts_vec[2] & 
                 ATC1 == max_mean_scripts_vec[3] & 
                 ATC2 == max_mean_scripts_vec[4]) %>% 
     autoplot(Scripts) + 
     labs(x = '', 
          y = 'Scripts count', 
          title = 'PBS Maximum Mean Time Series', 
          subtitle = str_glue("Concession: {max_mean_scripts_vec[1]}
                              Type: {max_mean_scripts_vec[2]}
                              ATC1: {max_mean_scripts_vec[3]}
                              ATC2: {max_mean_scripts_vec[4]}"
                              )
          )

time_series_max_mean_plot <- function(data) {
     
     max_mean_scripts_vec <- data %>% 
          features(Scripts, features = list(mean = mean)) %>% 
          slice_max(mean) %>% 
          as.character()
     
     data %>% 
          filter(Concession == max_mean_scripts_vec[1] & 
                      Type == max_mean_scripts_vec[2] & 
                      ATC1 == max_mean_scripts_vec[3] & 
                      ATC2 == max_mean_scripts_vec[4]) %>% 
          autoplot(Scripts) + 
          labs(x = '', 
               y = 'Scripts count', 
               title = 'PBS Maximum Mean Time Series', 
               subtitle = str_glue("Concession: {max_mean_scripts_vec[1]}
                              Type: {max_mean_scripts_vec[2]}
                              ATC1: {max_mean_scripts_vec[3]}
                              ATC2: {max_mean_scripts_vec[4]}"
               )
          )
}

PBS %>% 
     time_series_max_mean_plot()

# minimum standard deviation time series
min_sd_scripts_vec <- PBS %>% 
     time_series_mean_sd() %>% 
     slice_min(sd) %>% 
     as.vector()

# plot lowest (min) standard deviation time series
time_series_min_sd_plots <- function(data, i) {
     
     min_sd_scripts_vec <- PBS %>% 
          time_series_mean_sd() %>% 
          slice_min(sd) %>% 
          as.vector()
     
     data %>% 
          filter(Concession == min_sd_scripts_vec[i, ][1] %>% as.character() & 
                      Type == min_sd_scripts_vec[i, ][2] %>% as.character() & 
                      ATC1 == min_sd_scripts_vec[i, ][3] %>% as.character() & 
                      ATC2 == min_sd_scripts_vec[i, ][4] %>% as.character()
          ) %>% 
          autoplot(Scripts) + 
          labs(x = '',
               y = 'Scripts count',
               title = 'PBS Lowest Standard Deviation Time Series',
               subtitle = str_glue("Concession: {min_sd_scripts_vec[i, ][1]}
                              Type: {min_sd_scripts_vec[i, ][2]}
                              ATC1: {min_sd_scripts_vec[i, ][3]}
                              ATC2: {min_sd_scripts_vec[i, ][4]}"
               )
          )
}

for(i in 1:nrow(min_sd_scripts_vec)){
     
     print(PBS %>% time_series_min_sd_plots(i))
}

# Exercise 4.6.2 ----

data("tourism")
tourism

tourism_holiday_STL_features <- tourism %>% 
     filter(Purpose == 'Holiday') %>% 
     features(Trips, feature_set(pkgs = "feasts")) %>% 
     select(State, contains("trend"), contains("seasonal"))

tourism_holiday_STL_features %>% 
     glimpse()

# ggpairs
tourism_holiday_STL_features %>% 
     # mutate(State = State %>% as.factor()) %>% 
     GGally::ggpairs(mapping = aes(fill = State))

# 'State' factor order
tourism_holiday_STL_features %>% 
     # mutate(State = State %>% as.factor()) %>% 
     count(State)

# convert 'seasonal_peak_year', 'seasonal_trough_year' to factors
library(glue)

tourism_holiday_STL_features %>% 
     mutate(
          seasonal_peak_year = seasonal_peak_year +
               4*(seasonal_peak_year==0),
          seasonal_trough_year = seasonal_trough_year +
               4*(seasonal_trough_year==0),
          seasonal_peak_year = glue("Q{seasonal_peak_year}"),
          seasonal_trough_year = glue("Q{seasonal_trough_year}"),
     ) %>% 
     GGally::ggpairs(mapping = aes(fill = State)) + 
     theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Answer:
# Peak Q1: ACT, New South Wales, South Australia, Tasmania, Victoria, Western Australia
# Peak Q3: Northern Territory, Queensland
