# Forecasting: Principles and Practice (3rd ed.)
# Chapter 4 - 4.6 Exercises
# Source: https://otexts.com/fpp3/feast-exercises.html

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(tsibble)
library(tsibbledata)
library(plotly)

# load dataset
data(PBS)
PBS

# Exercise 4.6.1

# Write a function to compute the mean and standard deviation of a time series and 
# apply it to `PBS`

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
          autoplot(Scripts)
}

PBS %>% 
     time_series_max_mean_plot()

data <- PBS

time_series_min_sd_plot <- function(data) {
     
     max_mean_scripts_vec <- data %>% 
          features(Scripts, features = list(sd = sd)) %>% 
          slice_min(sd) %>% 
          slice(1) %>% 
          as.character()
     
     data %>% 
          filter(Concession == max_mean_scripts_vec[1] & 
                      Type == max_mean_scripts_vec[2] & 
                      ATC1 == max_mean_scripts_vec[3] & 
                      ATC2 == max_mean_scripts_vec[4]) %>% 
          autoplot(Scripts)
}
     
PBS %>% 
     time_series_min_sd_plot()

# Exercise 4.6.2



# PBS_average_std_scripts_tbl <- PBS %>% 
#      filter(ATC2 == "A10") %>% 
#      select(Month, Concession, Type, Scripts) %>% 
#      # as_tibble() %>% 
#      # group_by(ATC2) %>% 
#      summarise(avg_scripts = mean(Scripts), 
#                std_scripts = sd(Scripts))
# 
# max_avg_ATC2 <- PBS_average_std_scripts_tbl %>% 
#      slice_max(avg_scripts) %>% 
#      pull(ATC2)
# 
# # plot time series with maximum average
# max_avg_plot <- PBS %>% 
#      filter(ATC2 == max_avg_ATC2) %>% 
#      select(Month, Concession, Type, Scripts) %>% 
#      summarise(total_scripts = sum(Scripts)) %>% 
#      mutate(total_scripts = total_scripts / 1e6) %>% 
#      autoplot() + 
#      ggtitle('Maximum Average Time Series') + 
#      labs(x = "", y = "Total Scripts (Millions)")
# 
# ggplotly(max_avg_plot)
# 
# min_std_ATC2 <- PBS_average_std_scripts_tbl %>% 
#      slice_min(std_scripts) %>% 
#      pull(ATC2)
# 
# # plot time series with minimum standard deviation
# min_std_plot <- PBS %>% 
#      filter(ATC2 == min_std_ATC2) %>% 
#      select(Month, Concession, Type, Scripts) %>% 
#      summarise(total_scripts = sum(Scripts)) %>% 
#      autoplot() + 
#      ggtitle('Minimum Standard Deviation Time Series') + 
#      labs(x = "", y = "Total Scripts")
# 
# ggplotly(min_std_plot)
