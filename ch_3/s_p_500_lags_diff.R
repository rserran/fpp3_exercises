# S&P 500 Index - Lags and Differencing Features
# Source: https://atsa-es.github.io/atsa-labs/sec-tslab-differencing-to-remove-a-trend-or-seasonal-effects.html

# load packages
suppressMessages({
     library(tidyverse)
     library(timetk)
})
theme_set(theme_minimal())

# read dataset
spx <- read_rds('./data/spx_return_vol_tbl.rds')

spx

# create lag_1, lag_2 for `adjusted` values
spx %>% 
     select(date, adjusted) %>% 
     mutate(lag_1 = lag(adjusted, n = 1), 
            lag_2 = lag(adjusted, n = 2))

# create diff_1 (first difference)
spx %>% 
     select(date, adjusted) %>% 
     mutate(lag_1 = lag(adjusted, n = 1), 
            diff_1 = adjusted - lag_1)

diff(spx$adjusted, differences = 1) %>% head(10)

# plot `adjusted` values diff_1
spx %>% 
     select(date, adjusted) %>% 
     mutate(lag_1 = lag(adjusted, n = 1), 
            diff_1 = adjusted - lag_1) %>% 
     ggplot(aes(date, diff_1)) + 
     geom_line(linewidth = 0.5)

# create diff_2 (second order differencing)
spx %>% 
     select(date, adjusted) %>% 
     mutate(diff_2 = diff_vec(adjusted, lag = 1, difference = 2))

diff(spx$adjusted, differences = 2) %>% head(10)

# plot `adjusted` values diff_2
spx %>% 
     select(date, adjusted) %>% 
     mutate(diff_2 = diff_vec(adjusted, lag = 1, difference = 2)) %>% 
     ggplot(aes(date, diff_2)) + 
     geom_line(linewidth = 0.5)
