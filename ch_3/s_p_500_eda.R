# S&P 500 Index Time Series EDA
# Source: https://medium.com/@mrconnor/exploratory-data-analysis-of-time-series-data-987f8a0c2a0a

# load packages
suppressMessages(library(tidyverse))
library(tidyquant)
library(timetk)

# read S&P 500 daily data from 10 years ago
from = today() - years(10)
from = floor_date(as.Date(from))

spx <- tq_get("^GSPC", get = 'stock.prices', from = floor_date(as.Date(from), 'month'))
spx

# plot time series
spx %>% 
     select(date, adjusted) %>% 
     plot_time_series(date, adjusted, 
                      .title = 'S&P 500 Index Daily Adjusted Closing Price (Jan 2013 - Jan 2023 (partial)')

# calculate daily return percentage
spx_return_tbl <- spx %>% 
     select(date, adjusted) %>% 
     arrange(date) %>% 
     mutate(lag = lag(adjusted, n = 1), 
            return = ((adjusted / lag) - 1) * 100) %>% 
     drop_na()

# plot return time series
spx_return_tbl %>% 
     plot_time_series(date, return, 
                      .title = 'S&P 500 Index Daily Adjusted Closing Return Percentage (Jan 2013 - Jan 2023 (partial))')

# calculate daily volatility (absolute value of return)
spx_ret_vol_tbl <- spx_return_tbl %>% 
     select(-lag) %>% 
     mutate(volatility = abs(return))

spx_ret_vol_tbl

# plot daily volatility percentage
spx_ret_vol_tbl %>% 
     plot_time_series(date, volatility, 
                      .title = 'S&P 500 Index Daily Volatility Percentage (Jan 2013 - Jan 2023 (partial))')

# save `spx_ret_vol_tbl` to rds
spx_ret_vol_tbl %>% 
     write_rds('./data/spx_return_vol_tbl.rds')