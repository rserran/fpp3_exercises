# Forecasting: Principles and Practice (3rd ed.)
# Chapter 9
# Source: https://otexts.com/fpp3/arima.html

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)
theme_set(theme_minimal())

# 9.1 - Stationarity and Differencing

## Stationarity

### Constant mean
### Constant variance
### No seasonality

# Google stock adjusted closing prices
data("gafa_stock")
gafa_stock

google_stock <- gafa_stock %>% 
     filter(Symbol == 'GOOG')

google_stock %>% 
     select(Date, Adj_Close) %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE, color = 'steelblue') + 
     labs(x = NULL, 
          y = 'Adjusted Close', 
          title = 'Google Adjusted Closing Price (2014 - 2018)')

# ggplotly
g1 <- google_stock %>% 
     select(Date, Adj_Close) %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE, color = 'steelblue') + 
     labs(x = NULL, 
          y = 'Adjusted Close', 
          title = 'Google Adjusted Closing Price (2014 - 2018)')

ggplotly(g1)

# changes (differencing) in Google adjusted closing prices
google_stock %>% 
     select(Date, Adj_Close) %>% 
     mutate(Adj_Close = difference(Adj_Close)) %>% 
     drop_na() %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE, color = 'steelblue') + 
     labs(x = NULL, 
          y = 'Adjusted Close', 
          title = 'Change in Google Adjusted Closing Price (2014 - 2018)')

# ggplotly
g2 <- google_stock %>% 
     select(Date, Adj_Close) %>% 
     mutate(Adj_Close = difference(Adj_Close)) %>% 
     drop_na() %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE, color = 'steelblue') + 
     labs(x = NULL, 
          y = 'Adjusted Close', 
          title = 'Change in Google Adjusted Closing Price (2014 - 2018)')

ggplotly(g2)

# Stationarity test (ADF test)
library(tseries)

# Google adjusted closing price
adf.test(google_stock$Adj_Close)

# change in Google adjusted closing price
chg_adj_close_vec <- google_stock %>% 
     select(Date, Adj_Close) %>% 
     mutate(Adj_Close = difference(Adj_Close)) %>% 
     drop_na() %>% 
     pull(Adj_Close) %>% 
     as_vector()

adf.test(chg_adj_close_vec)

# kpss-test
kpss.test(google_stock$Adj_Close) # null = 'Level'
kpss.test(google_stock$Adj_Close, null = 'Trend')

kpss.test(chg_adj_close_vec) # null = 'Level'
kpss.test(chg_adj_close_vec, null = 'Trend')

# Annual Canadian Lynx trappings (1821 - 1934)
data("lynx")
lynx

plot(lynx)

adf.test(lynx)

kpss.test(lynx)
