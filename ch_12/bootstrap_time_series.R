# Bootstrapping Time Series for Gold Rush
# Source: https://datageeek.com/2020/12/14/bootstrapping-time-series-for-gold-rush/
# Data source: https://datageeek.com/2020/09/29/time-series-forecasting-knn-vs-arima/#train

# load packages
suppressMessages(library(tidyverse))
library(readxl)
library(forecast)
theme_set(theme_bw())

# read time series dataset
df_xautry <- read_excel("./ch_12/xau_try.xlsx") %>% 
     mutate(date = date(date))

# xautry_ts <- ts(df_xautry$price, start = c(2013,1), frequency = 12)

# plot time series
df_xautry %>% 
     ggplot(aes(date, price)) + 
     geom_line(linewidth = 1) + 
     geom_smooth(method = 'loess', se = FALSE, color = 'steelblue')

# simulation function
sim_forecast <-  function(data, nsim=100L, h, mdl=auto.arima, level=95){
     
     sim <- bld.mbb.bootstrap(data, nsim)
     
     h <- as.integer(h)
     future <- matrix(0, nrow=nsim, ncol=h)
     
     future <- sim %>% map(function(x){simulate(mdl(x),nsim=h)}) %>% 
          unlist() %>% matrix(ncol = h, nrow = nsim, byrow = TRUE)
     
     start <- tsp(data)[2]+1/12
     
     simfc <- structure(list(
          
          mean = future %>% colMeans() %>% ts(start = start, frequency = 12),
          
          lower = future %>% as.data.frame() %>% 
               map_dbl(quantile, prob = (1-level/100)/2) %>% 
               ts(start = start, frequency = 12),
          
          upper = future %>% as.data.frame() %>% 
               map_dbl(quantile, prob = (1-level/100)/2+level/100) %>% 
               ts(start = start, frequency = 12),
          
          level = level),
          class = "forecast")
     
     assign("simfc", simfc, envir = .GlobalEnv)
     
     simfc
     
}

# split dataset into train/test
train <- df_xautry %>% 
     filter(date <= '2019-02-01')

train_ts <- ts(train$price, start = c(2013,1), frequency = 12)

test <- df_xautry %>% 
     filter(date > '2019-02-01')

test_ts <- ts(test$price, start = c(2019, 3), frequency = 12)

# run simulation forecast function with train dataset
h <- 18

sim_forecast(train_ts, h = h)

# Arima model
arimafc <- train_ts %>% 
     auto.arima(stepwise = FALSE,approximation = FALSE,
                seasonal = FALSE, lambda = "auto") %>% 
     forecast(h = h, level = 95)

# plot simulation (bootstrapped samples) and Arima forecast vs. actual values
train_ts %>% 
     autoplot() + 
     labs(x = 'Year', 
          y = 'Price', 
          title = 'Monthly Golden Price per Gram') + 
     autolayer(test_ts, series="Real values", PI=FALSE) +
     autolayer(simfc, series="Bagged ARIMA", PI=FALSE) +
     autolayer(arimafc, series="ARIMA", PI=FALSE)

# evaluate models
# Arima
acc_arimafc <- arimafc %>% 
     accuracy(test_ts)

acc_arimafc[,c("RMSE","MAPE")]

# boostrap simulated time series
acc_simu <- simfc %>% 
     accuracy(test_ts)

acc_simu[,c("RMSE","MAPE")]

# Comment: Bootstrapped simulation with averaging (bagging) improves the 
# forecasts significantly.