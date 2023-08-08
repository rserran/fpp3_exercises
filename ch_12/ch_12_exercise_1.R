# Chapter 12 Exercise 1

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
theme_set(theme_bw())

# load dataset
data("pedestrian")
pedestrian

pedestrian %>% 
     tail()

# count `Sensor`
pedestrian %>% 
     count(Sensor)

# filter `Sensor` == 'Southern Cross Station'
ped_SCS_imp_ts <- pedestrian %>% 
     as_tibble() %>% 
     filter(Sensor == 'Southern Cross Station') %>% 
     select(-Sensor) %>% 
     mutate(Date = as.character(Date), 
            Time = str_pad(Time, width = 2, side = 'left', pad = 0)) %>% 
     unite(date_time, Date:Time, sep = ' ') %>% 
     mutate(date_time = ymd_h(date_time)) %>% 
     select(-Date_Time) %>% 
     as_tsibble() %>% 
     fill_gaps(.full = TRUE) %>% 
     mutate(Count = zoo::na.approx(Count))
     
# plot time series     
ped_SCS_imp_ts %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE, color = 'steelblue')

# plot time series for 4 weeks
ped_SCS_imp_ts %>% 
     filter(date(date_time) < '2015-02-01 00:00:00') %>% 
     autoplot() + 
     geom_smooth(method = 'loess', se = FALSE, color = 'steelblue')

summary(ped_SCS_imp_ts$Count)

# boxplot
ped_SCS_imp_ts %>% 
     ggplot(aes(Count)) + 
     geom_boxplot()

# log1p(Count)
ped_SCS_imp_ts %>% 
     ggplot(aes(log1p(Count))) + 
     geom_boxplot()

# sqrt(Count)
ped_SCS_imp_ts %>% 
     ggplot(aes(sqrt(Count))) + 
     geom_boxplot()

# STL with multiple seasonal periods
ped_SCS_imp_ts |> 
     model(
          STL(sqrt(Count) ~ season(period = 24) + 
                              season(period = 24 * 7), 
              robust = TRUE)
     ) |> 
     components() |> 
     autoplot() + 
     labs(x = "Observation")

# forecast from STL + ETS decomposition
dcmp_spec <- decomposition_model(
     STL(sqrt(Count) ~ season(period = 24) + 
              season(period = 24 * 7), 
         robust = TRUE), 
     ETS(season_adjust ~ season('N'))
)

fc <- ped_SCS_imp_ts |> 
     model(dcmp_spec) |> 
     forecast(h = 24 * 7)

fc |> 
     autoplot(
          ped_SCS_imp_ts |> 
               tail(24 * 28)
     ) + 
     labs(y = 'Pedestrian count')

# evaluate model
ped_SCS_imp_ts |> 
     model(dcmp_spec) |> 
     accuracy()

# residual diagnostics
ped_SCS_imp_ts |> 
     model(dcmp_spec) |> 
     gg_tsresiduals()

# Dynamic harmonic regression
fit <- ped_SCS_imp_ts |> 
     model(
          dhr = ARIMA(sqrt(Count) ~ PDQ(0, 0, 0) + pdq(d = 0) + 
                           fourier(period = 24, K = 12) + 
                           fourier(period = 24 * 7, K = 8))
     )

arima_fc <- fit |> 
     forecast(h = 24 * 7)

arima_fc |> 
     autoplot(
          ped_SCS_imp_ts |> 
               tail(24 * 28)
     ) + 
     labs(y = 'Pedestrian count')

fcst <- arima_fc |> 
     as_tsibble() %>% 
     select(date_time, .mean) %>% 
     rename(Count = .mean)

ped_SCS_imp_ts |> 
     tail(24 * 28) |> 
     add_column(actual_values = 'Actual') %>% 
     bind_rows(fcst) |> 
     mutate(actual_values = replace_na(actual_values, 'Forecast')) %>% 
     ggplot(aes(date_time, Count)) + 
     geom_line(aes(color = actual_values)) + 
     scale_color_manual(values = c('black', 'steelblue')) + 
     labs(x = '', 
          y = 'Pedestrian count', 
          color = '')

# evaluate model
fit |> 
     accuracy()

# residual diagnostics
fit |> 
     gg_tsresiduals()
