# FPP3

# Chapter 12 - Dynamic regression models ----

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)
theme_set(theme_light())

## 12.1 - Complex seasonality ----

# North American commercial bank calls per 5-minute interval
bank_calls |> 
     fill_gaps() |> 
     autoplot(Calls) + 
     geom_smooth(method = 'loess', linewidth = 1.5,  color = 'steelblue') + 
     labs(y = "Calls",
          title = "Five-minute call volume to bank")

# 4-week interval
bank_calls |> 
     filter(date(DateTime) < '2003-03-31') |> 
     fill_gaps() |> 
     autoplot(Calls) + 
     labs(y = "Calls",
          title = "Five-minute call volume to bank")

# STL with multiple seasonal periods

calls <- bank_calls |>
     mutate(t = row_number()) |>
     update_tsibble(index = t, regular = TRUE)

calls |>
     model(
          STL(sqrt(Calls) ~ season(period = 169) +
                   season(period = 5*169),
              robust = TRUE)
     ) |>
     components() |>
     autoplot() + labs(x = "Observation")

# Forecasts from STL+ETS decomposition

my_dcmp_spec <- decomposition_model(
     STL(sqrt(Calls) ~ season(period = 169) +
              season(period = 5*169),
         robust = TRUE),
     ETS(season_adjust ~ season("N"))
)

fc <- calls |>
     model(my_dcmp_spec) |>
     forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls |>
     new_data(n = 7 * 24 * 60 / 5) |>
     mutate(time = format(DateTime, format = "%H:%M:%S")) |>
     filter(
          time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
          wday(DateTime, week_start = 1) <= 5
     ) |>
     mutate(t = row_number() + max(calls$t)) |>
     left_join(fc, by = "t") |>
     as_fable(response = "Calls", distribution = Calls)

# Plot results with last 3 weeks of data

fc_with_times |>
     fill_gaps() |>
     autoplot(bank_calls |> tail(14 * 169) |> fill_gaps()) +
     labs(y = "Calls",
          title = "Five-minute call volume to bank")

### Dynamic harmonic regression with multiple seasonal periods ----

fit <- calls |>
     model(
          dhr = ARIMA(sqrt(Calls) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                           fourier(period = 169, K = 10) +
                           fourier(period = 5*169, K = 5)))

fc <- fit |> forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls |>
     new_data(n = 7 * 24 * 60 / 5) |>
     mutate(time = format(DateTime, format = "%H:%M:%S")) |>
     filter(
          time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
          wday(DateTime, week_start = 1) <= 5
     ) |>
     mutate(t = row_number() + max(calls$t)) |>
     left_join(fc, by = "t") |>
     as_fable(response = "Calls", distribution = Calls)

# Plot results with last 3 weeks of data
fc_with_times |>
     fill_gaps() |>
     autoplot(bank_calls |> tail(14 * 169) |> fill_gaps()) +
     labs(y = "Calls",
          title = "Five-minute call volume to bank")

### Example: Electricity demand ----

vic_elec |>
     pivot_longer(Demand:Temperature, names_to = "Series") |>
     ggplot(aes(x = Time, y = value)) +
     geom_line() + 
     geom_smooth(method = 'loess', linewidth = 1.5,  
                 se = FALSE, color = 'steelblue') + 
     facet_grid(rows = vars(Series), scales = "free_y") +
     labs(y = "")

# plotting electricity demand against temperature

elec <- vic_elec |>
     mutate(
          DOW = wday(Date, label = TRUE),
          Working_Day = !Holiday & !(DOW %in% c("Sat", "Sun")),
          Cooling = pmax(Temperature, 18)
     )

elec |>
     ggplot(aes(x=Temperature, y=Demand, col=Working_Day)) +
     geom_point(alpha = 0.6) +
     labs(x="Temperature (degrees Celsius)", y="Demand (MWh)")

### ARIMA ----

fit <- elec |>
     model(
          ARIMA(Demand ~ PDQ(0, 0, 0) + pdq(d = 0) +
                     Temperature + Cooling + Working_Day +
                     fourier(period = "day", K = 10) +
                     fourier(period = "week", K = 5) +
                     fourier(period = "year", K = 3))
     )

# forecast future electricity demand
elec_newdata <- new_data(elec, 2*48) |>
     mutate(
          Temperature = tail(elec$Temperature, 2 * 48),
          Date = lubridate::as_date(Time),
          DOW = wday(Date, label = TRUE),
          Working_Day = (Date != "2015-01-01") &
               !(DOW %in% c("Sat", "Sun")),
          Cooling = pmax(Temperature, 18)
     )

fc <- fit |>
     forecast(new_data = elec_newdata)

fc |>
     autoplot(elec |> tail(10 * 48)) +
     labs(title="Half hourly electricity demand: Victoria",
          y = "Demand (MWh)", x = "Time [30m]")

# residual diagnostics
fit |> 
     gg_tsresiduals()

# TBATS model

bank_calls$Calls |>
     forecast::tbats() |>
     forecast() |>
     autoplot()

## 12.2 Prophet ----

### Example: Quarterly cement production ----

library(fable.prophet)

cement <- aus_production |>
     filter(year(Quarter) >= 1988)

train <- cement |>
     filter(year(Quarter) <= 2007)

fit <- train |>
     model(
          arima = ARIMA(Cement),
          ets = ETS(Cement),
          prophet = prophet(Cement ~ season(period = 4, order = 2,
                                            type = "multiplicative"))
     )

fc <- fit |> forecast(h = "2 years 6 months")

fc |> autoplot(cement)

# evaluate models
fc |> 
     accuracy(cement)

### Example: Half-hourly electricity demand ----

fit <- elec |>
     model(
          prophet(Demand ~ Temperature + Cooling + Working_Day +
                       season(period = "day", order = 10) +
                       season(period = "week", order = 5) +
                       season(period = "year", order = 3))
     )

fit |>
     components() |>
     autoplot()

# residuals diagnostics
fit |> 
     gg_tsresiduals()

# forecast
fc <- fit |>
     forecast(new_data = elec_newdata)

fc |>
     autoplot(elec |> tail(10 * 48)) +
     labs(x = "Date", y = "Demand (MWh)")

## 12.3 Vector autoregressions ----

### Example: A VAR model for forecasting US consumption ----

fit <- us_change |>
     model(
          aicc = VAR(vars(Consumption, Income)),
          bic = VAR(vars(Consumption, Income), ic = "bic")
     )

fit

glance(fit)

# residual ACF
fit |>
     augment() |>
     ACF(.innov) |>
     autoplot()

# forecasts plot generated by VAR(5) model
fit |>
     select(aicc) |>
     forecast() |>
     autoplot(us_change |> 
                   filter(year(Quarter) > 2010)
              )

## 12.4 Neural network models

### Example: Sunspots ----

sunspots <- sunspot.year |> 
     as_tsibble()

sunspots |> 
     autoplot(value) + 
     geom_smooth(method = 'loess', se = FALSE, 
                 linewidth = 1.5, color = 'steelblue')

fit <- sunspots |>
     model(NNETAR(sqrt(value)))

fit |>
     forecast(h = 30) |>
     autoplot(sunspots) +
     labs(x = "Year", 
          y = "Counts", 
          title = "Yearly sunspots")

# Simulation of 9 possible future sample paths for the sunspot data.
fit |>
     generate(times = 9, h = 30) |>
     autoplot(.sim) +
     autolayer(sunspots, value) +
     theme(legend.position = "none")

## 12.5 Bootstrapping and bagging ----

### Bootstrapping time series ----

# cement production
cement <- aus_production |>
     filter(year(Quarter) >= 1988) |>
     select(Quarter, Cement)

cement_stl <- cement |>
     model(stl = STL(Cement))

cement_stl |>
     components() |>
     autoplot()

# generate bootstrapped versions of the data
cement_stl |>
     generate(new_data = cement, times = 10,
              bootstrap_block_size = 8) |>
     autoplot(.sim) +
     autolayer(cement, Cement) +
     guides(colour = "none") +
     labs(title = "Cement production: Bootstrapped series",
          y="Tonnes ('000)")

### Bagged forecasts ----

# simulate many time series of the original data
sim <- cement_stl |>
     generate(new_data = cement, times = 100,
              bootstrap_block_size = 8) |>
     select(-.model, -Cement)

# fit ETS to each of the new generated time series
ets_forecasts <- sim |>
     model(ets = ETS(.sim)) |>
     forecast(h = 12)

ets_forecasts |>
     update_tsibble(key = .rep) |>
     autoplot(.mean) +
     autolayer(cement, Cement) +
     guides(colour = "none") +
     labs(title = "Cement production: bootstrapped forecasts",
          y="Tonnes ('000)")

# average these forecasts for each time period to obtain the “bagged forecasts” 
# for the original data.
bagged <- ets_forecasts |>
     summarise(bagged_mean = mean(.mean))

cement |>
     model(ets = ETS(Cement)) |>
     forecast(h = 12) |>
     autoplot(cement) +
     autolayer(bagged, bagged_mean, col = "#D55E00") +
     labs(title = "Cement production in Australia",
          y="Tonnes ('000)")
