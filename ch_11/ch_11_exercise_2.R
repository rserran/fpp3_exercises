# Chapter 11 Exercise 2

# load packages
suppressMessages(library(tidyverse))
library(fpp3)

# load tourism dataset
tourism <- tsibble::tourism |>
     mutate(State = recode(State,
                           `New South Wales` = "NSW",
                           `Northern Territory` = "NT",
                           `Queensland` = "QLD",
                           `South Australia` = "SA",
                           `Tasmania` = "TAS",
                           `Victoria` = "VIC",
                           `Western Australia` = "WA"
          )
     )

# tourism dataset with aggregations
tourism_full <- tourism |> 
     aggregate_key((State/Region) * Purpose, Trips = sum(Trips))

# fit ETS model
fit <- tourism_full |>
     filter(year(Quarter) <= 2015) |>
     model(base = ETS(Trips)) |>
     reconcile(
          bu = bottom_up(base),
          ols = min_trace(base, method = "ols"),
          mint = min_trace(base, method = "mint_shrink")
     )

# generate forecasts
fc <- fit |> forecast(h = "2 years")

# plot the four point forecasts for the overnight trips for the Australian total, 
# the states, and the purposes of travel, along with the actual observations of 
# the test set.
fc |>
     filter(is_aggregated(Region), is_aggregated(Purpose)) |>
     autoplot(
          tourism_full |> filter(year(Quarter) >= 2011),
          level = NULL
     ) +
     labs(y = "Trips ('000)", 
          title = 'Forecasts of overnight trips for Australia and its states over the test period 2016Q1–2017Q4') +
     facet_wrap(vars(State), scales = "free_y")

# forecasts by `Purpose`
fc |>
     filter(is_aggregated(State), !is_aggregated(Purpose)) |>
     autoplot(
          tourism_full |> filter(year(Quarter) >= 2011),
          level = NULL
     ) +
     labs(y = "Trips ('000)", 
          title = 'Forecasts of overnight trips by purpose of travel over the test period 2016Q1–2017Q4. ') +
     facet_wrap(vars(Purpose), scales = "free_y")

# evaluate model (State level)
fc |>
     filter(is_aggregated(Region), is_aggregated(Purpose)) |>
     accuracy(
          data = tourism_full,
          measures = list(rmse = RMSE, 
                          mase = MASE, 
                          ss = skill_score(CRPS))
     ) |>
     group_by(.model) |>
     summarise(rmse = mean(rmse), 
               mase = mean(mase), 
               sspc = mean(ss) * 100)

# evaluate model (Purpose level)
fc |>
     filter(is_aggregated(State), !is_aggregated(Purpose)) |>
     accuracy(
          data = tourism_full,
          measures = list(rmse = RMSE, 
                          mase = MASE, 
                          ss = skill_score(CRPS))
     ) |>
     group_by(.model) |>
     summarise(rmse = mean(rmse), 
               mase = mean(mase), 
               sspc = mean(ss) * 100)

# ARIMA model
fit_ARIMA <- tourism_full |>
     filter(year(Quarter) <= 2015) |>
     model(base = ARIMA(Trips)) |>
     reconcile(
          bu = bottom_up(base),
          ols = min_trace(base, method = "ols"),
          mint = min_trace(base, method = "mint_shrink")
     )

# generate forecasts
fc <- fit_ARIMA |> forecast(h = "2 years")

# plot forecasts (State level)
fc |>
     filter(is_aggregated(Region), is_aggregated(Purpose)) |>
     autoplot(
          tourism_full |> filter(year(Quarter) >= 2011),
          level = NULL
     ) +
     labs(y = "Trips ('000)", 
          title = 'Forecasts of overnight trips for Australia and its states over the test period 2016Q1–2017Q4') +
     facet_wrap(vars(State), scales = "free_y")

# plot forecasts (Purpose level)
fc |>
     filter(is_aggregated(State), !is_aggregated(Purpose)) |>
     autoplot(
          tourism_full |> filter(year(Quarter) >= 2011),
          level = NULL
     ) +
     labs(y = "Trips ('000)", 
          title = 'Forecasts of overnight trips by purpose of travel over the test period 2016Q1–2017Q4. ') +
     facet_wrap(vars(Purpose), scales = "free_y")

# evaluate model (State level)
fc |>
     filter(is_aggregated(Region), is_aggregated(Purpose)) |>
     accuracy(
          data = tourism_full,
          measures = list(rmse = RMSE, 
                          mase = MASE, 
                          ss = skill_score(CRPS))
     ) |>
     group_by(.model) |>
     summarise(rmse = mean(rmse), 
               mase = mean(mase), 
               sspc = mean(ss) * 100)

# evaluate model (Purpose level)
fc |>
     filter(is_aggregated(State), !is_aggregated(Purpose)) |>
     accuracy(
          data = tourism_full,
          measures = list(rmse = RMSE, 
                          mase = MASE, 
                          ss = skill_score(CRPS))
     ) |>
     group_by(.model) |>
     summarise(rmse = mean(rmse), 
               mase = mean(mase), 
               sspc = mean(ss) * 100)
