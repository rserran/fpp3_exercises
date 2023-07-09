# Chapter 11 Exercise 1

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(timetk)
library(patchwork)
theme_set(theme_light())

# read PBS dataset
PBS

PBS |> 
     mutate(date = ym(Month)) |> 
     select(date, Concession, Type, ATC1, ATC2, Scripts) |> 
     as_tibble() |> 
     skimr::skim()

# aggregate `Scripts` by `(ATC1/ATC2) * Concession * Type`
pbs_hts <- PBS |> 
     select(Month, Concession, Type, ATC1, ATC2, Scripts) |> 
     aggregate_key((ATC1/ATC2) * Concession * Type, Scripts = sum(Scripts / 1e3))

pbs_hts %>% 
     filter(!is_aggregated(ATC1)) %>% View()

# plot total `Scripts` by `ATC1`
pbs_hts |> 
     select(Month, Scripts) |> 
     mutate(date = ym(Month)) |> 
     filter(is_aggregated(Concession), is_aggregated(ATC1), 
            is_aggregated(ATC2), is_aggregated(Type)
     ) |> 
     plot_time_series(
          .date_var = date, 
          .value = Scripts, 
          # .facet_vars = ATC1, 
          # .facet_ncol = 4, 
          .x_lab = NULL, 
          .y_lab = 'Number of scripts ("000s")', 
          .title = 'Australia PBS Scripts Total'
     )

# plot `Scripts` by `Concession` 
p1 <- pbs_hts %>% 
     filter(!is_aggregated(Concession), is_aggregated(ATC1), 
            is_aggregated(ATC2), is_aggregated(Type)) |>
     autoplot(Scripts) +
     labs(y = "Number of scripts", 
          title = 'Concession', 
          fill = 'Concession') + 
     theme(plot.title = element_text(hjust = 0.5))

# plot `Scripts` by `Type` 
p2 <- pbs_hts %>% 
     filter(is_aggregated(Concession), is_aggregated(ATC1), 
            is_aggregated(ATC2), !is_aggregated(Type)) |>
     autoplot(Scripts) +
     labs(y = "Number of scripts", 
          title = 'Type', 
          fill = 'Type') + 
     theme(plot.title = element_text(hjust = 0.5))

# plot `Scripts` by `ATC1` 
p3 <- pbs_hts %>% 
     filter(is_aggregated(Concession), !is_aggregated(ATC1), 
            is_aggregated(ATC2), is_aggregated(Type)) |>
     autoplot(Scripts) +
     labs(y = "Number of scripts", 
          title = 'ATC1', 
          fill = 'ATC1') + 
     theme(plot.title = element_text(hjust = 0.5))

p1 + p2 + p3 + plot_layout(guides = 'collect')

# forecast total `Scripts` using ETS, ARIMA, SNAIVE models
fit_total <- pbs_hts |> 
     mutate(date = ym(Month)) |> 
     filter(date <= '2005-06-01') |> 
     select(-date) |> 
     filter(is_aggregated(Concession), is_aggregated(ATC1), 
            is_aggregated(ATC2), is_aggregated(Type)
            ) |> 
     model(ets = ETS(Scripts), 
           arima = ARIMA(Scripts), 
           snaive = SNAIVE(Scripts))

fc_total <- fit_total |> forecast(h = "3 years")

# plot observed and forecast
fc_total |> 
     autoplot(
          pbs_hts |> filter(year(Month) >= 2000)
     )

# evaluate models
fc_total |> 
     accuracy(
          data = pbs_hts,
          measures = list(rmse = RMSE, 
                          mase = MASE
                          )
     ) |>
     group_by(.model) |>
     summarise(rmse = mean(rmse), 
               mase = mean(mase)
               )

# A tibble: 3 × 3
# .model  rmse  mase
# <chr>  <dbl> <dbl>
# 1 arima  1273.  1.52
# 2 ets    1006.  1.21
# 3 snaive 1013.  1.29

# ETS model is the best for all three metrics

# ETS ----
# using minT to reconcile ETS `ATC1` forecasts
pbs_hts_1 <- PBS |> 
     select(Month, ATC1, Concession, Type, Scripts) |> 
     aggregate_key(ATC1 * Concession * Type, 
                   Scripts = sum(Scripts / 1e3)
     )

fit_ets_reconcile <- pbs_hts_1 |> 
     mutate(date = ym(Month)) |> 
     filter(date <= '2005-06-01') |> 
     select(-date) |> 
     model(ets = ETS(Scripts)) |>
     reconcile(
          bu = bottom_up(ets), 
          mint = min_trace(ets), method = "mint_shrink"
          )

fc_ets_reconcile <- fit_ets_reconcile |> forecast(h = "3 years")

# plot forecasts by `ATC1`
fc_ets_reconcile |> 
     filter(is_aggregated(Concession), is_aggregated(Type)) |> 
     autoplot(
          pbs_hts_1 |> filter(year(Month) >= 2000)
     ) +
     labs(y = "Scripts ('000)", 
          title = 'Forecasts of ATC1 for Australia PBS (2000 – 2008)') +
     facet_wrap(vars(ATC1), scales = "free_y")

# evaluate models
fc_ets_reconcile |> 
     filter(is_aggregated(Concession), is_aggregated(Type)) |> 
     accuracy(
          data = pbs_hts_1,
          measures = list(rmse = RMSE, 
                          mase = MASE
                          )
     ) |>
     group_by(.model) |>
     summarise(rmse = mean(rmse), 
               mase = mean(mase)
               )

# A tibble: 3 × 3
# .model  rmse  mase
# <chr>  <dbl> <dbl>
# 1 bu      172.  1.83
# 2 ets     146.  1.71
# 3 mint    153.  1.31

# ARIMA ----
# using minT to reconcile ARIMA `ATC1` forecasts
fit_arima_reconcile <- pbs_hts_1 |> 
     mutate(date = ym(Month)) |> 
     filter(date <= '2005-06-01') |> 
     select(-date) |> 
     model(arima = ARIMA(Scripts)) |>
     reconcile(
          bu = bottom_up(arima), 
          mint = min_trace(arima), method = "mint_shrink"
     )

fc_arima_reconcile <- fit_arima_reconcile |> forecast(h = "3 years")

# plot forecasts by `ATC1`
fc_arima_reconcile |> 
     filter(is_aggregated(Concession), is_aggregated(Type)) |> 
     autoplot(
          pbs_hts_1 |> filter(year(Month) >= 2000)
     ) +
     labs(y = "Scripts ('000)", 
          title = 'Forecasts of ATC1 for Australia PBS (2000 – 2008)') +
     facet_wrap(vars(ATC1), scales = "free_y")

# evaluate models
fc_arima_reconcile |> 
     accuracy(
          data = pbs_hts_1,
          measures = list(rmse = RMSE, 
                          mase = MASE
                          )
     ) |>
     group_by(.model) |>
     summarise(rmse = mean(rmse), 
               mase = mean(mase)
               )

# A tibble: 3 × 3
# .model  rmse  mase
# <chr>  <dbl> <dbl>
# 1 arima   87.7  1.72
# 2 bu      87.0  1.70
# 3 mint    85.1  1.68

# SNAIVE ----
# using minT to reconcile ARIMA `ATC1` forecasts
fit_snaive_reconcile <- pbs_hts_1 |> 
     mutate(date = ym(Month)) |> 
     filter(date <= '2005-06-01') |> 
     select(-date) |> 
     model(snaive = SNAIVE(Scripts)) |>
     reconcile(
          bu = bottom_up(snaive), 
          mint = min_trace(snaive), method = "mint_shrink"
     )

fc_snaive_reconcile <- fit_snaive_reconcile |> forecast(h = "3 years")

# plot forecasts by `ATC1`
fc_snaive_reconcile |> 
     filter(is_aggregated(Concession), is_aggregated(Type)) |> 
     autoplot(
          pbs_hts_1 |> filter(year(Month) >= 2000)
     ) +
     labs(y = "Scripts ('000)", 
          title = 'Forecasts of ATC1 for Australia PBS (2000 – 2008)') +
     facet_wrap(vars(ATC1), scales = "free_y")

# evaluate models
fc_snaive_reconcile |> 
     accuracy(
          data = pbs_hts_1,
          measures = list(rmse = RMSE, 
                          mase = MASE
                          )
     ) |>
     group_by(.model) |>
     summarise(rmse = mean(rmse), 
               mase = mean(mase)
               )

# A tibble: 3 × 3
# .model  rmse  mase
# <chr>  <dbl> <dbl>
# 1 bu      79.3  1.92
# 2 mint    79.3  1.92
# 3 snaive  79.3  1.92

# ETS with MinT reconciliation is the best model for the `ATC1` forecasts, 
# according the mase metric.

# in general. minT reconciliation improves the base (model) forecasts, except in the
# SNAIVE model.