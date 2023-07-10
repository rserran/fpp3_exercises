# Chapter 11 Exercise 1

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(timetk)
library(patchwork)
theme_set(theme_light())

# read PBS dataset
data(PBS)

PBS |> 
     mutate(date = ym(Month)) |> 
     select(date, Concession, Type, ATC1, ATC2, Scripts) |> 
     as_tibble() |> 
     skimr::skim()

# function to identify time series with `sd` == 0
time_series_mean_sd <- function(data) {
     
     data %>% 
          features(Scripts, features = list(mean = mean, sd = sd))
}

PBS %>% 
     time_series_mean_sd() %>% 
     filter(sd == 0)

# random noise to time series with `sd` == 0
PBS_non_zero <- PBS %>% 
     filter(Scripts == 0) %>% 
     mutate(Scripts = Scripts + rnorm(n = 6171, mean = 0.1, sd = 0.05))

PBS_sd <- PBS %>% 
     filter(!Scripts == 0) %>% 
     bind_rows(PBS_non_zero)

PBS_sd %>% 
     time_series_mean_sd() %>% 
     filter(sd== 0)

PBS_sd %>% 
     filter(Scripts == 0)

# Concession == 'General' & Type == 'Co-payments' & ATC1 == 'R' & ATC2 == 'R'

# aggregate `Scripts` by `(ATC1/ATC2) * Concession * Type`
pbs_hts <- PBS_sd |> 
     select(Month, Concession, Type, ATC1, ATC2, Scripts) |> 
     aggregate_key((ATC1/ATC2) * Concession * Type, Scripts = sum(Scripts))

pbs_hts %>% 
     filter(!is_aggregated(ATC2)) %>% View()

# plot total `Scripts` by `ATC2`
pbs_hts |> 
     # select(Month, Scripts) |> 
     mutate(date = ym(Month)) |> 
     filter(is_aggregated(Concession), !is_aggregated(ATC1), 
            !is_aggregated(ATC2), is_aggregated(Type)
     ) |> 
     as_tibble() |> 
     select(date, Scripts, ATC2) |> 
     mutate(ATC2 = ATC2 %>% as.factor) |> 
     plot_time_series(
          .date_var = date, 
          .value = Scripts, 
          .facet_vars = ATC2,
          # .facet_ncol = 4, 
          # .x_lab = NULL, 
          # .y_lab = 'Number of scripts ("000s")', 
          # .title = 'Australia PBS Scripts', 
          .trelliscope = TRUE
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
     mutate(Scripts = Scripts / 1e3) |> 
     model(ets = ETS(Scripts), 
           arima = ARIMA(Scripts), 
           snaive = SNAIVE(Scripts))

fc_total <- fit_total |> forecast(h = "3 years")

# plot observed and forecast
fc_total |> 
     autoplot(
          pbs_hts |> 
               filter(year(Month) >= 2000) |> 
               mutate(Scripts = Scripts / 1e3)
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
# using minT to reconcile ETS `ATC2` forecasts
pbs_hts_1 <- PBS_sd |> 
     select(Month, ATC1, ATC2, Concession, Type, Scripts) |> 
     aggregate_key(ATC1/ATC2 * Concession * Type, 
                   Scripts = sum(Scripts)
     )

pbs_hts_1 %>% 
     filter(!is_aggregated(Concession), !is_aggregated(Type), 
            !is_aggregated(ATC1), !is_aggregated(ATC2)) %>% 
     time_series_mean_sd() %>% 
     filter(sd == 0)

fit_ets_reconcile <- pbs_hts_1 |> 
     mutate(date = ym(Month)) |> 
     filter(date <= '2005-06-01') |> 
     select(-date) |> 
     model(ets = ETS(Scripts)) |>
     reconcile(
          bu = bottom_up(ets), 
          mint = min_trace(ets, method = "mint_shrink")
          )

fc_ets_reconcile <- fit_ets_reconcile |> forecast(h = "3 years")

# plot forecasts total Scripts
fc_ets_reconcile |> 
     filter(is_aggregated(Concession), is_aggregated(Type), 
            is_aggregated(ATC1)) |> 
     mutate(Scripts = Scripts / 1e3, 
            .mean = .mean / 1e3) |>
     autoplot(
          pbs_hts_1 |> 
               filter(year(Month) >= 2000) |> 
               mutate(Scripts = Scripts / 1e3)
     ) +
     labs(y = "Scripts ('000s)", 
          title = 'Forecasts for Australia PBS (2000 – 2008)') + 
     ylim(5000, 25000)

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
# .model   rmse  mase
# <chr>   <dbl> <dbl>
# 1 bu     42050.  1.66
# 2 ets    38934.  1.48
# 3 mint   57696.  2.09

# ARIMA ----
# using minT to reconcile ARIMA `ATC1` forecasts
fit_arima_reconcile <- pbs_hts_1 |> 
     mutate(date = ym(Month)) |> 
     filter(date <= '2005-06-01') |> 
     select(-date) |> 
     model(arima = ARIMA(Scripts)) |>
     reconcile(
          bu = bottom_up(arima), 
          mint = min_trace(arima, method = "mint_shrink")
     )

fc_arima_reconcile <- fit_arima_reconcile |> forecast(h = "3 years")

# plot forecasts total Scripts
fc_arima_reconcile |> 
     filter(is_aggregated(Concession), is_aggregated(Type), 
            is_aggregated(ATC1)) |> 
     mutate(Scripts = Scripts / 1e3, 
            .mean = .mean / 1e3) |>
     autoplot(
          pbs_hts_1 |> 
               filter(year(Month) >= 2000) |> 
               mutate(Scripts = Scripts / 1e3)
     ) +
     labs(y = "Scripts ('000s)", 
          title = 'Forecasts for Australia PBS (2000 – 2008)') + 
     ylim(5000, 20000)

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
# .model   rmse  mase
# <chr>   <dbl> <dbl>
# 1 arima  22616.  2.20
# 2 bu     22880.  2.28
# 3 mint   22349.  2.21

# SNAIVE ----
# using minT to reconcile ARIMA `ATC2` forecasts
fit_snaive_reconcile <- pbs_hts_1 |> 
     mutate(date = ym(Month)) |> 
     filter(date <= '2005-06-01') |> 
     select(-date) |> 
     model(snaive = SNAIVE(Scripts)) |>
     reconcile(
          bu = bottom_up(snaive), 
          mint = min_trace(snaive, method = "mint_shrink")
     )

fc_snaive_reconcile <- fit_snaive_reconcile |> forecast(h = "3 years")

# plot forecasts total Scripts
fc_snaive_reconcile |> 
     filter(is_aggregated(Concession), is_aggregated(Type), 
            is_aggregated(ATC1)) |> 
     mutate(Scripts = Scripts / 1e3, 
            .mean = .mean / 1e3) |>
     autoplot(
          pbs_hts_1 |> 
               filter(year(Month) >= 2000) |> 
               mutate(Scripts = Scripts / 1e3)
     ) +
     labs(y = "Scripts ('000s)", 
          title = 'Forecasts for Australia PBS (2000 – 2008)') + 
     ylim(5000, 20000)

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
# .model   rmse  mase
# <chr>   <dbl> <dbl>
# 1 bu     22285.  2.27
# 2 mint   22285.  2.27
# 3 snaive 22285.  2.27

# Conclusions:

# In the ETS model, no improvement resulted on the bottoms-up and MinT reconcilation
# compared to the base model.

# For the ARIMA model, MinT reconciliation is the best model for total Scripts forecasts, 
# according the rmse and mase metrics.

# In the SNAIVE model, the rmse and mase metrics are the same for the base model and
# the reconcilations methods. The reason for this results is that SNAIVE forecast 
# are constants, so the variance is zero (MinT minimizes the overall variance).
