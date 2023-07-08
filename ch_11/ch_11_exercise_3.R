# Chapter 11 Exercise 3

# load packages
suppressMessages(library(tidyverse))
library(fpp3)

# read Australian prison dataset
prison <- read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv") %>% 
     as_tibble()

prison

# create `prison_gts`
prison_ts <- prison %>% 
     mutate(Quarter = yearquarter(Date)) |> 
     select(-Date)  |> 
     as_tsibble(key = c(Gender, Legal, State, Indigenous), 
                index = Quarter) |> 
     relocate(Quarter)

prison_gts <- prison_ts |> 
     aggregate_key(Gender * Legal * State, Count = sum(Count)/1e3)

# Forecasts for the total Australian quarterly adult prison population for the 
# period 2015Q1–2016Q4 (bootstrap = TRUE)
fit <- prison_gts |>
     filter(year(Quarter) <= 2014) |>
     model(base = ETS(Count)) |>
     reconcile(
          bottom_up = bottom_up(base),
          MinT = min_trace(base, method = "mint_shrink")
     )

fc <- fit |> forecast(h = 8, bootstrap = TRUE)

fc |>
     filter(is_aggregated(State), is_aggregated(Gender),
            is_aggregated(Legal)) |>
     autoplot(prison_gts, alpha = 0.7, level = 90) +
     labs(y = "Number of prisoners ('000)",
          title = "Australian prison population (total)")

# evaluate model
fc |>
     filter(is_aggregated(State), is_aggregated(Gender),
            is_aggregated(Legal)) |>
     accuracy(data = prison_gts,
              measures = list(mase = MASE,
                              ss = skill_score(CRPS)
              )
     ) |>
     group_by(.model) |>
     summarise(mase = mean(mase), sspc = mean(ss) * 100)

# A tibble: 3 × 3
# .model     mase  sspc
# <chr>     <dbl> <dbl>
# 1 MinT      0.908  71.7
# 2 base      1.74   55.4
# 3 bottom_up 1.84   32.0
