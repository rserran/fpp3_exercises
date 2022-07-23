# Forecasting: Principles and Practice (3rd ed.) ----
## Chapter 5 ----
## Source: https://otexts.com/fpp3/toolbox.html

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)
theme_set(theme_minimal())

### 5.5 - Distributional forecasts and prediction intervals ----

# prediction intervals

# Re-index based on trading days
google_stock <- gafa_stock %>%
     filter(Symbol == "GOOG", year(Date) >= 2015) %>%
     mutate(day = row_number()) %>%
     update_tsibble(index = day, regular = TRUE)

google_jan_2016 <- google_stock %>%
     filter(yearmonth(Date) == yearmonth("2016 Jan"))

# Filter the year of interest
google_2015 <- google_stock %>% 
     filter(year(Date) == 2015)

google_2015 %>%
     model(NAIVE(Close)) %>%
     forecast(h = 10) %>%
     hilo()

# plot forecast with prediction intervals
google_2015 %>%
     model(NAIVE(Close)) %>%
     forecast(h = 10) %>%
     autoplot(google_2015) +
     labs(title="Google daily closing stock price", y="$US" )

# prediction intervals from bootstrapped residuals  
fit <- google_2015 %>% 
     model(NAIVE(Close))

sim <- fit %>% generate(h = 30, times = 5, bootstrap = TRUE)
sim

# plot
b1 <- google_2015 %>%
     ggplot(aes(x = day)) +
     geom_line(aes(y = Close)) +
     geom_line(aes(y = .sim, colour = as.factor(.rep)),
               data = sim) +
     labs(title="Google daily closing stock price", y="$US" ) +
     guides(colour = "none")

ggplotly(b1)

# bootstrapped prediction interval
fc <- fit %>% 
     forecast(h = 30, bootstrap = TRUE)

fc

# plot fc
fc %>% 
     autoplot(google_2015) + 
     labs(title="Google daily closing stock price", y="$US")

# 'times' argument to control number of bootstrapped samples
google_2015 %>%
     model(NAIVE(Close)) %>%
     forecast(h = 10, bootstrap = TRUE, times = 1000) %>%
     hilo()

### 5.6 - Forecasting using transformations ----

# bias adjustments
prices %>%
     filter(!is.na(eggs)) %>%
     model(RW(log(eggs) ~ drift())) %>%
     forecast(h = 50) %>%
     autoplot(prices %>% 
                   filter(!is.na(eggs)),
              level = 80, point_forecast = lst(mean, median)
     ) +
     labs(title = "Annual egg prices",
          y = "$US (in cents adjusted for inflation) ")

### 5.7 - Forecasting with decomposition ----

#### Example: Employment in the US retail sector ----

us_retail_employment <- us_employment %>%
     filter(year(Month) >= 1990, Title == "Retail Trade")

dcmp <- us_retail_employment %>%
     model(STL(Employed ~ trend(window = 7), robust = TRUE)) %>%
     components() %>%
     select(-.model)

dcmp %>%
     model(NAIVE(season_adjust)) %>%
     forecast() %>%
     autoplot(dcmp) +
     labs(y = "Number of people",
          title = "US retail employment")

# using 'decomposition_model()'
fit_dcmp <- us_retail_employment %>%
     model(stlf = decomposition_model(
          STL(Employed ~ trend(window = 7), robust = TRUE),
          NAIVE(season_adjust)
          )
     )

fit_dcmp %>%
     forecast() %>%
     autoplot(us_retail_employment)+
     labs(y = "Number of people",
          title = "US retail employment")

# residual diagnostics
fit_dcmp %>% 
     gg_tsresiduals()

# Shapiro test for normality
shapiro.test(fit_dcmp$stlf[[1]][[1]][[1]][[1]]$.resid)

# qqplot
library(car)
qqPlot(fit_dcmp$stlf[[1]][[1]][[1]][[1]]$.resid)

### 5.8 - Evaluating point Forecast accuracy ----

#### Functions to subset a time series ----
aus_production %>% 
     filter(year(Quarter) >= 1995)

# equivalent
aus_production %>% 
     filter_index("1995 Q1" ~ .)

# slice()
aus_production %>%
     slice(n()-19:0)

# subsetting first twelve observations from each group
aus_retail %>%
     group_by(State, Industry) %>%
     slice(1:12)

#### Forecast errors ----
# Examples
recent_production <- aus_production %>%
     filter(year(Quarter) >= 1992)

# skim 'recent_production'
recent_production %>% 
     as_tibble() %>% 
     mutate(Quarter = as.Date(Quarter)) %>% 
     skimr::skim()

beer_train <- recent_production %>%
     filter(year(Quarter) <= 2007)

beer_fit <- beer_train %>%
     model(
          Mean = MEAN(Beer),
          `Naïve` = NAIVE(Beer),
          `Seasonal naïve` = SNAIVE(Beer),
          Drift = RW(Beer ~ drift())
     )

beer_fc <- beer_fit %>%
     forecast(h = 10)

beer_fc %>%
     autoplot(
          aus_production %>% filter(year(Quarter) >= 1992),
          level = NULL
     ) +
     labs(
          y = "Megalitres",
          title = "Forecasts for quarterly beer production"
     ) +
     guides(colour = guide_legend(title = "Forecast"))

# metrics
accuracy(beer_fc, recent_production) %>% 
     select(.model, RMSE, MAE, MAPE, MASE)

# non-seasonal example
google_fit <- google_2015 %>%
     model(
          Mean = MEAN(Close),
          `Naïve` = NAIVE(Close),
          Drift = RW(Close ~ drift())
     )

google_fc <- google_fit %>%
     forecast(google_jan_2016)

google_fc %>%
     autoplot(bind_rows(google_2015, google_jan_2016),
              level = NULL) +
     labs(y = "$US",
          title = "Google closing stock prices from Jan 2015") +
     guides(colour = guide_legend(title = "Forecast"))

# metrics
accuracy(google_fc, google_stock) %>% 
     select(.model, RMSE, MAE, MAPE, MASE)

### 5.9 - Evaluating distributional forecast accuracy ----

# quantile scores
google_fc %>%
     filter(.model == "Naïve") %>%
     autoplot(bind_rows(google_2015, google_jan_2016), level=80)+
     labs(y = "$US",
          title = "Google closing stock prices")

# accuracy with 'quantile_score()' function
google_fc %>%
     filter(.model == "Naïve", Date == "2016-01-04") %>%
     accuracy(google_stock, list(qs=quantile_score), probs=0.10)

# Winkler score
google_fc %>%
     filter(.model == "Naïve", Date == "2016-01-04") %>%
     accuracy(google_stock,
              list(winkler = winkler_score), level = 80)

# continuous ranked probability score (CRPS)
google_fc %>%
     accuracy(google_stock, list(crps = CRPS))

# scale-free comparisons
google_fc %>%
     accuracy(google_stock, list(skill = skill_score(CRPS)))

### 5.10 - Time series cross-validation ----

# Time series cross-validation accuracy
google_2015_tr <- google_2015 %>%
     stretch_tsibble(.init = 3, .step = 1) %>%
     relocate(Date, Symbol, .id)

google_2015_tr

# TSCV accuracy
google_2015_tr %>%
     model(RW(Close ~ drift())) %>%
     forecast(h = 1) %>%
     accuracy(google_2015) %>% 
     select(.model, RMSE, MAE, MAPE, MASE)

# Training set accuracy
google_2015 %>%
     model(RW(Close ~ drift())) %>%
     accuracy() %>% 
     select(.model, RMSE, MAE, MAPE, MASE)

#### Example: Forecast horizon accuracy with cross-validation ----

google_2015_tr <- google_2015 %>%
     stretch_tsibble(.init = 3, .step = 1)

fc <- google_2015_tr %>%
     model(RW(Close ~ drift())) %>%
     forecast(h = 8) %>%
     group_by(.id) %>%
     mutate(h = row_number()) %>%
     ungroup() %>%
     as_fable(response = "Close", distribution = Close)

fc %>%
     accuracy(google_2015, by = c("h", ".model")) %>%
     ggplot(aes(x = h, y = RMSE)) +
     geom_point()
