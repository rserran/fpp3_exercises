# Forecasting: Principles and Practice (3rd ed.) ----
## Chapter 5 - The Forecaster's Toolbox ----
## Source: https://otexts.com/fpp3/toolbox.html
## Source: Chapter 5 - Kevin K. R4DS session

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)
theme_set(theme_minimal())

### 5.1 - A tidy forecasting workflow ----

# global_economy GDP
gdppc <- global_economy %>% 
     mutate(GDP_per_capita = GDP / Population)

# plot gdppc
gdppc %>%
     filter(Country == "Sweden") %>%
     autoplot(GDP_per_capita) + 
     geom_line(size = 1) + 
     geom_smooth(method = 'loess', se = FALSE, color = 'steelblue') + 
     labs(y = "US Dollars", title = "GDP per capita for Sweden")

# define a model (example: linear trend model TSLM)
TSLM(GDP_per_capita ~ trend())

# train the model
fit <- gdppc %>% 
     model(trend_model = TSLM(GDP_per_capita ~ trend()))

fit

# forecast
fit %>% forecast(h = "3 years")

# GDP forecast plot for Sweden
fit %>%
     forecast(h = "3 years") %>%
     filter(Country == "Sweden") %>%
     autoplot(gdppc) + 
     labs(y = "US Dollars", title = "GDP per capita for Sweden")

### 5.2 - Some simple forecasting method ----

# Australia quarterly brick production
bricks <- aus_production %>%
     filter_index("1970 Q1" ~ "2004 Q4") %>%
     select(Bricks)

bricks

bricks %>% 
     autoplot() + 
     geom_line(size = 1) + 
     geom_smooth(method = 'loess', se = FALSE, color = 'steelblue') + 
     labs(y = 'Millions', title = 'Australia Quarterly CLay Bricks Production')

# STL decomposition method
dcmp <- bricks %>%
     model(stl = STL(Bricks))

components(dcmp)

# plot STL decomposition
components(dcmp) %>% 
     autoplot()

#### Mean method ----
fit_mean <- bricks %>% 
     model(MEAN(Bricks))

fit_mean %>% 
     forecast(h = "5 years") %>% 
     autoplot(bricks) + 
     labs(y = 'Millions', title = 'Australia Quarterly CLay Bricks Production', 
          subtitle = 'Mean method forecast')

#### Naive method ----
fit_naive <- bricks %>% 
     model(NAIVE(Bricks))

fit_naive %>% 
     forecast(h = "5 years") %>% 
     autoplot(bricks) + 
     labs(y = 'Millions', title = 'Australia Quarterly CLay Bricks Production', 
          subtitle = 'Naive method forecast')

#### Seasonal naive method ----
fit_snaive <- bricks %>% 
     model(SNAIVE(Bricks ~ lag('year')))

fit_snaive %>% 
     forecast(h = "5 years") %>% 
     autoplot(bricks) + 
     labs(y = 'Millions', title = 'Australia Quarterly CLay Bricks Production', 
          subtitle = 'Seasonal naive method forecast')

#### Drift method ----
fit_drift <- bricks %>% 
     model(RW(Bricks ~ drift()))

fit_drift %>% 
     forecast(h = "5 years") %>% 
     autoplot(bricks) + 
     labs(y = 'Millions', title = 'Australia Quarterly CLay Bricks Production', 
          subtitle = 'Drift method forecast')

#### Example: Australian quarterly beer production ----

# Set training data from 1992 to 2006
train <- aus_production %>%
     filter_index("1992 Q1" ~ "2006 Q4")

# Fit the models
beer_fit <- train %>%
     model(
          Mean = MEAN(Beer),
          `Naïve` = NAIVE(Beer),
          `Seasonal naïve` = SNAIVE(Beer)
     )

# set train/test datasets using rsample and timetk packages
library(timetk)
library(lubridate)
library(rsample)

# create 'data_tk' as tibble
data_tk <- aus_production %>% 
     mutate(date = as.Date(Quarter)) %>% 
     filter(date >= '1992-01-01') %>% 
     as_tibble() %>% 
     select(-Quarter) %>% 
     relocate(date, .before = everything())

# split 'data_tk' into train/test
split_tk <- time_series_split(
     data = data_tk, 
     initial = 60, # end date == '2006-10-01' (2006 Q4)
     assess = 14 # remainder use as testing
)

train_tk <- training(split_tk)
test_tk <- testing(split_tk)

# use modeltime package to fit models (minus the mean model)
library(modeltime)
library(parsnip)

# naive model
naive_mod <- naive_reg() %>% 
     set_engine('naive') %>% 
     fit(Beer ~ date, data = train_tk)

# snaive model
snaive_mod <- naive_reg() %>% 
     set_engine('snaive') %>% 
     fit(Beer ~ date, data = train_tk)

# models table
models_tbl <- modeltime_table(
     naive_mod, 
     snaive_mod
)

# Generate forecasts for 14 quarters
beer_fc <- beer_fit %>% forecast(h = 14)

# Plot forecasts against actual values
beer_fc %>%
     autoplot(train, level = NULL) +
     autolayer(
          filter_index(aus_production, "2007 Q1" ~ .),
          colour = "black"
     ) +
     labs(
          y = "Megalitres",
          title = "Forecasts for quarterly beer production"
     ) +
     guides(colour = guide_legend(title = "Forecast"))

# modeltime forecast
calibration_tbl <- models_tbl %>% 
     modeltime_calibrate(new_data = test_tk)

calibration_tbl %>% 
     modeltime_forecast(
          new_data = test_tk, 
          actual_data = data_tk
     ) %>% 
     plot_modeltime_forecast(
          .legend_max_width = 25,  # for mobile screens
          .interactive = TRUE
     )

#### Example: Google’s daily closing stock price ----

# Re-index based on trading days
google_stock <- gafa_stock %>%
     filter(Symbol == "GOOG", year(Date) >= 2015) %>%
     mutate(day = row_number()) %>%
     update_tsibble(index = day, regular = TRUE)

# Filter the year of interest
google_2015 <- google_stock %>% 
     filter(year(Date) == 2015)

# Fit the models
google_fit <- google_2015 %>%
     model(
          Mean = MEAN(Close),
          `Naïve` = NAIVE(Close),
          Drift = NAIVE(Close ~ drift())
     )

# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock %>%
     filter(yearmonth(Date) == yearmonth("2016 Jan"))

google_fc <- google_fit %>%
     forecast(new_data = google_jan_2016)

# Plot the forecasts
google_fc %>%
     autoplot(google_2015, level = NULL) +
     autolayer(google_jan_2016, Close, colour = "black") +
     labs(y = "$US",
          title = "Google daily closing stock prices",
          subtitle = "(Jan 2015 - Jan 2016)") +
     guides(colour = guide_legend(title = "Forecast"))

### 5.3 - Fitted values and residuals ----

# use 'augment()' to obtain fitted values and residuals
beer_res <- beer_fit %>% 
     augment()

beer_res

# Shapiro test for normality
shapiro.test(beer_res$.innov)

# qqplot
library(car)
qqPlot(beer_res$.innov)

### 5.4 - Residual diagnostics ----

google_2015 %>% 
     autoplot(Close) +
     labs(y = "$US",
          title = "Google daily closing stock prices in 2015")

# plot residuals
aug <- google_2015 %>%
     model(NAIVE(Close)) %>%
     augment()

aug %>% 
     autoplot(.innov) +
     labs(y = "$US",
          title = "Residuals from the naïve method")

# residuals histogram
aug %>%
     ggplot(aes(x = .innov)) +
     geom_histogram(fill = 'steelblue') +
     labs(title = "Histogram of residuals")

# Shapiro test for normality
shapiro.test(aug$.innov)

# qqplot
qqPlot(aug$.innov)

# ACF plot
aug %>%
     ACF(.innov) %>%
     autoplot() +
     labs(title = "Residuals from the naïve method")

# ggtsresiduals()
google_2015 %>%
     model(NAIVE(Close)) %>%
     gg_tsresiduals()

## Portmanteau tests for autocorrelation

aug %>% 
     features(.innov, box_pierce, lag = 10, dof = 0)

aug %>% 
     features(.innov, ljung_box, lag = 10, dof = 0)

# alternative approach
fit <- google_2015 %>% 
     model(RW(Close ~ drift()))

tidy(fit)

fit %>% 
     augment() %>% 
     features(.innov, ljung_box, lag=10, dof=1)
