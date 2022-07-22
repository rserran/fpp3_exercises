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

#### Example: Employment in the US retail sector

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

