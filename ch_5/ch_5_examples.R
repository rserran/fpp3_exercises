# Forecasting: Principles and Practice (3rd ed.) ----
## Chapter 5 ----
## Source: https://otexts.com/fpp3/toolbox.html

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

# GDP forecast plot for Sewden
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

