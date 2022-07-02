# Forecasting: Principles and Practice (3rd ed.)
# Chapter 3 - Time Series Decomposition

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)

# 3.1 - Transformations and adjustments (examples)

# global_economy dataset
data("global_economy")

g1 <- global_economy %>%
     filter(Country == "Australia") %>%
     autoplot(GDP/Population) + 
     theme_minimal() + 
     labs(title= "GDP per capita", y = "$US")

ggplotly(g1)

# aus_retail
data("aus_retail")

print_retail <- aus_retail %>%
     filter(Industry == "Newspaper and book retailing") %>%
     group_by(Industry) %>%
     index_by(Year = year(Month)) %>%
     summarise(Turnover = sum(Turnover))

aus_economy <- global_economy %>%
     filter(Code == "AUS")

g2 <- print_retail %>%
     left_join(aus_economy, by = "Year") %>%
     mutate(Adjusted_turnover = Turnover / CPI * 100) %>%
     pivot_longer(c(Turnover, Adjusted_turnover),
                  values_to = "Turnover") %>%
     mutate(name = factor(name,
                          levels=c("Turnover","Adjusted_turnover"))) %>% 
     ggplot(aes(x = Year, y = Turnover)) +
     geom_line() +
     facet_grid(name ~ ., scales = "free_y") + 
     theme_minimal() + 
     labs(title = "Turnover: Australian print media industry",
          y = "$AU")

ggplotly(g2)

# food (aus_retail) - mathematical transformations
food <- aus_retail %>%
     filter(Industry == "Food retailing") %>%
     summarise(Turnover = sum(Turnover))

food

food %>% autoplot(Turnover) +
     labs(y = "Turnover ($AUD)")

# square root transformation
food %>% autoplot(sqrt(Turnover)) +
     labs(y = "Square root turnover")

# log transformation
food %>% autoplot(log(Turnover)) +
     labs(y = "Log turnover")

# cube root transformation
food %>% autoplot(Turnover^(1/3)) +
     labs(y = "Cube root turnover")

# Box-Cox transformation
lambda <- food %>%
     features(Turnover, features = guerrero) %>% 
     pull(lambda_guerrero)

lambda

food %>% autoplot(box_cox(Turnover, lambda)) +
     labs(y = "Box-Cox transformed turnover")

# 3.2 - Time series components (examples)

# us_employment
data("us_employment")

us_retail_employment <- us_employment %>%
     filter(year(Month) >= 1990, Title == "Retail Trade") %>%
     select(-Series_ID)

us_retail_employment

g3 <- autoplot(us_retail_employment, Employed) + 
     theme_minimal() + 
     labs(y = "Persons (thousands)",
          title = "Total employment in US retail")

ggplotly(g3)

# STL decomposition method
dcmp <- us_retail_employment %>%
     model(stl = STL(Employed))

components(dcmp)

# plot trend component
components(dcmp) %>%
     as_tsibble() %>%
     autoplot(Employed, colour="gray") +
     geom_line(aes(y=trend), colour = "#D55E00") + 
     theme_minimal() + 
     labs(
          y = "Persons (thousands)",
          title = "Total employment in US retail"
     )

# plot STL decomposition
components(dcmp) %>% 
     autoplot()

# seasonally adjusted data
components(dcmp) %>%
     as_tsibble() %>%
     autoplot(Employed, colour = "gray") +
     geom_line(aes(y=season_adjust), colour = "#0072B2") +
     labs(y = "Persons (thousands)",
          title = "Total employment in US retail")

# seasonality by month
components(dcmp) %>% 
     gg_subseries(season_year)

# 3.3 - Moving Averages

global_economy %>%
     filter(Country == "Australia") %>%
     autoplot(Exports) +
     labs(y = "% of GDP", title = "Total Australian exports")

# 5-MA (five period moving average)
aus_exports <- global_economy %>%
     filter(Country == "Australia") %>%
     mutate(
          `5-MA` = slider::slide_dbl(Exports, mean,
                                     .before = 2, .after = 2, .complete = TRUE)
     )

aus_exports

# plot moving average
g4 <- aus_exports %>%
     autoplot(Exports) +
     geom_line(aes(y = `5-MA`), colour = "#D55E00") + 
     theme_minimal() + 
     labs(y = "% of GDP",
          title = "Total Australian exports") +
     guides(colour = guide_legend(title = "series"))

ggplotly(g4)

# 3.5 - Methods used by official statistics agencies

# X-11 method (multiplicative decomposition)
library(seasonal)
x11_dcmp <- us_retail_employment %>%
     model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) %>%
     components()

autoplot(x11_dcmp) + 
     theme_minimal() + 
     labs(title =
               "Decomposition of total US retail employment using X-11.")

# trend-cycle and seasonally adjusted components
x11_dcmp %>%
     ggplot(aes(x = Month)) +
     geom_line(aes(y = Employed, colour = "Data")) +
     geom_line(aes(y = season_adjust,
                   colour = "Seasonally Adjusted")) +
     geom_line(aes(y = trend, colour = "Trend")) +
     labs(y = "Persons (thousands)",
          title = "Total employment in US retail") +
     scale_colour_manual(
          values = c("gray", "#0072B2", "#D55E00"),
          breaks = c("Data", "Seasonally Adjusted", "Trend")
     ) + 
     theme_minimal()

# seasonality by month
x11_dcmp %>%
     gg_subseries(seasonal)

# SEATS method

seats_dcmp <- us_retail_employment %>%
     model(seats = X_13ARIMA_SEATS(Employed ~ seats())) %>%
     components()

autoplot(seats_dcmp) + 
     theme_minimal() + 
     labs(title =
               "Decomposition of total US retail employment using SEATS")
