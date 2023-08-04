# Chapter 12 Exercise 3

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
theme_set(theme_bw())

# read `lynx` dataset
data("lynx")

lynx

# convert to tsibble
lynx_tbl <- lynx %>% 
     as_tsibble() %>% 
     rename(year = 1)

lynx_tbl |> 
     autoplot(value) + 
     geom_smooth(method = 'loess', se = FALSE, 
                 linewidth = 1.5, color = 'steelblue')

# fit NNETAR model
fit <- lynx_tbl |>
     model(NNETAR(sqrt(value)))

# forecast
fit |>
     forecast(h = 10) |>
     autoplot(lynx_tbl) +
     labs(x = "Year", 
          y = "Trappings", 
          title = "Yearly lynx trappings")
