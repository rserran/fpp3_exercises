# Forecasting: Principles and Practice (3rd ed.)
# Chapter 8
# Source: https://otexts.com/fpp3/expsmooth.html

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)

# algeria_economy
algeria_economy <- global_economy %>%
     filter(Country == "Algeria")

algeria_economy %>%
     autoplot(Exports) + 
     theme_minimal() + 
     labs(y = "% of GDP", title = "Exports: Algeria")

# STL decomposition method
dcmp <- algeria_economy %>%
     model(stl = STL(Exports))

components(dcmp)

# plot STL decomposition
components(dcmp) %>% 
     autoplot()

# Comments: Time series does not have a seasonal component. No obvious trend
# pattern is illustrated.
