# Forecasting: Principles and Practice (3rd ed.)
# Chapter 8
# Source: https://otexts.com/fpp3/expsmooth.html

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
library(plotly)
theme_set(theme_minimal())

# algeria_economy
algeria_economy <- global_economy %>%
     filter(Country == "Algeria")

algeria_economy %>%
     autoplot(Exports) + 
     geom_smooth(method = 'loess', se = FALSE, color = 'blue') + 
     expand_limits(y = c(0, 60)) + 
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

# Example: Algerian exports

# Estimate parameters
fit <- algeria_economy %>%
     model(ETS(Exports ~ error("A") + trend("N") + season("N")))

fc <- fit %>%
     forecast(h = 5)

fc

# plot forecast
fc %>%
     autoplot(algeria_economy) +
     geom_line(aes(y = .fitted), col="#D55E00",
               data = augment(fit)) +
     labs(y="% of GDP", title="Exports: Algeria") +
     guides(colour = "none")

