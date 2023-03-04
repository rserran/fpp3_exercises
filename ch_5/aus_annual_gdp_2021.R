# Australia GPD

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
theme_set(theme_minimal())

# Australia Annual GDP from `tsibbledata`
aus_annual_gdp <- global_economy %>% 
     filter(Country == 'Australia') %>% 
     select(Year, GDP, Growth) %>% 
     as_tibble()

aus_annual_gdp

# FRED - Australia Quarterly GDP (non-seasonal) 1960 - 2022
aus_quarterly_gdp <- read_csv('../../Downloads/NGDPRNSAXDCAUQ.csv') %>% 
     rename(date = 1, 
            GDP = 2)

aus_quarterly_gdp

# convert to yearly gdp figures
aus_quarterly_gdp %>% 
     group_by(year(date)) %>% 
     summarise(GDP = sum(GDP), .groups = 'drop') %>% 
     rename(year = 1)

# Australia Annual GDP (World Bank Data) 1960 - 2021
aus_annual_gdp_2021 <- read_csv('../../Downloads/austrialia_annual_gdp.csv')

aus_annual_gdp_2021

# calculate annual growth
aus_annual_gdp_growth <- aus_annual_gdp_2021 %>% 
     mutate(Australia_GDP_lag1 = lag(Australia_GDP), 
            Annual_growth = ((Australia_GDP - Australia_GDP_lag1) / Australia_GDP_lag1) * 100) %>% 
     select(-Australia_GDP_lag1)

aus_annual_gdp_growth %>% 
     tail(10)

# plot annual growth
aus_annual_gdp_growth %>% 
     ggplot(aes(Year, Annual_growth)) + 
     geom_line(linewidth = 0.5)
