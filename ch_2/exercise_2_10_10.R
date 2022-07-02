# FPP3

# Chapter 2 - Time Series Graphics

# Exercise 2.10.10

# load packages
suppressMessages(library(tidyverse))
library(fpp3)

# air passengers
AirPassengers

AirPassengers %>% 
     as_tsibble() %>% 
     autoplot()

AirPassengers %>% 
     as_tsibble() %>% 
     ACF() %>% 
     autoplot()

# Answer: 3.D

# mink trappings
pelt

pelt %>% 
     autoplot(Lynx)

pelt %>% 
     ACF(Lynx) %>% 
     autoplot()

# Answer: 4.C

# Answer: 1.B

# Answer: 2.A
