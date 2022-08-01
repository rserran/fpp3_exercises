# Chapter 4.5 FPP3 - Principal Component Analysis

# load packages
suppressMessages(library(tidyverse))
library(fpp3)
theme_set(theme_minimal())

# Australia tourism dataset
tourism_features <- tourism %>% 
     features(Trips, feature_set(pkgs = "feasts"))

tourism_features

pca <- tourism_features %>%
     select(-State, -Region, -Purpose) %>%
     prcomp(scale = TRUE)

head(pca)

# compute total variance percent
variance = pca$sdev^2 / sum(pca$sdev^2)
variance

# convert variance to dataframe
pc <- c(1:length(variance))
df_variance <- data.frame(pc, variance)
df_variance

# create variance cumulative percentage column
df_variance <- df_variance %>% 
     mutate(cum_perc = cumsum(variance))


# Scree plot
# qplot(c(1:10), variance[1:10]) +
#      geom_line() +
#      geom_point(size=4)+
#      xlab("Principal Component") +
#      ylab("Variance Explained") +
#      ggtitle("Scree Plot") +
#      ylim(0, 1)

# Scree plot

df_variance %>% 
     ggplot(aes(pc, variance)) + 
     geom_col(color = 'black', fill = 'steelblue') + 
     # geom_point(size=4)+
     xlab("Principal Component") +
     ylab("Variance Explained") +
     ggtitle("Scree Plot") +
     ylim(0, 0.5) + 
     theme_minimal()

# Source: https://stackoverflow.com/questions/53716675/adjusting-the-second-y-axis-on-a-pareto-chart-in-r
library(ggQC)
df_variance %>% 
     ggplot(aes(pc, variance)) + 
     geom_bar(stat="identity") +
     theme(axis.text.x=element_text(angle=90,hjust=1)) + 
     stat_pareto(point.color = "red",   
                 point.size = 2,        
                 line.color = "black",  
                 #size.line = 1,        
                 bars.fill = c("steelblue", "orange")) + 
     theme_minimal() + 
     labs(x = 'PC', 
          title = 'Scree Plot')
