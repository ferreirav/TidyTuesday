# TidyTuesday Training Series
# R Script for Week 43
# Data on Giant Pumpkins 

# Code Sourced from: https://gist.github.com/iago-pssjd/8596bb7e4284bb0bb180a6a576dd8e5b
# Author: iago-pssjd
# December 2021

#Load libraries-----------------------------------------------------------------
library(tidyverse)


# Load data---------------------------------------------------------------------

#tt_data <- tt_load("")

tuesdata <- tidytuesdayR::tt_load('2021-10-19')
tuesdata <- tidytuesdayR::tt_load(2021, week = 43)

data <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

# Manipulating and Plotting data

(data %>% 
    filter(!grepl("damaged", country)) %>% 
    mutate(across(c(weight_lbs, ott, est_weight, pct_chart), gsub, pattern = ",", replacement = ""),
             across(c(weight_lbs, ott, est_weight, pct_chart), as.numeric)) %>% 
    filter(!is.na(id)) %>% 
    with_groups(c(country, id), mutate, n = n()) %>% 
    filter(n >= 100) %>% 
    ggplot(aes(x = id, y = weight_lbs)) + # Aesthetic Mappings
    geom_jitter(size = 0.5, shape = 2, color = "brown") + # Jittering geoms
    geom_boxplot(color = "darkblue", fill = "lightblue", alpha = 0.75) + # Boxplot geoms
    geom_violin(color = "darkgreen", fill = "lightgreen", alpha = 0.25) + # Violin geoms
    facet_wrap(~country))

ggsave("./training_folder/2021_w43/2021_w43_3.png", plot = last_plot(), width = 18.6, height = 10)
