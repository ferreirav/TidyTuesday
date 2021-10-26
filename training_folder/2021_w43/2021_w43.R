################################################################################
# TidyTuesday Training Series
# R Script for Week 43
# Data on Giant Pumpkins 

# Code Sourced from: https://gist.github.com/leeolney3/e64f898c55dc6a8612c364d054beca16
# Author: leeolney
# October 2021
################################################################################


#Load libraries-----------------------------------------------------------------
library(tidyverse)
library(ggtext)



# Load data---------------------------------------------------------------------

#tt_data <- tt_load("")

pumpkins <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv")

view(pumpkins)



# Data Wrangling----------------------------------------------------------------

(df <- pumpkins %>% 
    filter(place != "EXH") %>% 
    mutate(weight = parse_number(weight_lbs), place = as.numeric(place)) %>% 
    drop_na(place) %>% 
    mutate(id2 = id) %>% separate(id2, c("year", "type"), sep = "-") %>% 
    filter(place <= 100) %>% 
    filter(type %in% c("F", "P", "S")))


# Plotting data - ridge line  style---------------------------------------------

(df %>% 
  filter(type %in% c("L", "F", "W")) %>% 
   mutate(type = recode(type, "L" = "**Long Gourd**<br>(lenght in inches)",
                        "F" = "**Field Pumpkin**<br>(weight in pounds)",
                        "W" = "**Giant Watermelon**<br>(weight in pounds)")) %>% 
   ggplot(aes(y = type, x = weight, fill = factor(stat(quantile)))))
