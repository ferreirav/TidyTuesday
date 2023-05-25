# TidyTuesday Training Series
# R Script for Week 24/2022
# Data on US Droughts

# Code Sourced from: https://github.com/gilbertfontana/TidyTuesday/blob/main/Week24/Week24.R
# Author: gilbertfontana
# June 2022

#Load libraries-----------------------------------------------------------------
library(tidyverse)
library(janitor)
library(scico)
library(showtext)
library(lubridate)
library(zoo)
library(maps)
library(gganimate)
library(magick)
library(transformr)
  
# Load data---------------------------------------------------------------------

#tt_data <- tt_load("")

data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv")


# Wrangling Data

drought_fips <- data %>% 
  filter(State == "CA") %>% 
  mutate(date = as.yearqtr(date),
         date2 = as.Date(date)) %>% 
  group_by(State, FIPS, date, date2) %>% 
  summarise(avg_dsci = mean(DSCI)) %>% 
  ungroup()


df_fips <- county.fips %>% 
  as_tibble() %>%
  extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$") %>% 
  mutate(fips = as.character(fips)) %>% 
  mutate(
    fips = 
      case_when(
        str_length(fips) < 5 ~ paste0("0", fips),
        TRUE ~ fips
      )
  )


df <- drought_fips %>% 
  clean_names() %>% 
  left_join()


