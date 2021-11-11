# TidyTuesday Training Series
# R Script for Week 45
# Data on African Conflits 

# Code Sourced from: https://github.com/marthaluka/TidyTuesday/blob/main/30DayMapChallenge/conflict_in_Africa.R
# Author: Martha Luka
# November 2021

#Load libraries-----------------------------------------------------------------

install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
# install.packages("pacman") # In case need to be installed 
require("pacman")
pacman::p_load(spData, spDataLarge, sf, tidyverse, janitor, ggrepel, viridis, hrbrthemes,
               RColorBrewer, ggtext, cowplot, patchwork)
