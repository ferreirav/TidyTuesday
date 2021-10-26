# TidyTuesday Training Series
# R Script for Week 43
# Data on Giant Pumpkins 

# Code Sourced from: https://gist.github.com/leeolney3/e64f898c55dc6a8612c364d054beca16
# Author: leeolney
# October 2021

#Load libraries-----------------------------------------------------------------
library(tidyverse)
library(ggtext)


# Load data---------------------------------------------------------------------

#tt_data <- tt_load("")

data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv")
