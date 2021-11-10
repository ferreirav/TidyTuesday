# TidyTuesday Training Series
# R Script for Week 45
# Data on U.S. Population
# Code Sourced from: https://github.com/queeniel1/Tidy-Tuesday/blob/main/Week%2045
# Author: Queenie
# October 2021


# Load data---------------------------------------------------------------------

# https://github.com/rfordatascience/tidytuesday/blob/master/README.md

StatesPopulation <- read.csv("https://raw.githubusercontent.com/ds4stats/r-tutorials/master/intro-maps/data/StatePopulation.csv")

#Load libraries-----------------------------------------------------------------
library(tidyverse)
library(maps)
library(extrafont)

# Load fonts
font_import()
loadfonts(device = "pdf")
fonts()

# Load maps
States <- map_data("state")

# Data Exploration
view(StatesPopulation)
view(States)

MergedStates <- inner_join(States, StatesPopulation, by = "region")
Cities <- filter(us.cities, long >= -130)
Most_populated_cities <- Cities %>% 
  group_by(country.etc) %>% 
  filter(pop == max(pop))

# Plotting the data and maps

ggplot() + geom_polygon(data = States, aes(x = long, y = lat, group = group),
                        color = "black", fill = "darkgray")

g <- ggplot()
g <- g + geom_polygon(data = MergedStates,
                      aes(x = long, y = lat, group = group, fill = population/1000000),
                      color = "black", size = 0.2) +
  scale_fill_continuous(name = "State Population (M)", low = "#FFBDBD", high = "#850000",
                        limits = c(0,45), breaks = c(5,15,25,35,45),
                        na.value = "grey50") +
  labs(title = "Most Populated cities per State (in Millions)") +
  theme_minimal() +
  theme(text = element_text(family = "Impact", face = "bold", size = 12, color = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") +
  ylab("")
g

g <- g + geom_point(data = Most_populated_cities, aes(x = long, y = lat, size = pop/1000000),
                    color = "navy blue", alpha = 0.7) +
  scale_size(name = "Population in Cities (Millions)")
g


ggsave("./training_folder/2021_w45/States_most_populated_cities.png", g, width = 12, height = 6.1)
