# TidyTuesday Training Series
# R Script for Week 42
# Data on Global Seafood Production 

# Inspired By:
# Author: Abdoul Madjid
# October 2021

#Load libraries-----------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(ggforce)
library(ggtext)
library(scales)


# Load data---------------------------------------------------------------------

#tt_data <- tt_load("")

data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv")

# Data Wrangling----------------------------------------------------------------

captured_vs_farmed <- data %>% 
  rename(aquaculture = `Aquaculture production (metric tons)`,
         capture = `Capture fisheries production (metric tons)`)

main_entities <- c("China", "Japan", "European Union", "Peru", "Indonesia", 
                   "United States", "India", "Chile", "Norway", "Philippines", 
                   "Russia", "South Korea", "Thailand", "Africa Western and Central", 
                   "Middle East & North Africa")

# We will subset years from 2003 to 2018

culture_evolutions <- captured_vs_farmed %>% 
  filter(Entity %in% main_entities, between(Year, 2003, 2018)) %>% 
  arrange(Entity, Year) %>% 
  group_by(Entity) %>% 
  mutate(
    evolution = (aquaculture - lag(aquaculture)) / aquaculture,
    evolution = 100 * evolution
  ) %>% 
  filter(!is.na(evolution)) %>% 
  ungroup()


# Compute proportions of Aquaculture
# in each entity total production

(culture_proportions <- culture_evolutions %>% 
    group_by(Entity) %>% 
    summarise(
      overall = sum(aquaculture, capture),
      aquaculture_prop = sum(aquaculture) / overall
      ) %>% 
    mutate(
      overall = round(overall / 1000000, 2),
      Entity = fct_reorder(Entity, aquaculture_prop)
    ) %>% 
    arrange(Entity) %>% 
    mutate(index = row_number(),
           index = index*2) %>% 
    relocate(index)
    )


# Distribution of annual changes

(culture_evolutions <- culture_evolutions %>% 
    mutate(
      Entity = factor(Entity, levels = levels(culture_proportions$Entity)),
      # creates n groups with equal ranges
      evolution_progress = cut_number(evolution, 6) %>% 
      # Important to keep spaces of differents width
        factor(labels = c("Strongly\nDecrease", "", " ",
                          "  ", "   ", "Strongly\nIncrease"))
    ) %>% 
    arrange(Entity) %>% 
    group_by(Entity) %>% 
    mutate(index = group_indices(),
           index = index * 2) %>% 
    relocate(index))


# Graphics----------------------------------------------------------------------

ggplot() +
  geom_circle(data = culture_evolutions, aes(x0 = Year, y0 = index, r = 0.4995, 
                                             fill = fct_rev(evolution_progress)),
              size = 0.25, color = NA) +
  # Circles were added to the graph
  geom_text(data = culture_evolutions, aes(x = Year, y = index, 
                                           label = paste0(ifelse(evolution > 0, "+", ""), glue::glue("{round(evolution,1)}%"))), 
            size = 2, color = "white", family = "Times New Roman", fontface = "bold") +
  # Here we added the values inside the circles
  geom_richtext(data = culture_proportions, aes(y = index, label = glue::glue("<span>**{Entity}**</span><br><span style=\"color: grey25;\">**{overall}M Tons**</span>")),
                hjust = 1,  x = 2003, fill = NA, label.color = NA,
                lineheight = 0.95,
                family = "Times New Roman"
                ) +
  
  
  geom_rect(data = tibble(xmin = 2020, xmax = 2024, ))