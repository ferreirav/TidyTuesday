# TidyTuesday Training Series
# R Script for Week 45
# Data on African Conflits 

# Code Sourced from: https://github.com/gkaramanis/tidytuesday/blob/master/2021/2021-week45/afrimapr.R
# Author: Georgios Karamanis
# November 2021


# Get african data
remotes::install_github("afrimapr/afrilearndata")


#Load libraries-----------------------------------------------------------------
library(afrilearndata)
library(tidyverse)
library(paletteer)
library(pals)
library(here)

getwd()

# Data

afripop_df <- afripop2020 %>% 
  as.data.frame(xy = T) %>% 
  rename(pop = 3) %>% 
  filter(!is.na(pop)) %>% 
  mutate(
    pop2 = case_when(
      pop <= 500 ~ pop,
      TRUE ~ 600
    )
  )

str(afripop_df)

f1 = "Porpora"
f2 = "Public Headline"

africa_plot <- ggplot(afripop_df) +
  geom_tile(aes(x, y, fill = pop2)) +
  annotate("text", -5, -5, label = "Africa", family = "Times New Roman",
           size = 22, fontface = "bold", color = "grey97") +
  scale_fill_gradientn(colours = viridis(100), breaks = seq(0, 600, 100), labels = c(seq(0, 500, 100), ">500")) +
  guides(fill = guide_colorbar(title = "Population density\n(people/km2)", label.position = "left", title.hjust = 0.5)) +
  labs(caption = "Data: afrilearndata - Inspired by Georgios Karamanis - Made by Vitor Ferreira") +
  coord_fixed() +
  theme_void(base_family = "Times New Roman", base_size = 14) +
  theme(
    legend.position = c(0.22, 0.27),
    legend.key.width = unit(0.5, "line"),
    legend.title = element_text(size = 16, margin = margin(0, 0, 10, -20), color = "grey85", lineheight = 1.1),
    legend.text = element_text(size = 11, color = "grey85"),
    plot.background = element_rect(fill = "#58507E", colour = NA),
    plot.caption = element_text(hjust = 0.5, colour = "grey80"),
    plot.margin = margin(10, 10, 10, 10)
  )

africa_plot

ggsave("./training_folder/2021_w45/africa_densities.png", device = "png")
