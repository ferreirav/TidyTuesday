# TidyTuesday Training Series
# R Script for Week 43
# Data on Giant Pumpkins 

# Code Sourced from: https://juliasilge.com/blog/giant-pumpkins/
# Author: Julia Silge
# October 2021

#Load libraries-----------------------------------------------------------------
library(tidyverse)


# Load data---------------------------------------------------------------------

#tt_data <- tt_load("")

pumpkins_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv")

glimpse(pumpkins_raw)
head(pumpkins_raw, n = 20)

# Data Wrangling----------------------------------------------------------------
(pumpkins <- 
  pumpkins_raw %>% 
  separate(id, into = c("year", "type")) %>% 
   mutate(across(c(year, weight_lbs, ott, place), parse_number)) %>% 
   filter(type == "P") %>% 
   select(weight_lbs, year, place, ott, gpc_site, country))

pumpkins 

# First modelling and plotting--------------------------------------------------

(pumpkins %>% 
  filter(ott > 20, ott < 1e3) %>% 
   ggplot(aes(ott, weight_lbs, color = place)) +
   geom_point(alpha = 0.2, size = 1.1) +
   labs(x = "Over-The-Top Inches", y = "Weight (lbs)") +
   scale_color_viridis_c())

# Modellong relationships over time---------------------------------------------
# Has there been any shift in this relationship over time?

(pumpkins %>% 
    filter(ott > 20, ott < 1e3) %>% 
    ggplot(aes(ott, weight_lbs, color = place)) +
    geom_point(alpha = 0.2, size = 1.1, color = "gray60") +
    geom_smooth(aes(color = factor(year)),
                method = lm, formula = y ~ splines::bs(x, 3),
                # splines is a package for working with regressions splines
                se = FALSE, size = 1.5, alpha = 0.6) +
    labs(x = "Over-The-Top Inches", y = "Weight (lbs)", color = NULL) +
    scale_color_viridis_d())
    # It exists a difference in the color maps ending format [c() and d()]

# Comparing countries with the greatest pumpkins production---------------------
# Which countries did produce more or less massive pumpkins?

(pumpkins %>% 
   mutate(
     country = fct_lump(country, n = 10),
     country = fct_reorder(country, weight_lbs)
     ) %>% 
   ggplot(aes(country, weight_lbs, color = country)) +
   geom_boxplot(outlier.colour = NA) +
   geom_jitter(alpha = 0.1, width = 0.15) +
   labs(x = NULL, y = "Weight (lbs)",
        title = "How countries are producing massive pumpkins",
        caption = "Replicate made by Vitor Ferreira\nAuthor: Julia Silge.com") +
   theme_light() +
   theme(legend.position = "none",
         plot.subtitle = element_text(size = 15, color = "grey5", 
                                      margin = margin(b = 15), hjust = .5, 
                                      family = "Times New Roman", face = "bold"),
         plot.caption = element_text(color = "black", size = rel(.95), 
                                     family = "Times New Roman")))

# Continuation of Modelling using Machile Learning (ML) techniques--------------
# found on the webpage where this code is sourced

# Saving graphics---------------------------------------------------------------
getwd() # To confirm we are in TidyTuesday working folder
ggsave("./training_folder/2021_w43/2021_w43_2.png", width = 12.5, height = 15.5,
       device = "png")
