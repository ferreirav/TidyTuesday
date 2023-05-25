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


# Wrangling Data----------------------------------------------------------------


drought_fips <- data %>% 
  filter(State == "CA") %>% 
  mutate(date_test = as.yearqtr(date),
         date2 = as.Date(date)) %>% 
  group_by(State, FIPS, date_test, date2) %>% 
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
  left_join(
    df_fips)


df2 <- df %>% 
  left_join(
    map_data("county")) %>% 
  as_tibble()


# Defining fonts and other features---------------------------------------------

font <- "Saira Extra Condensed"
font_add_google(family = font, font)
showtext_auto(enable = T)
theme_set(theme_minimal(base_family = font))
bg <- "ivory"
txt_col <- "grey20"



# Plotting----------------------------------------------------------------------


ggplot(data = df2, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = avg_dsci), color = txt_col, size = .2) +
  scale_fill_scico(palette = "lajolla",
                   name = "Drought Level",
                   breaks = c(25, 475),
                   limits = c(0, 500),
                   labels = c("low", "high"),
                   begin = .2,
                   end = .8) +
  labs(
    title = "Droughts in California",
    subtitle = "{closest_state}",
    caption = "Vitor Ferreira | Original Workviz from Gilbert Fontana | #TidyTuesday Week 24
    \nData: National Integrated Drought Information System") +
  coord_map(clip = "off") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0, size = 28, color = txt_col, lineheight = .8,
                              face = "bold", margin = margin(0,0,0,0)),
    plot.subtitle = element_text(hjust = 0, size = 16, color = txt_col, 
                                 margin = margin(10, 0, 20, 0)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = .5, margin = margin(20,0,0,0), size = 12,
                                colour = txt_col, face = "bold"),
    plot.background = element_rect(color = bg, fill = bg),
    plot.margin = margin(30,30,30,30),
    legend.position = c(.95, .75),
    legend.title = element_text(colour = txt_col, size = 12, face = "bold"),
    legend.text = element_text(color = txt_col, size = 12),
    legend.direction = "horizontal") +
  guides(fill = guide_colorbar(ticks.colour = NA, title.position = "top", title.hjust = .5)) +
  transition_states(df2$date_test)

### !!!Some error between this two steps as could not produce the gift image!!! ###

mapGIF <- animate(ani, height = 700, width = 700, fps = 20, duration = 20, bg = bg)

anim_save("./training_folder/2022_w24/week_24.gif", animation = mapGIF)






