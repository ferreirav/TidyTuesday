# TidyTuesday Training Series
# R Script for Week 48
# Data on Doctor Who Tv Series

# Code Sourced from: https://github.com/TamayoLeivaJ/TidyTuesday/blob/gh-pages/2021/2021_Week_048/2021_Week_048.R
# Author: Tamayo Leiva
# December 2021

#Load libraries-----------------------------------------------------------------
library(here)
library(tidyverse)
library(tidytuesdayR)
library(scales)
library(ggtext)
library(systemfonts)
library(png)
library(ggimage)


# Example of code to folder creation
dir.create(here("training_folder/2021_w48/mk_dir_test"), recursive = TRUE, mode = "0755")

# Load data---------------------------------------------------------------------


episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')


# Data Wrangling----------------------------------------------------------------
#### What is the averagre star rating of Doctor Who?

doctor_who <- tibble(doctor = c("Christopher Eccleston", rep("David Tennant", 3), rep("Matt Smith", 3), rep("Peter Capaldi", 3), rep("Jodie Whittaker", 3)),
                     doctor_n = c("The Ninth Doctor", rep("The Tenth Doctor", 3), rep("The Eleventh Doctor", 3), rep("The Twelfth Doctor", 3), rep("The Thirteenth Doctor", 3)),
                     season_number = c(1, 2:4, 5:7, 8:10, 11:13),
                     doctor_color = c("#DA4D40",rep("#518F4A", 3),rep("#F27979", 3),rep("#5590A8", 3),rep("#EF9E0E", 3)))

(episodes <- episodes %>% 
  left_join(doctor_who, by = "season_number") %>% 
  filter(type != "special" & !is.na(rating) & season_number < 13) %>% 
  arrange(season_number, episode_number) %>%
  mutate(episode_id = row_number()) %>% 
  group_by(season_number) %>% 
  mutate(rating_avg = mean(rating),
         x = min(episode_id),
         xend = max(episode_id),
         season_label = min(episode_id) + (max(episode_id) - min(episode_id))/2) %>% 
  group_by(doctor) %>% 
  mutate(doctor_label = min(episode_id) + (max(episode_id) - min(episode_id))/2,
         doctor_image_label = min(episode_id) + (max(episode_id) - min(episode_id))/2) %>% 
  ungroup() %>% 
  mutate(season_number = factor(season_number)) %>% 
  mutate(doctor_image = case_when(doctor_n == "The Ninth Doctor" ~ "./training_folder/2021_w48/sourced_images/Doctor_Who_09.png",
                                  doctor_n == "The Tenth Doctor" ~ "./training_folder/2021_w48/sourced_images/Doctor_Who_10.png",
                                  doctor_n == "The Eleventh Doctor" ~ "./training_folder/2021_w48/sourced_images/Doctor_Who_11.png",
                                  doctor_n == "The Twelfth Doctor" ~ "./training_folder/2021_w48/sourced_images/Doctor_Who_12.png",
                                  doctor_n == "The Thirteenth Doctor" ~ "./training_folder/2021_w48/sourced_images/Doctor_Who_13.png"))
  )


# Plotting----------------------------------------------------------------------

#### Plot aesthetics ####
background  <- c("#011126")
lines_color <- c("#310F3E")
title_color <- c("#F6F6F8")
text_color  <- c("#DA4D40")
caption_color  <- c("#EF9E0E")

### Annotation ###

annotation_title_text <- c("Dr. Who are the favourites?")

### Plot ###

episodes %>% 
  ggplot(aes(episode_id, rating, group = season_number)) +
  geom_point(aes(color = season_number)) +
  geom_segment(aes(x = x - 0.2, xend = xend + 0.2, y = rating_avg, yend = rating_avg, colour = season_number)) +
  geom_segment(aes(x = episode_id, xend = episode_id, y = rating, yend = rating_avg, colour = season_number)) +
  # ### Annotations ###
  # ### Add y-axis information
  geom_point(data = tibble(x = -2, y = seq(76, 91, 1)), aes(x, y), shape = "-", color = text_color, inherit.aes = FALSE) +
  geom_text(data = tibble(x = -4, y = seq(76, 91, length.out = 6)), aes(x, y, label = y), color = text_color, size = 5, family = "Times New Roman", inherit.aes = FALSE) +
  geom_text(data = tibble(x = -7, y = 83.5), aes(x, y), label = "Rating", color = text_color, size = 3, angle = 90, family = "Times New Roman", inherit.aes = FALSE) +
  geom_point(data = tibble(x = 149, y = seq(76, 91, 1)), aes(x, y), shape = "-", color = caption_color, inherit.aes = FALSE) +
  geom_text(data = tibble(x = 152, y = seq(76, 91, length.out = 6)), aes(x, y, label = y), color = caption_color, size = 3, family = "Times New Roman", inherit.aes = FALSE) +
  ### Image Annotations ###
  geom_image(aes(x = doctor_image_label, y = 95, image = doctor_image), asp = 1.5) +
  ### Text Annotations ###
  geom_label(aes(x = season_label, y = 75, label = paste0("S",season_number), color = season_number), fill = background, size = 4, angle = 90, family = "Times New Roman") +
  geom_label(aes(x = doctor_label, y = 92, label = doctor_n, color = season_number), fill = background, size = 4, angle = 90, family = "Times New Roman") +
  annotate(geom = "curve", x = 70, y = 80, xend = 91, yend = 83.33333, curvature = -.4, arrow = arrow(length = unit(0.2, "lines"), type = "closed"), color = "#5590A8") +
  geom_text(data = tibble(x = 70, y = 79), aes(x, y), color = "#5590A8", label = "Middle line represent\nthe Season mean", size = 4, angle = 0, family = "Times New Roman", hjust = 0.5, vjust = 0.5, inherit.aes = FALSE) +
  ### Scales ###
  scale_x_continuous(limits = c(-7, 152)) +
  scale_y_continuous(expand = c(0,0), limits = c(74, 98)) +
  scale_color_manual(values = doctor_who$doctor_color) +
  coord_cartesian(expand = TRUE, clip = "on") +
  ### Themes ###
  theme_classic() +
  theme(
    ### Text ###
    text = element_text(face = "plain", family = "Times New Roman", color = text_color, hjust = 0.5, vjust = 0.5, angle = 0),
    ### Axis ###
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    ### Panel Grid ###
    ## Plot Aesthetics ##
    panel.background = element_rect(fill = background, colour = NA),
    plot.background = element_rect(fill = background, color = NA),
    legend.background = element_rect(fill = background, color = NA),
    legend.key = element_rect(fill = background, color = NA),
    ### Legend ###
    legend.position = "none",
    ### Titles and Captions ###
    plot.title.position = "plot",
    plot.title = element_markdown(color = title_color, family = "Times New Roman", face = "plain", size = 30),
    plot.caption.position = "plot",
    plot.caption = element_markdown(color = caption_color, family = "Times New Roman", hjust = 1, halign = 1, size = 14
                                    ),
    ### Margin ###
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
  labs(title = annotation_title_text,
       caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @Vitor Ferreira<br>
                              <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> Inspired by TamayoLeivaJ<br><br> 
                              Source: Dr. Who {datardis}<br>")

### Saving Plot ###
ggsave("./training_folder/2021_w48/doctor_who_2.png", width = 15, height = 9)
