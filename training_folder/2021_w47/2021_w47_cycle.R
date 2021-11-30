# TidyTuesday Training Series
# R Script for Week 46
# This week is dedicated to map drawing from the 30DayMapChallenge

# Code Sourced from: https://jamiehudson.netlify.app/post/distance_map/
# Author: Jamie Hudson
# November 2021

# We will draw a map with walking distances using a {openrouteservice} library 
# which will enable us to draw geographic data from Open Street Map through API.

# Install libraries instructions
### install.packages("remotes")
remotes::install_github("GIScience/openrouteservice-r", force = TRUE)


#Load libraries-----------------------------------------------------------------
library(openrouteservice)
library(mapview)
library(tidyverse)
library(osmdata)
library(sf)
library(paletteer)
library(ggfx)


# To access Open Route Source (ORS) we need to validate our personal key for openrouteservice
# Key is found in your account dashboard ("https://openrouteservice.org/dev/#/home")

ors_api_key("5b3ce3597851110001cf6248d2addc59da094e1a9a8b5d79763bd09b")

# Coordinates of hour house (SW6 1SJ) where we then can define walking distances 
# of equal time from this point

coordinates <- data.frame(lon = c(-0.197222), lat = c(51.485311))

# We will the store the isochrones

house_iso <- ors_isochrones(locations = coordinates, profile = "cycling-regular",
                            range = 3600, interval = 600, output = "sf")

            ### Arguments explained here:
  
            # - locations: where we specify the coordinate of our site of interest.
            # - profile: where we set our “route profile” which is basically our method of transport.
            #           - Options include transport via car, bike, and wheelchair amongst others (a full list can be obtained with the function ors_profile())
            # - range: The maximum value for our analyses in seconds (or meters if we wanted to do distance- but let’s stick with time).
            # - interval: The interval of our isochrones in seconds (i.e if you wanted an isochrone for every hour, this value would be 60 seconds x 60 minutes = 3600).
            # - output: the format of our output, in this case sf.

# Using mapview() to produce an interactive leaflet map to visualise the isochrones

mapviewOptions(fgb = FALSE)

intervals <- levels(factor(house_iso$value))
house_iso_list <- split(house_iso, intervals)
house_iso_list <- house_iso_list[rev(intervals)]

names(house_iso_list) <- sprintf("%s_min", as.numeric(names(house_iso_list))/60)

mapview(house_iso_list, alpha.regions = 0.2, homebutton = FALSE, legend = FALSE)


# Other creative ways to build the map ahead are found, and inspired at:
# https://taraskaduk.com/posts/2021-01-18-print-street-maps/#ref-burkhart
# https://ggplot2tutor.com/tutorials/streetmaps
# https://joshuamccrain.com/tutorials/maps/streets_tutorial.html


# We will now set the margins of our map
x <- c(-0.40649, 0.01036)
y <- c(51.33759, 51.63762)

custom_fulham <- rbind(x, y)
colnames(custom_fulham) <- c("min", "max")

# To obtain data for the background we will use osmdata to access data OpenStreetMap in a vector form

streets <- custom_fulham %>% 
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "scondary", "tertiary", "trunk",
                  "secondary_link", "terciary_link", "residential", "living_street",
                  "unclassified", "service", "footway")) %>% 
  osmdata_sf()


# Next step is to crop the "roads", "highways", "footpaths", etc

# To do this, I have created the little function below. In a nutshell, this finds the 
# difference between consecutive isochrones (i.e. 30 mins and 40 mins) 
# and crops our street coordinates to match this.

rep.x <- function(i, na.rm = FALSE) {
  
  if(i == length(house_iso_list)) {streets$osm_lines %>% st_intersection(house_iso_list[[i]])}
  
  else if(i < length(house_iso_list)) {streets$osm_lines %>% st_intersection(st_difference(house_iso_list[[i]], house_iso_list[[i+1]]))}
  
}


# We can then apply this function to each element of our cj_iso_list list 
# (where each element is an isochrone) using the lapply() function. We can then extract 
# each of the individual isochrone of the list if we wanted to (and plot lots of individual 
# geoms for each isochrone), though a more concise way is running the function bind_rows() 
# from the dplyr() package, which splits a list into a data frame.

list_df <- lapply(1:length(house_iso_list), rep.x)

iso_df <- dplyr::bind_rows(list_df)


# Finally we will plot the final map
## First we are defining the color palette

colpal <- rev(paletteer_c("pals::ocean.speed", 6))
# Alternative color pallet is called "magma"
# Other colour options at: https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html


cycling_map <- ggplot() +
  geom_sf(data = streets$osm_lines,
          color = "#151515",
          size = 0.2) +
  geom_sf(data = iso_df,
          aes(colour = as.factor(value),
              geometry = geometry),
          fill = "#060606",
          size = 0.2,
          alpha = 0.8) +
  scale_colour_manual(values = rev(colpal),
                      labels = seq(10, 60, 10),
                      guide = guide_legend(override.aes = list(fill = rev(colpal), alpha = 1),
                                           nrow = 1,
                                           keywidth = 1.5,
                                           keyheight = 0.3,
                                           title.position = "top",
                                           label.position = "bottom",
                                           label.hjust = 0.5)) +
  coord_sf(xlim = custom_fulham[1,],
           ylim = custom_fulham[2,],
           expand = FALSE) +
  with_outer_glow(annotate(geom = "text", label = "Cycling around SW6",
                           x = -0.20, y = 51.625, size = 8.5, hjust = 0.5, colour = colpal[6],
                           family = "mono"),
                  colour = colpal[4], sigma = 10, expand = 10) +
  with_outer_glow(annotate(geom = "text", label = "How far can cycle from SW6 in 60 minutes?",
                           x = -0.20, y = 51.610, size = 4, hjust = 0.5, colour = colpal[6], family = "mono"),
                  colour = colpal[4], sigma = 10, expand = 7) +
  with_outer_glow(annotate(geom = "text", label = "@Vitor Ferreira | source = © openrouteservice.org by HeiGIT | Map data © OpenStreetMap contributors",
                           x = -0.20, y = 51.350, size = 3, hjust = 0.5, colour = colpal[6], family = "mono"),
                  colour = colpal[4], sigma = 10, expand = 4) +
  theme_void() + # Ignoring this line we can see the coordinates for rescale the labels
  theme(plot.background = element_rect(fill = "#060606"),
        panel.background = element_rect(fill = "#060606"),
        legend.text = with_outer_glow(element_text(colour = colpal[6],
                                                   family = "mono"),
                                      colour = colpal[4], sigma = 2, expand = 3),
        legend.title = element_blank(),
        legend.position=c(0.5, 0.80),
        legend.justification = "bottom",
        legend.direction = "horizontal")
cycling_map

ggsave("./training_folder/2021_w47/sw6_cycling_map.png", cycling_map, width = 12, height = 6.1)
