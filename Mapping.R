#### Maps for Buffer Data ####


# Before anything else, verify that your environment is totally clear.
# This is important, because old objects can foul up the works
# Clean up the working environment
rm(list = ls())

# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

#### Cleaning up the data ####


# packages for mapping
library(sf)
library(terra)
library(dplyr)
library(spData) # this is for the online resource bookmarked with a lot of code
library(rnaturalearth)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package


ne_countries() |>
  ggplot() +
  geom_sf()

library(mapview)
ne_countries()|>
  mapview

nz <- ne_countries(country = "New Zealand") |>
  mapview()

nz |>
  mapview()
# Add fill layer to nz shape
map_nz = tm_shape(nz) +
  tm_fill("grey40") 
# Add border layer to nz shape
tm_shape(nz) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(nz) +
  tm_fill() +
  tm_borders() 

print(map_nz)

