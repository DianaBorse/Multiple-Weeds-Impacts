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

#### Load Packages and cleaning up the data ####

# packages for mapping
library(sf)
library(terra)
library(dplyr)
library(spData) # this is for the online resource bookmarked with a lot of code
library(rnaturalearth)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

library(readr)
Bufferdata <- read_csv("Buffer Data/Bufferdata.csv")

# remove rows that do not include surveys
Bufferdata <- Bufferdata %>% filter(EndOutcome != "Q")
Bufferdata <- Bufferdata %>% filter(EndOutcome != "NV")
Bufferdata <- Bufferdata %>% filter(EndOutcome != "NH")
Bufferdata <- Bufferdata %>% filter(EndOutcome != "CB")
Bufferdata <- Bufferdata %>% filter(EndOutcome != "NA")

# make it a shape file
Buffer <- Bufferdata |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

#### Make a map of properties ####
ggplot(Buffer) +
  geom_sf()

# Add fill layer to nz shape
tm_shape(nz) +
  tm_fill() 
# Add border layer to nz shape
tm_shape(nz) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(nz) +
  tm_fill() +
  tm_borders()

map_nz = tm_shape(nz) + tm_polygons()

map_nz2 = map_nz +
  tm_shape(Buffer) + tm_symbols()

map_nz2

#### Inset smaller map ####
nz_region = st_bbox(c(xmin = 172000, xmax = 175000,
                      ymin = -3500000, ymax = -3600000),
                    crs = 4326) |> 
  st_as_sfc()

nz_region_map = tm_shape(nz, bbox = nz_region)

nz_map = tm_shape(nz) + tm_polygons() +
  tm_shape(Buffer) + tm_symbols() + 
  tm_shape(nz_region) + tm_borders(lwd = 3) +
  tm_layout(bg.color = "lightblue")

library(grid)
norm_dim = function(obj){
  bbox = st_bbox(obj)
  width = bbox[["xmax"]] - bbox[["xmin"]]
  height = bbox[["ymax"]] - bbox[["ymin"]]
  w = width / max(width, height)
  h = height / max(width, height)
  return(unit(c(w, h), "snpc"))
}
main_dim = norm_dim(nz_region)
ins_dim = norm_dim(nz)

ins_vp = viewport(width = ins_dim[1] * 0.5, height = ins_dim[2] * 0.5,
                  x = unit(1, "npc") - unit(0.5, "cm"), y = unit(0.5, "cm"),
                  just = c("right", "bottom"))

grid.newpage()
print(nz_region_map, vp = main_vp)
pushViewport(main_vp)
print(nz_map, vp = ins_vp)

# try something else
library(ggspatial)
library(ggplot2)
library(maptiles)

# Rough bounding box for NZ in WGS84 (EPSG:4326)
nz_bbox <- st_bbox(c(
  xmin = 166,   # west
  xmax = 179,   # east
  ymin = -48,   # south
  ymax = -34    # north
), crs = st_crs(4326))

# Get satellite imagery from ESRI (no API key needed)
nz_sat <- get_tiles(
  x = nz_bbox,
  provider = "Esri.WorldImagery",  # satellite imagery
  zoom = 5                         # adjust: 4–6 for country-level
)

# Quick base plot
plot_tiles(nz_sat)


