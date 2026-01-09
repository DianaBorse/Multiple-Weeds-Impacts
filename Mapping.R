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

# remove outliers
Bufferdata<-Bufferdata[-817, ] 
Bufferdata<-Bufferdata[-36, ] 

# make it a shape file
Buffer <- Bufferdata |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

#### Inset map of properties ####
ggplot(Buffer) +
  geom_sf()

# add a satellite map using an API from LINZ
library(leaflet)
my_api_key <- "2e6d6ded62814d4a961ad3b5574183b7"
leaflet() %>% 
addTiles(urlTemplate = paste0(
  "https://basemaps.linz.govt.nz/v1/tiles/aerial/3857/{z}/{x}/{y}.png?api=", 
  my_api_key 
  ),
  attribution = "© LINZ" 
  ) %>% 
  setView(lng = 174.7633, lat = -36.8485, zoom = 12) |># add a map using an API from LINZ
  addCircleMarkers(
    data = Buffer,
    radius = 2,
    color = "orange",
    fillOpacity = 0.8,
    stroke = FALSE
  ) %>%
  addMiniMap(
    tiles =(urlTemplate = paste0(
      "https://basemaps.linz.govt.nz/v1/tiles/aerial/3857/{z}/{x}/{y}.png?api=", 
      my_api_key 
    )),
    toggleDisplay = TRUE
  )

# simple topo map
leaflet() %>% addProviderTiles("Esri.WorldTopoMap") %>% setView(lng = 174.7633, lat = -36.8485, zoom = 12)


library(leaflet)
library(leaflet.extras2)

leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7633, lat = -36.8485, zoom = 12) %>%
  addMiniMap(
    tiles = providers$Esri.WorldTopoMap,
    toggleDisplay = TRUE,
    minimized = FALSE,
    width = 150,
    height = 150
  )

library(sf)
library(leaflet)
library(leaflet.extras2)

# inset map of properties on topomap
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7633, lat = -36.8485, zoom = 12) %>%
  addCircleMarkers(
    data = Buffer,
    radius = 2,
    color = "orange",
    fillOpacity = 0.8,
    stroke = FALSE
  ) %>%
  addMiniMap(
    tiles = providers$Esri.WorldTopoMap,
    toggleDisplay = TRUE
  )

#### Species Distribution Maps ####
# Reduce to only one column to represent each species
Bufferdata <- subset(Bufferdata, select = -c(OldestGrowthStageHoneysuckle, Area_m2Honeysuckle, HeightMetresHoneysuckle, OldestGrowthStageMadeiraVine,
  Area_m2MadeiraVine, HeightMetresMadeiraVine, Area_m2Periwinkle, HeightMetresPeriwinkle, OldestGrowthStageEnglishIvy, Area_m2EnglishIvy, 
  HeightMetresEnglishIvy, OldestGrowthStageBlueMorningGlory, Area_m2BlueMorningGlory, HeightMetresBlueMorningGlory, OldestGrowthStageJasmine, 
  Area_m2Jasmine, HeightMetresJasmine, OldestGrowthStageCoastalBanksia, Area_m2CoastalBanksia, HeightMetresCoastalBanksia, OldestGrowthStageRhamnus, 
  Area_m2Rhamnus, HeightMetresRhamnus, OldestGrowthStageWoolly, Area_m2Woolly, HeightMetresWoolly,
  Area_m2BushyAsparagus, HeightMetresBushyAsparagus, OldestGrowthStageClimbingAsparagus, Area_m2ClimbingAsparagus, HeightMetresClimbingAsparagus,
  OldestGrowthStageGinger, Area_m2Ginger, Height_metresGinger, OldestGrowthStageMothPlant, Area_m2MothPlant, HeightMetresMothPlant, CreationDate, EndOutcome))

colnames(Bufferdata)[1:13] <- c("A.sericifera", "H.gardnerianum", "A.scandens", "A.densiflorus", "S.mauritianum", "R.alaternus", 
                                "B.integrifolia", "J.polyanthum", "I.tricolor", "H.helix", "V.major", "A.cordifolia", "L.japonica") ## Renaming the columns

# Now I need any rows that are NA to be 0 and any with text to be 1
Bufferdata[, 1:13] <- lapply(Bufferdata[, 1:13], function(col) {
  ifelse(!is.na(col), "1", NA) })
Bufferdata <- Bufferdata %>%
  mutate_all(~ replace(., is.na(.), 0))

# Great! Now let's start with moth plant, if there is a 1, keep, if there is a
# 0, remove that row
library(dplyr)
MothPlant <- Bufferdata %>%
  filter(A.sericifera != 0)

# and let's make one that is the properties without moth plant as well
NoMothPlant <- Bufferdata %>%
  filter(A.sericifera == 0)

# now make that into a shapefile
# make it a shape file
MothPlant <- MothPlant |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
# make it a shape file
NoMothPlant <- NoMothPlant |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

# map of moth plant
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7233, lat = -36.8125, zoom = 14.25) %>%
  addCircleMarkers(
    data = MothPlant,
    radius = 3,
    color = "#B82783",
    fillOpacity = 0.8,
    stroke = FALSE
  ) |>
  addCircleMarkers(
    data = NoMothPlant,
    radius = 2,
    color = "grey",
    fillOpacity = 0.5,
    stroke = FALSE
  )

# Ginger
library(dplyr)
Ginger <- Bufferdata %>%
  filter(H.gardnerianum != 0)

# and let's make one that is the properties without as well
NoGinger <- Bufferdata %>%
  filter(H.gardnerianum == 0)

# now make that into a shapefile
# make it a shape file
Ginger <- Ginger |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
# make it a shape file
NoGinger <- NoGinger |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

# map of ginger
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7233, lat = -36.8125, zoom = 14.25) %>%
  addCircleMarkers(
    data = Ginger,
    radius = 3,
    color = "#73B897",
    fillOpacity = 1.0,
    stroke = FALSE
  ) |>
  addCircleMarkers(
    data = NoGinger,
    radius = 2,
    color = "grey",
    fillOpacity = 0.5,
    stroke = FALSE
  )

# Climbing Aparagus
library(dplyr)
CAsp <- Bufferdata %>%
  filter(A.scandens != 0)

# and let's make one that is the properties without as well
NoCAsp <- Bufferdata %>%
  filter(A.scandens == 0)

# now make that into a shapefile
# make it a shape file
CAsp <- CAsp |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
# make it a shape file
NoCAsp <- NoCAsp |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

# map
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7233, lat = -36.8125, zoom = 14.25) %>%
  addCircleMarkers(
    data = CAsp,
    radius = 3,
    color = "#E37450",
    fillOpacity = 1.0,
    stroke = FALSE
  ) |>
  addCircleMarkers(
    data = NoCAsp,
    radius = 2,
    color = "grey",
    fillOpacity = 0.5,
    stroke = FALSE
  )

# Bushy Aparagus
library(dplyr)
BAsp <- Bufferdata %>%
  filter(A.densiflorus != 0)

# and let's make one that is the properties without as well
NoBAsp <- Bufferdata %>%
  filter(A.densiflorus == 0)

# now make that into a shapefile
# make it a shape file
BAsp <- BAsp |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
# make it a shape file
NoBAsp <- NoBAsp |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

# map 
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7233, lat = -36.8125, zoom = 14.25) %>%
  addCircleMarkers(
    data = BAsp,
    radius = 3,
    color = "#EAC225",
    fillOpacity = 1.0,
    stroke = FALSE
  ) |>
  addCircleMarkers(
    data = NoBAsp,
    radius = 2,
    color = "grey",
    fillOpacity = 0.5,
    stroke = FALSE
  )

# Woolly Nightshade
library(dplyr)
Woolly <- Bufferdata %>%
  filter(S.mauritianum != 0)

# and let's make one that is the properties without as well
NoWoolly <- Bufferdata %>%
  filter(S.mauritianum == 0)

# now make that into a shapefile
# make it a shape file
Woolly <- Woolly |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
# make it a shape file
NoWoolly <- NoWoolly |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

# map
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7233, lat = -36.8125, zoom = 14.25) %>%
  addCircleMarkers(
    data = Woolly,
    radius = 3,
    color = "#001889",
    fillOpacity = 0.8,
    stroke = FALSE
  ) |>
  addCircleMarkers(
    data = NoWoolly,
    radius = 2,
    color = "grey",
    fillOpacity = 0.5,
    stroke = FALSE
  )

# skip rhamnus, never showed up
# Coastal Banksia
library(dplyr)
Banksia <- Bufferdata %>%
  filter(B.integrifolia != 0)

# and let's make one that is the properties without as well
NoBanksia <- Bufferdata %>%
  filter(B.integrifolia == 0)

# now make that into a shapefile
# make it a shape file
Banksia <- Banksia |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
# make it a shape file
NoBanksia <- NoBanksia |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

# map
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7233, lat = -36.8125, zoom = 14.25) %>%
  addCircleMarkers(
    data = Banksia,
    radius = 4,
    color = "#7D0112",
    fillOpacity = 0.8,
    stroke = FALSE
  ) |>
  addCircleMarkers(
    data = NoBanksia,
    radius = 2,
    color = "grey",
    fillOpacity = 0.5,
    stroke = FALSE
  )

# Jasmine
library(dplyr)
Jasmine <- Bufferdata %>%
  filter(J.polyanthum != 0)

# and let's make one that is the properties without as well
NoJasmine <- Bufferdata %>%
  filter(J.polyanthum == 0)

# now make that into a shapefile
# make it a shape file
Jasmine <- Jasmine |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
# make it a shape file
NoJasmine <- NoJasmine |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

# map 
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7233, lat = -36.8125, zoom = 14.25) %>%
  addCircleMarkers(
    data = Jasmine,
    radius = 3,
    color = "#8A008D",
    fillOpacity = 0.8,
    stroke = FALSE
  ) |>
  addCircleMarkers(
    data = NoJasmine,
    radius = 2,
    color = "grey",
    fillOpacity = 0.5,
    stroke = FALSE
  )

# MorningGlory
library(dplyr)
MorningGlory <- Bufferdata %>%
  filter(I.tricolor != 0)

# and let's make one that is the properties without as well
NoMorningGlory <- Bufferdata %>%
  filter(I.tricolor == 0)

# now make that into a shapefile
# make it a shape file
MorningGlory <- MorningGlory |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
# make it a shape file
NoMorningGlory <- NoMorningGlory |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

# map 
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7233, lat = -36.8125, zoom = 14.25) %>%
  addCircleMarkers(
    data = MorningGlory,
    radius = 3,
    color = "#44038A",
    fillOpacity = 0.8,
    stroke = FALSE
  ) |>
  addCircleMarkers(
    data = NoMorningGlory,
    radius = 2,
    color = "grey",
    fillOpacity = 0.5,
    stroke = FALSE
  )

# English Ivy
library(dplyr)
Ivy <- Bufferdata %>%
  filter(H.helix != 0)

# and let's make one that is the properties without as well
NoIvy <- Bufferdata %>%
  filter(H.helix == 0)

# now make that into a shapefile
# make it a shape file
Ivy <- Ivy |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
# make it a shape file
NoIvy <- NoIvy |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

# map 
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7233, lat = -36.8125, zoom = 14.25) %>%
  addCircleMarkers(
    data = Ivy,
    radius = 3,
    color = "#51B09F",
    fillOpacity = 0.8,
    stroke = FALSE
  ) |>
  addCircleMarkers(
    data = NoIvy,
    radius = 2,
    color = "grey",
    fillOpacity = 0.5,
    stroke = FALSE
  )

# Periwinkle
library(dplyr)
Periwinkle <- Bufferdata %>%
  filter(V.major != 0)

# and let's make one that is the properties without as well
NoPeriwinkle <- Bufferdata %>%
  filter(V.major == 0)

# now make that into a shapefile
# make it a shape file
Periwinkle <- Periwinkle |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
# make it a shape file
NoPeriwinkle <- NoPeriwinkle |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

# map 
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7233, lat = -36.8125, zoom = 14.25) %>%
  addCircleMarkers(
    data = Periwinkle,
    radius = 4,
    color = "#3B99B1",
    fillOpacity = 0.8,
    stroke = FALSE
  ) |>
  addCircleMarkers(
    data = NoPeriwinkle,
    radius = 2,
    color = "grey",
    fillOpacity = 0.5,
    stroke = FALSE
  )

# Madeira Vine
library(dplyr)
Madeira <- Bufferdata %>%
  filter(A.cordifolia != 0)

# and let's make one that is the properties without as well
NoMadeira <- Bufferdata %>%
  filter(A.cordifolia == 0)

# now make that into a shapefile
# make it a shape file
Madeira <- Madeira |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
# make it a shape file
NoMadeira <- NoMadeira |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

# map 
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7233, lat = -36.8125, zoom = 14.25) %>%
  addCircleMarkers(
    data = Madeira,
    radius = 3,
    color = "#D85A68",
    fillOpacity = 0.8,
    stroke = FALSE
  ) |>
  addCircleMarkers(
    data = NoMadeira,
    radius = 2,
    color = "grey",
    fillOpacity = 0.5,
    stroke = FALSE
  )

# Honeysuckle
library(dplyr)
Honeysuckle <- Bufferdata %>%
  filter(L.japonica != 0)

# and let's make one that is the properties without as well
NoHoneysuckle <- Bufferdata %>%
  filter(L.japonica == 0)

# now make that into a shapefile
# make it a shape file
Honeysuckle <- Honeysuckle |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)
# make it a shape file
NoHoneysuckle <- NoHoneysuckle |>
  st_as_sf(coords = c("x", "y"),
           crs = 4326)

# map 
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7233, lat = -36.8125, zoom = 14.25) %>%
  addCircleMarkers(
    data = Honeysuckle,
    radius = 3,
    color = "#EA8E2B",
    fillOpacity = 0.8,
    stroke = FALSE
  ) |>
  addCircleMarkers(
    data = NoHoneysuckle,
    radius = 2,
    color = "grey",
    fillOpacity = 0.5,
    stroke = FALSE
  )


hcl.colors(14, palette = "Plasma")
hcl.colors(14, palette = "Zissou 1")
#[1] "#001889" "#44038A" "#6C008C" "#8A008D" "#A3098A" "#B82783" "#CA4178" "#D85A68" "#E37450" "#EA8E2B" "#EDA900" "#EBC400" "#E4E100" "#DAFF47"
#[1] "#3B99B1" "#37A6A9" "#51B09F" "#73B897" "#95BE95" "#B0C590" "#CECA87" "#EAC225" "#E9B01D" "#E89D15" "#E78A05" "#E87300" "#ED5600" "#F5191C"

# Map for fieldwork sites
library(readr)
PlotData <- read_csv("Fieldwork/PlotData_Clean_WN_removed_PopnDensity.csv")

# coordinates are not formatted correctly, I need to divide them by 100
PlotData$East_Coordinates <- PlotData$East_Coordinates / 100
PlotData$South_Coordinates <- PlotData$South_Coordinates / 100
PlotData$South_Coordinates <- PlotData$South_Coordinates * -1

# All of the points are shifted north and west of where they should be
# the handheld GPS should have been in WGS 84 which means it would be crs 4326
# the easiest thing to do will likely be to make a new df with manually entered
# coordinates from maps

# make it a shape file
Plot <- PlotData |>
  st_as_sf(coords = c("East_Coordinates", "South_Coordinates"),
           crs = 4326)

st_bbox(Plot)

# inset map of properties on topomap
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng = 174.7633, lat = -36.8485, zoom = 12) %>%
  addCircleMarkers(
    data = Plot,
    radius = 2,
    color = "orange",
    fillOpacity = 0.8,
    stroke = FALSE
  ) %>%
  addMiniMap(
    tiles = providers$Esri.WorldTopoMap,
    toggleDisplay = TRUE
  )


st_coordinates(Plot)[1:5, ]
summary(PlotData$East_Coordinates)
summary(PlotData$South_Coordinates)

library(sf)

# 1. Drop geometry completely
Plot_clean <- st_drop_geometry(Plot)

# 2. Ensure coordinates are numeric and have correct signs
Plot_clean$East_Coordinates  <- as.numeric(PlotData$East_Coordinates)
Plot_clean$South_Coordinates <- as.numeric(PlotData$South_Coordinates)

# 3. Rebuild geometry from scratch
Plot_clean <- st_as_sf(
  Plot_clean,
  coords = c("East_Coordinates", "South_Coordinates"),  # lon, lat
  crs = 4326
)

# 4. Check
st_bbox(Plot_clean)

leaflet(Plot_clean) %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  addCircleMarkers()
