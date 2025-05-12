#### GLM ####

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

library(dplyr)

#library(readr)
#SurveyData <- read_csv("SurveyData_Clean.csv")

library(readr)
SurveyData <- read_csv("SurveyData_Clean_WN_removed.csv")

#library(readr)
#PlotData <- read_csv("PlotData_Clean.csv")

library(readr)
PlotData <- read_csv("PlotData_Clean_WN_removed.csv")

# The species that were entered as percent-cover need to be accounted for
# Given that the number of plants will vary greatly, but cover plants were all
# in Tier_1, for each percent cover plant I will give a value of 

# The plot needs to be numeric, so I need to change the Plot names to unique
# numeric variables, site needs to be given a number value as does W_N

# This code gives the combined plot names a simple, unique, numeric ID, but I 
# would like clearer ID's so I will use another method
# SurveyData_Combined$Plot <- as.numeric(as.factor(SurveyData_Combined$Plot))

# Assigning the Site a unique numeric value
SurveyData$Site <- as.numeric(as.factor(SurveyData$Site))
# Assigning W and N unique numeric values (weed is now 2, native is now 1)
SurveyData$W_N <- as.numeric(as.factor(SurveyData$W_N))

# Now combine this into unique numerical plot names
library(tidyr)
SurveyData_Combined <- SurveyData %>%
  unite(Plot, Site, Plot, sep = "-")

# Unite Plot and Weed vs Native columns so that now the plot id is all in one
# column that includes the site, plot number, and whether it is weed or native
library(tidyr)
SurveyData_Combined <- SurveyData_Combined %>%
  unite(Plot, Plot, W_N, sep = "-")

# Need to subset to only include the environmental weeds
# Subset the data to only include species that are weeds using the Status column
SurveyData_Combined_subset <- SurveyData_Combined[SurveyData_Combined$Status == 1, ]

# Calculate the number of environmental weeds per plot
library(dplyr)
RichnessWeed <- SurveyData_Combined_subset %>% group_by(Plot) %>% summarize(unique_values = n_distinct(ScientificName))

# Repeat for plot data
# Assigning the Site a unique numeric value
PlotData$Site <- as.numeric(as.factor(PlotData$Site))
# Assigning W and N unique numeric values (weed is now 2, native is now 1)
PlotData$Weed_Native <- as.numeric(as.factor(PlotData$Weed_Native))

# Now combine this into unique numerical plot names
library(tidyr)
PlotData_Combined <- PlotData %>%
  unite(Plot, Site, Plot, sep = "-")

# Unite Plot and Weed vs Native columns so that now the plot id is all in one
# column that includes the site, plot number, and whether it is weed or native
library(tidyr)
PlotData_Combined <- PlotData_Combined %>%
  unite(Plot, Plot, Weed_Native, sep = "-")


# Now I need to only include the environmental variables that I want to include
# for the GLM

PlotData_Combined <- subset(PlotData_Combined, select = -c(Date, CanopyCover_App, Waypoint, East_Coordinates, South_Coordinates, CoverVascular,
                                                           CoverNonVascular, CoverLitter, CoverBareSoil, CoverDebris, CoverGrass,
                                                           Topography, ParentMaterial, Notes))

# Need to simplify the column names 
colnames(PlotData_Combined)[3:11] <- c("Height", "DBH",
                                       "Slope", "Canopy",
                                       "Erosion", "Disturbance",
                                       "Pests", "Litter", "Housing") ## Renaming the columns

# Let's have a quick look at the relationships between variables and the number of environmental weeds



# Need to transform the data square root was given as a good transformation, so I will do that
# SurveyData_Combined$Tier_1_sqrt <- sqrt(SurveyData_Combined$Tier_1)
# repeat for weeds only data
# SurveyData_Combined_subset$Tier_1_sqrt <- sqrt(SurveyData_Combined_subset$Tier_1)

library(dplyr)
Env_Species <- left_join(RichnessWeed, PlotData_Combined, by = "Plot")  # Preserves all rows from df1

# GGPLOT for height
WeedRichnessPlot <- ggplot(data = Env_Species, mapping = aes(x = unique_values, y = Height)) +
  geom_point(size = 2) + # Set point size
  theme_minimal() +
  labs(x = "Weed Species Richness", y = "Central Species Height") +
  theme_classic()

print(WeedRichnessPlot)

# GGPLOT for DBH
WeedRichnessSlope <- ggplot(data = Env_Species, mapping = aes(x = unique_values, y = DBH)) +
  geom_point(size = 2) + # Set point size
  theme_minimal() +
  labs(x = "Weed Species Richness", y = "Central Species DBH") +
  theme_classic()

print(WeedRichnessDBH)

# GGPLOT for Slope
WeedRichnessSlope <- ggplot(data = Env_Species, mapping = aes(x = unique_values, y = Slope)) +
  geom_point(size = 2) + # Set point size
  theme_minimal() +
  labs(x = "Weed Species Richness", y = "Slope") +
  theme_classic()

print(WeedRichnessSlope)

# GGPLOT for Canopy
WeedRichnessCanopy <- ggplot(data = Env_Species, mapping = aes(x = unique_values, y = Canopy)) +
  geom_point(size = 2) + # Set point size
  theme_minimal() +
  labs(x = "Weed Species Richness", y = "Canopy cover") +
  theme_classic()

print(WeedRichnessCanopy)

# GGPLOT for Litter
WeedRichnessLitter <- ggplot(data = Env_Species, mapping = aes(x = unique_values, y = Litter)) +
  geom_point(size = 2) + # Set point size
  theme_minimal() +
  labs(x = "Weed Species Richness", y = "Litter") +
  theme_classic()

print(WeedRichnessLitter)

# GGPLOT for Housing Density
WeedRichnessHousing <- ggplot(data = Env_Species, mapping = aes(x = unique_values, y = Housing)) +
  geom_point(size = 2) + # Set point size
  theme_minimal() +
  labs(x = "Weed Species Richness", y = "Housing") +
  theme_classic()

print(WeedRichnessHousing)

# Load the required packages
library(lattice)
library(R2jags)
Mydotplot <- function(DataSelected)

MyVar <- c("Height", "DBH", "Slope", "Litter", "Housing")
Mydotplot(Env_Species[, MyVar])


# Extract data
WeedRichnessPlotData <- ggplot_build(WeedRichnessPlot)$data[[1]]

# Convert to data frame
df_points <- as.data.frame(WeedRichnessPlotData)

# I need to make a data frame that contains the environmental factors with the survey data
# That means I need Plot_species with PlotData_Weeds

Env_SpeciesPoints <- cbind(df_points, Env_Species) ## This combines the dataset with the coordinates
en = envfit(WeedRichnessPlot, Env_SpeciesPoints, permutations = 9999, na.rm = TRUE) ## This creates the arrows
en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en) * 0.5 ##Adjust the last number to change the length of the arrows
en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en) * 0.5 ##Adjust the last number to change the length of the arrows


