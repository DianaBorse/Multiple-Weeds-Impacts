#### NMDS ####

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

library(readr)
SurveyData <- read_csv("SurveyData_Clean.csv")

# Unite Site and Plot columns
library(tidyr)
SurveyData_Combined <- SurveyData %>%
  unite(Plot, Site, Plot, sep = " - ")

# Unite Plot and Weed vs Native columns so that now the plot id is all in one
# column that includes the site, plot number, and whether it is weed or native
library(tidyr)
SurveyData_Combined <- SurveyData_Combined %>%
  unite(Plot, Plot, W_N, sep = " - ")

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
  unite(Plot, Site, Plot, sep = " - ")

# Unite Plot and Weed vs Native columns so that now the plot id is all in one
# column that includes the site, plot number, and whether it is weed or native
library(tidyr)
SurveyData_Combined <- SurveyData_Combined %>%
  unite(Plot, Plot, W_N, sep = " - ")

#### Change the data to be a matrix (n sample units x p species) ####

# This code gives NA values when species are not present in a plot
#Survey_wideNA <- SurveyData_Combined %>%
#  pivot_wider(names_from = Plot, 
#              values_from = Tier_1, 
#              id_cols = ScientificName)

# This code gives 0 values when species are not present in a plot
Survey_wide <- SurveyData_Combined %>%
  pivot_wider(names_from = Plot, 
              values_from = Tier_1, 
              id_cols = ScientificName) %>%
  mutate_all(~ replace(., is.na(.), 0))

# Needs to be numeric
# Check the data types of each column
str(Survey_wide)

# Need to change the Plots to numeric names

# Note is that this is not subset, may need to subset to only the 
# most common species or species that occur more than 5 times etc.

#### NMDS ####


library(vegan)

set.seed(42)

z <- metaMDS(comm = Survey_wide,
             autotransform = FALSE,
             distance = "bray",
             engine = "monoMDS",
             k = 3,
             weakties = TRUE,
             model = "global",
             maxit = 300,
             try = 40,
             trymax = 100)
