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

# Load packages
require(vegan)
require(labdsv)
require(magrittr)
#### Cleaning up the data ####

library(dplyr)

library(readr)

library(readr)
Oak <- read_csv("Oak_data_47x216.csv")
Oak_spp <- read.csv("Oak_species_189x5.csv", header = TRUE)

# Subset response and explanatory variables
Oak_abund <- Oak[ , colnames(Oak) %in% Oak_spp$SpeciesCode] 
Oak_explan <- Oak[ , ! colnames(Oak) %in% Oak_spp$SpeciesCode]

library(vegan)
# Make adjustments to response variables Oak 1 is essentially my survey_wide
Oak1 <- Oak_abund %>%
  vegtab(minval = 0.05 * nrow(Oak_abund)) %>% # Remove rare species
  decostand("max") # Relativize by species maxima

# I need to make something like this, but with Group/Central Species instead of
# Grazing
Oak_explan <- Oak_explan %>%
  rownames_to_column(var = "Stand") %>%
  rowid_to_column(var = "ID") %>%
  mutate(GP_GC = paste(GrazPast, GrazCurr, sep = "_")) %>%
  merge(y = data.frame(GP_GC = c("No_No", "Yes_No", "Yes_Yes"),
                       Grazing = c("Never", "Past", "Always"))) %>%
  arrange(ID)


