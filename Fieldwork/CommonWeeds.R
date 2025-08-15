#### Most Environmental Weeds ####

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
SurveyData <- read_csv("Fieldwork/SurveyData_Clean.csv")

# Unite Site and Plot columns
library(tidyr)
SurveyData_united <- SurveyData %>%
  unite(Plot, Site, Plot, sep = " - ")

# Unite Plot and Weed vs Native columns so that now the plot id is all in one
# column that includes the site, plot number, and whether it is weed or native
library(tidyr)
SurveyData_Combined <- SurveyData_united %>%
  unite(Plot, Plot, W_N, sep = " - ")

# Subset to only include seedlings
# I need to make this data only include seedlings. Therefore, I need to remove
# the rows that only include species > 51 cm.

SurveyData_Combined$seedlings <- SurveyData_Combined$Tier_1 - SurveyData_Combined$Tier_3

SurveyData_Combined <- subset(SurveyData_Combined, seedlings != 0)
# Now the data.frame only includes the seedlings.

#### Look at the top 20 most common "Scientific Names" ####
top_10_values <- names(sort(table(SurveyData_Combined$ScientificName), decreasing = TRUE))[1:10] 

print(top_10_values)

top_20_values <- names(sort(table(SurveyData_Combined$CommonName), decreasing = TRUE))[1:20] 

print(top_20_values)

top_counts <- SurveyData_Combined |>
  dplyr::count(ScientificName, sort = TRUE) |>
  dplyr::slice_head(n = 15)

print(top_counts)

# Subset to Native species
NativeSpecies <- SurveyData_Combined[SurveyData_Combined$CentralSpecies == "Native", ]

# Look at the top 20 most common Species under native species centred plots
top_20_valuesN <- names(sort(table(NativeSpecies$ScientificName), decreasing = TRUE))[1:20] 

print(top_20_valuesN)
top_20_valuesN <- names(sort(table(NativeSpecies$CommonName), decreasing = TRUE))[1:20] 

print(top_20_valuesN)

top_counts <- NativeSpecies |>
  dplyr::count(ScientificName, sort = TRUE) |>
  dplyr::slice_head(n = 15)

print(top_counts)


# Subset for S mauritianum
S_mauritianum <- SurveyData_Combined[SurveyData_Combined$CentralSpecies == "Solanum mauritianum", ]

# Look at the top 20 most common Species under native species centred plots
top_20_valuesWN <- names(sort(table(S_mauritianum$ScientificName), decreasing = TRUE))[1:20] 

print(top_20_valuesWN)

top_20_valuesWN <- names(sort(table(S_mauritianum$CommonName), decreasing = TRUE))[1:20] 

print(top_20_valuesWN)

top_counts <- S_mauritianum |>
  dplyr::count(ScientificName, sort = TRUE) |>
  dplyr::slice_head(n = 15)

print(top_counts)

# Subset for L. lucidum
L_lucidum <- SurveyData_Combined[SurveyData_Combined$CentralSpecies == "Ligustrum lucidum", ]

# Look at the top 20 most common Species under native species centred plots
top_20_valuesLL <- names(sort(table(L_lucidum$ScientificName), decreasing = TRUE))[1:20] 

print(top_20_valuesLL)

top_counts <- L_lucidum |>
  dplyr::count(ScientificName, sort = TRUE) |>
  dplyr::slice_head(n = 15)

print(top_counts)

# Subset for P. lophantha
P_lophantha <- SurveyData_Combined[SurveyData_Combined$CentralSpecies == "Paraserianthes lophantha", ]

# Look at the top 20 most common Species under native species centred plots
top_20_valuesPL <- names(sort(table(P_lophantha$ScientificName), decreasing = TRUE))[1:20] 

print(top_20_valuesPL)

top_counts <- P_lophantha |>
  dplyr::count(ScientificName, sort = TRUE) |>
  dplyr::slice_head(n = 15)

print(top_counts)


#### Look at the top 20 most common "Common Names" #####
top_20_values <- names(sort(table(SurveyData_Combined$CommonName), decreasing = TRUE))[1:20] 

print(top_20_values)

# Subset to Native species
NativeSpecies <- SurveyData_Combined[SurveyData_Combined$CentralSpecies == "Native", ]

# Look at the top 20 most common Species under native species centred plots
top_20_valuesN <- names(sort(table(NativeSpecies$CommonName), decreasing = TRUE))[1:20] 

print(top_20_valuesN)

# Subset for S mauritianum
S_mauritianum <- SurveyData_Combined[SurveyData_Combined$CentralSpecies == "Solanum mauritianum", ]

# Look at the top 20 most common Species under native species centred plots
top_20_valuesWN <- names(sort(table(S_mauritianum$CommonName), decreasing = TRUE))[1:20] 

print(top_20_valuesWN)

# Subset for L. lucidum
L_lucidum <- SurveyData_Combined[SurveyData_Combined$CentralSpecies == "Ligustrum lucidum", ]

# Look at the top 20 most common Species under native species centred plots
top_20_valuesLL <- names(sort(table(L_lucidum$CommonName), decreasing = TRUE))[1:20] 

print(top_20_valuesLL)

# Subset for P. lophantha
P_lophantha <- SurveyData_Combined[SurveyData_Combined$CentralSpecies == "Paraserianthes lophantha", ]

# Look at the top 20 most common Species under native species centred plots
top_20_valuesPL <- names(sort(table(P_lophantha$CommonName), decreasing = TRUE))[1:20] 

print(top_20_valuesPL)


#### have a look at how many are environmental weeds in each ####

# Subset to Native species
NativeSpecies <- SurveyData_Combined[SurveyData_Combined$CentralSpecies == "Native", ]

# Look at the top 20 most common Species under native species centred plots
top_20_valuesN <- names(sort(table(NativeSpecies$CommonName), decreasing = TRUE))[1:20]

# Subset to top 20 species in native centred plots
Native <- NativeSpecies[NativeSpecies$CommonName %in% top_20_valuesN, ] 

# Only unique values
Unique <- 

count_status_1 <- sum(Native$Status == 1)

print(count_status_1)
