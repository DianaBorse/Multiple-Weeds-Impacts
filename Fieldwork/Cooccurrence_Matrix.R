#### Co-occurrence Matrix Code ####

# Before anything else, verify that your environment is totally clear.
# This is important, because old objects can foul up the works
# Clean up the working environment
rm(list = ls())

# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# At the beginning of a script, it is useful to make sure you have
# downloaded and installed all necessary packages.

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(tidyverse)){install.packages("Hmisc")}
if(!require(tidyverse)){install.packages("ggfortify")}
if(!require(tidyverse)){install.packages("multcomp")}
if(!require(tidyverse)){install.packages("nlme")}
if(!require(tidyverse)){install.packages("broom")}
if(!require(tidyverse)){install.packages("ggmosaic")}
if(!require(tidyverse)){install.packages("epitools")}
if(!require(tidyverse)){install.packages("swirl")}
if(!require(tidyverse)){install.packages("corrplot")}


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
SurveyData_united <- SurveyData %>%
  unite(Plot, Site, Plot, sep = " - ")

# Unite Plot and Weed vs Native columns so that now the plot id is all in one
# column that includes the site, plot number, and whether it is weed or native

library(tidyr)
SurveyData_Combined <- SurveyData_united %>%
  unite(Plot, Plot, W_N, sep = " - ")

# I need to make this data only include seedlings. Therefore, I need to remove
# the rows that only include species > 51 cm.

SurveyData_Combined$seedlings <- SurveyData_Combined$Tier_1 - SurveyData_Combined$Tier_3

SurveyData_Combined <- subset(SurveyData_Combined, seedlings != 0)
# Now the data.frame only includes the seedlings.


# Next I need to give each plot a unique numerical ID because the co-occurrence 
# matrix requires this

SurveyData_Combined$Plot <- as.numeric(as.factor(SurveyData_Combined$Plot))

# I need to subset to just the most common species because I have too many species
# to run the co-occurrence for

# This shows me what the most common species are but does not subset my data

# SurveyData_Combined_Subset <- SurveyData_Combined %>%
#  count(ScientificName) %>%
#  top_n(15) %>%
#  arrange(n, ScientificName) %>%
#  mutate(ScientificName = factor(ScientificName, levels = unique(ScientificName)))

# That only yields a result with the top occurring species and their count



#### Co-occurrence Matrix with top 15 most commonly occurring scientific name ####


# This includes both weeds and native species in each plot

#  I need to subset my whole data frame to only include the rows with the most 
# commonly occurring scientific name


# Identify the top 15 most common values in the categorical column 
top_15_values <- names(sort(table(SurveyData_Combined$ScientificName), decreasing = TRUE))[1:15] 

# find wattle position (it is 17th most common)
top_20_values <- names(sort(table(SurveyData_Combined$ScientificName), decreasing = TRUE))[1:20] 

print(top_20_values)
 
# Subset the data frame to include only rows with the top 15 most common values 
subset_SurveyData_Combined <- SurveyData_Combined[SurveyData_Combined$ScientificName %in% top_15_values, ] 
# View the subset data frame 
print(subset_SurveyData_Combined)


# My data needs to change to presence/absence by plot. Plot needs to be the 
# columns, and all species are rows with presence/absence denoted as a 0 or a 1

# The following are lines of code to make a tibble, but I don't think that's as
# useful as a dataframe will be


# PresenceAbsence <-SurveyData_Combined %>%
# rbind(as.matrix(SurveyData_Combined[, 2]), as.matrix(SurveyData_Combined[, -1])) %>%
# as_tibble() %>% 
# distinct() %>% 
# na.omit()

# This creates a new data frame that is presence/absence

library(tidyr)
library(dplyr)

PresenceAbsence <-subset_SurveyData_Combined %>%
  pivot_wider(id_cols = ScientificName, names_from=Plot, values_from=Plot,
              values_fn=function(x) any(unique(x) == x) * 1, values_fill = 0)

# instead of being a tibble, I wanted to convert it back to a data frame
PresenceAbsence_df = as.data.frame(PresenceAbsence)


# Needs to remove the first column of numbers as row names and make the Scientific 
# names of species into the row names
row.names(PresenceAbsence_df) <- PresenceAbsence_df$ScientificName 
# Remove the first column from the data frame 
PresenceAbsence_df <- PresenceAbsence_df[, -1]


## Co-occur

# with practice data

# data(finches)

# cooccur.finches <- cooccur(finches,
#          type = "spp_site",
#            thresh = TRUE,
#            spp_names = TRUE)
# class(cooccur.finches)


# with my data

# install.packages("cooccur")
library(cooccur)

cooccur.Survey <- cooccur(PresenceAbsence_df,
                         type = "spp_site",
                          thresh = TRUE,
                             spp_name = TRUE)
class(cooccur.Survey)

summary(cooccur.Survey)
cooccur(mat = PresenceAbsence_df, type = "spp_site", thresh = TRUE, spp_names = TRUE)

Prob_table <- prob.table(cooccur.Survey)

# plot the co-occurrence matrix, need to make the plot into an object so that the legend can be removed

plot <- plot(cooccur.Survey, legend = NULL) # add "plotrand = TRUE" to include completely random species

# Remove legend 
plot + theme(legend.position = "none")

#### Co-occurrence matrix with top 15 most common non-native species ####

# Subset the data to only include species that are weeds using the Status column
subset_SurveyData_Combined_weeds <- SurveyData_Combined[SurveyData_Combined$Status == 1, ]


# Identify the top 15 most common values in the categorical column 
top_15_values <- names(sort(table(subset_SurveyData_Combined_weeds$ScientificName), decreasing = TRUE))[1:15] 

# Subset the data frame to include only rows with the top 15 most common values 
subset_SurveyData_Combined_weeds10 <- SurveyData_Combined[SurveyData_Combined$ScientificName %in% top_15_values, ] 



# My data needs to change to presence/absence by plot. Plot needs to be the 
# columns, and all species are rows with presence/absence denoted as a 0 or a 1

# This creates a new data frame that is presence/absence

library(tidyr)
library(dplyr)

PresenceAbsence <-subset_SurveyData_Combined_weeds10 %>%
  pivot_wider(id_cols = ScientificName, names_from=Plot, values_from=Plot,
              values_fn=function(x) any(unique(x) == x) * 1, values_fill = 0)

# instead of being a tibble, I wanted to convert it back to a data frame
PresenceAbsence_df = as.data.frame(PresenceAbsence)

# Needs to remove the first column of numbers as row names and make the Scientific 
# names of species into the row names
row.names(PresenceAbsence_df) <- PresenceAbsence_df$ScientificName

# Remove the first column from the data frame 
PresenceAbsence_df <- PresenceAbsence_df[, -1]


## Co-occur 

# install.packages("cooccur")
library(cooccur)

cooccur.Survey <- cooccur(PresenceAbsence_df,
                          type = "spp_site",
                          thresh = TRUE,
                          spp_name = TRUE)
class(cooccur.Survey)

summary(cooccur.Survey)
cooccur(mat = PresenceAbsence_df, type = "spp_site", thresh = TRUE, spp_names = TRUE)

Prob_table <- prob.table(cooccur.Survey)

plot <- plot(cooccur.Survey) # add "plotrand = TRUE" to include completely random specie

# Remove legend 
plot + theme(legend.position = "none") + ggtitle(NULL)


Woolly_Cooccur <- pair(mod = cooccur.Survey, spp = "Solanum mauritianum")

pair(mod = cooccur.Survey, spp = "Ligustrum lucidum")

pair(mod = cooccur.Survey, spp = "Paraserianthes lophantha")


#### Co-occurrence matrix with top 15 most common environmental weeds ####

# Subset the data to only include species that are weeds using the WeedList Column
subset_SurveyData_Combined_weeds <- SurveyData_Combined[SurveyData_Combined$WeedList == 1, ]


# Identify the top 15 most common values in the categorical column 
top_15_values <- names(sort(table(subset_SurveyData_Combined_weeds$ScientificName), decreasing = TRUE))[1:15] 

# view(top_15_values)

# Subset the data frame to include only rows with the top 15 most common values 
subset_SurveyData_Combined_weeds15 <- SurveyData_Combined[SurveyData_Combined$ScientificName %in% top_15_values, ] 



# My data needs to change to presence/absence by plot. Plot needs to be the 
# columns, and all species are rows with presence/absence denoted as a 0 or a 1

# This creates a new data frame that is presence/absence

library(tidyr)
library(dplyr)

PresenceAbsence <-subset_SurveyData_Combined_weeds15 %>%
  pivot_wider(id_cols = ScientificName, names_from=Plot, values_from=Plot,
              values_fn=function(x) any(unique(x) == x) * 1, values_fill = 0)

# instead of being a tibble, I wanted to convert it back to a data frame
PresenceAbsence_df = as.data.frame(PresenceAbsence)

# Needs to remove the first column of numbers as row names and make the Scientific 
# names of species into the row names
row.names(PresenceAbsence_df) <- PresenceAbsence_df$ScientificName

# Remove the first column from the data frame 
PresenceAbsence_df <- PresenceAbsence_df[, -1]


## Co-occur 

# install.packages("cooccur")
library(cooccur)

cooccur.Survey <- cooccur(PresenceAbsence_df,
                          type = "spp_site",
                          thresh = TRUE,
                          spp_name = TRUE)
class(cooccur.Survey)

summary(cooccur.Survey)
cooccur(mat = PresenceAbsence_df, type = "spp_site", thresh = TRUE, spp_names = TRUE)

Prob_table <- prob.table(cooccur.Survey)

plot <- plot(cooccur.Survey) # add "plotrand = TRUE" to include completely random species

# Remove legend 
plot + theme(legend.position = "none") + ggtitle(NULL)

Woolly_Cooccur <- pair(mod = cooccur.Survey, spp = "Solanum mauritianum")

pair(mod = cooccur.Survey, spp = "Ligustrum lucidum")

pair(mod = cooccur.Survey, spp = "Paraserianthes lophantha")

# Try subsetting to non-seedlings

SurveyData_Combined <- subset(SurveyData_Combined, Tier_3 != 0)
