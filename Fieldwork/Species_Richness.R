#### Species Richness ####

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

#### Calculating total species richness by central species ####

# calculate how many different values there are for ScientificName for each Central Species

library(dplyr)
Richness <- SurveyData_Combined %>% group_by(Plot) %>% summarize(unique_values = n_distinct(ScientificName))

SurveyData_Combined$Richness <- 
# Calculate Summarry Statistics
library(dplyr)
summ_Richness <- Richness %>%
  group_by(CentralSpecies) %>%
  summarise(mean_unique_values = mean(unique_values),
            median_unique_values = median(unique_values),
            IQR_unique_values = IQR(unique_values),
            sd_unique_values = sd(unique_values),
            var_unique_values = var(unique_values),
            se_unique_values = sd(unique_values)/sqrt(177))

# Check for normality
ggplot(Richness) +
  geom_histogram(aes(unique_values), binwidth = 1)

ggplot(Richness) +
  geom_boxplot(aes(x = "", y = unique_values))

# ANOVA
modelRichness <- lm(unique_values~CentralSpecies, data = Richness)

anova(modelRichness)

# make a boxplot

library(ggplot2)
RichnessPlot <- ggplot(data = Richness, 
                         aes(y = unique_values, ##Change this to variable name
                             x = CentralSpecies)) + ##Change this to variable name
  geom_boxplot(aes(x = factor(CentralSpecies, level=c('Native', 'Paraserianthes lophantha', 'Ligustrum lucidum', 'Solanum mauritianum'))), fill = "lightblue3", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Species Richness") + xlab("Central Species") +   ##Change axis titles
  theme(axis.text.x=element_text(face = "italic", size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
RichnessPlot

#### Native Species Richness by Central species ####

# subset the data to only include the native speices
subset_SurveyData_Combined_Native <- SurveyData_Combined[SurveyData_Combined$Status == 0, ]

# calculate the richness (unique values for the ScientificName)
library(dplyr)
RichnessNative <- subset_SurveyData_Combined_Native %>% group_by(Plot, CentralSpecies) %>% summarize(unique_values = n_distinct(ScientificName))

# Calculate Summarry Statistics
library(dplyr)
summ_RichnessNative <- RichnessNative %>%
  group_by(CentralSpecies) %>%
  summarise(mean_unique_values = mean(unique_values),
            median_unique_values = median(unique_values),
            IQR_unique_values = IQR(unique_values),
            sd_unique_values = sd(unique_values),
            var_unique_values = var(unique_values),
            se_unique_values = sd(unique_values)/sqrt(158))

# Check for normality
ggplot(RichnessNative) +
  geom_histogram(aes(unique_values), binwidth = 1)

ggplot(RichnessNative) +
  geom_boxplot(aes(x = "", y = unique_values))

# ANOVA
modelRichnessNative <- lm(unique_values~CentralSpecies, data = RichnessNative)

anova(modelRichnessNative)


# make a boxplot
library(ggplot2)
RichnessPlotNative <- ggplot(data = RichnessNative, 
                       aes(y = unique_values, ##Change this to variable name
                           x = CentralSpecies)) + ##Change this to variable name
  geom_boxplot(aes(x = factor(CentralSpecies, level=c('Native', 'Paraserianthes lophantha', 'Ligustrum lucidum', 'Solanum mauritianum'))), fill = "green4", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Species Richness") + xlab("Central Species") +   ##Change axis titles
  theme(axis.text.x=element_text(face = "italic", size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
RichnessPlotNative

#### Non-Native Species Richness by Central species ####

# subset the data to only include the non-native species
subset_SurveyData_Combined_Weed <- SurveyData_Combined[SurveyData_Combined$WeedList == 1, ]

# calculate the species richness of the weeds
library(dplyr)
RichnessWeed <- subset_SurveyData_Combined_Weed %>% group_by(Plot, CentralSpecies) %>% summarize(unique_values = n_distinct(ScientificName))

# Calculate Summarry Statistics
library(dplyr)
summ_RichnessWeed <- RichnessWeed %>%
  group_by(CentralSpecies) %>%
  summarise(mean_unique_values = mean(unique_values),
            median_unique_values = median(unique_values),
            IQR_unique_values = IQR(unique_values),
            sd_unique_values = sd(unique_values),
            var_unique_values = var(unique_values),
            se_unique_values = sd(unique_values)/sqrt(27))


# Check for normality
ggplot(RichnessWeed) +
  geom_histogram(aes(unique_values), binwidth = 1)

ggplot(RichnessWeed) +
  geom_boxplot(aes(x = "", y = unique_values))

# Try a transformation
RichnessWeed$unique_values_log <- log10(RichnessWeed$unique_values)

# Check for normality
ggplot(RichnessWeed) +
  geom_histogram(aes(unique_values_log), binwidth = 0.1)

ggplot(RichnessWeed) +
  geom_boxplot(aes(x = "", y = unique_values_log))

# ANOVA
modelRichnessWeed <- lm(unique_values_log~CentralSpecies, data = RichnessWeed)

anova(modelRichnessWeed)

# make a boxplot for visualization
library(ggplot2)
RichnessPlotWeed <- ggplot(data = RichnessWeed, 
                             aes(y = unique_values_log, ##Change this to variable name
                                 x = CentralSpecies)) + ##Change this to variable name
  geom_boxplot(aes(x = factor(CentralSpecies, level=c('Native', 'Paraserianthes lophantha', 'Ligustrum lucidum', 'Solanum mauritianum'))), fill = "red4", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Weed Species Richness") + xlab("Central Species") +   ##Change axis titles
  theme(axis.text.x=element_text(face = "italic", size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
RichnessPlotWeed

?geom_boxplot

#### Look at just seedlings ####

# I need to make this data only include seedlings. Therefore, I need to remove
# the rows that only include species > 51 cm.

SurveyData_Combined$seedlings <- SurveyData_Combined$Tier_1 - SurveyData_Combined$Tier_3

Subset_SurveyData_Combined <- subset(SurveyData_Combined, seedlings != 0)

# subset the data to only include the non-native species
subset_SurveyData_Combined_Weed <- Subset_SurveyData_Combined[Subset_SurveyData_Combined$WeedList == 1, ]

# calculate the species richness of the weeds
library(dplyr)
RichnessWeed <- subset_SurveyData_Combined_Weed %>% group_by(Plot, CentralSpecies) %>% summarize(unique_values = n_distinct(ScientificName))

# Calculate Summarry Statistics
library(dplyr)
summ_RichnessWeed <- RichnessWeed %>%
  group_by(CentralSpecies) %>%
  summarise(mean_unique_values = mean(unique_values),
            median_unique_values = median(unique_values),
            IQR_unique_values = IQR(unique_values),
            sd_unique_values = sd(unique_values),
            var_unique_values = var(unique_values),
            se_unique_values = sd(unique_values)/sqrt(27))


# Check for normality
ggplot(RichnessWeed) +
  geom_histogram(aes(unique_values), binwidth = 1)

ggplot(RichnessWeed) +
  geom_boxplot(aes(x = "", y = unique_values))

# Try a transformation
RichnessWeed$unique_values_log <- log10(RichnessWeed$unique_values)

# Check for normality
ggplot(RichnessWeed) +
  geom_histogram(aes(unique_values_log), binwidth = 0.1)

ggplot(RichnessWeed) +
  geom_boxplot(aes(x = "", y = unique_values_log))

# ANOVA
modelRichnessWeed <- lm(unique_values_log~CentralSpecies, data = RichnessWeed)

anova(modelRichnessWeed)

# make a boxplot for visualization
library(ggplot2)
RichnessPlotWeed <- ggplot(data = RichnessWeed, 
                           aes(y = unique_values_log, ##Change this to variable name
                               x = CentralSpecies)) + ##Change this to variable name
  geom_boxplot(aes(x = factor(CentralSpecies, level=c('Native', 'Paraserianthes lophantha', 'Ligustrum lucidum', 'Solanum mauritianum'))), fill = "red4", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Weed Species Richness") + xlab("Central Species") +   ##Change axis titles
  theme(axis.text.x=element_text(face = "italic", size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
RichnessPlotWeed

?geom_boxplot
