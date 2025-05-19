#### Environmental Weed Abundance Code ####

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

# Subset data to only include environmental weeds
subset_SurveyDataWeeds <- SurveyData[SurveyData$WeedList == 1, ]

# Subset data to remove NA values
subset_SurveyDataWeedsNoNA <- subset_SurveyDataWeeds %>%
  filter(!is.na(Tier_1))

# Calculate Summarry Statistics
library(dplyr)
SummSurveyDataWeeds <- subset_SurveyDataWeedsNoNA %>%
  group_by(CentralSpecies) %>%
  summarise(mean_Tier_1 = mean(Tier_1),
            median_Tier_1 = median(Tier_1),
            IQR_Tier_1 = IQR(Tier_1),
            sd_Tier_1 = sd(Tier_1),
            var_Tier_1 = var(Tier_1),
            se_Tier_1 = sd(Tier_1)/sqrt(389))


# Visualize the data
EnvWeedPlot <- ggplot(data = subset_SurveyDataWeedsNoNA, 
                         aes(y = Tier_1, ##Change this to variable name
                             x = CentralSpecies)) + ##Change this to variable name
  geom_boxplot(aes(x = factor(CentralSpecies, level=c('Native', 'Paraserianthes lophantha', 'Ligustrum lucidum', 'Solanum mauritianum'))), fill = "chocolate3", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Abundance of Environmental Weeds") + xlab("Central Species") +   ##Change axis titles
  theme(axis.text.x=element_text(size=8, hjust = 0.5, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
EnvWeedPlot

# ANOVA
modelAbundanceWeed <- lm(Tier_1~CentralSpecies, data = subset_SurveyDataWeedsNoNA)

anova(modelAbundanceWeed)

# Does not appear to vary between the different central species. 
# I want to see if there is a difference simply between weed and native plots now

# Change weed names to just be weed
WeedvsNativeSimple <- subset_SurveyDataWeedsNoNA %>%
  mutate(CentralSpecies = if_else(CentralSpecies == "Native", "Native", "Weed"))

# Calculate Summary stats for that

library(dplyr)
SummWeedvsNativeSimple <- WeedvsNativeSimple %>%
  group_by(CentralSpecies) %>%
  summarise(mean_Tier_1 = mean(Tier_1),
            median_Tier_1 = median(Tier_1),
            IQR_Tier_1 = IQR(Tier_1),
            sd_Tier_1 = sd(Tier_1),
            var_Tier_1 = var(Tier_1),
            se_Tier_1 = sd(Tier_1)/sqrt(389))

# Visualize
EnvWeedPlot2 <- ggplot(data = WeedvsNativeSimple, 
                      aes(y = Tier_1, ##Change this to variable name
                          x = CentralSpecies)) + ##Change this to variable name
  geom_boxplot(aes(x = factor(CentralSpecies, level=c('Native', 'Weed'))), fill = "chocolate3", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Abundance of Environmental Weeds") + xlab("Central Species") +   ##Change axis titles
  theme(axis.text.x=element_text(size=8, hjust = 0.5, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
EnvWeedPlot2

# Still nothing interesting, too many outliers... could consider a transformation
# Let's look for normality
ggplot(WeedvsNativeSimple) +
  geom_histogram(aes(Tier_1), binwidth = 1)

ggplot(WeedvsNativeSimple) +
  geom_boxplot(aes(x = "", y = Tier_1))

# As expected, very much not normal
LogWeedvsNative <- WeedvsNativeSimple %>%
  mutate(ln_Tier_1 = log10(Tier_1))

# Check for normality of the transformed data
ggplot(LogWeedvsNative) +
  geom_histogram(aes(ln_Tier_1), binwidth = 1)

ggplot(LogWeedvsNative) +
  geom_boxplot(aes(x = "", y = ln_Tier_1))

# Better, but still not normal. 
# Have a look at the boxplots now
EnvWeedPlot3 <- ggplot(data = LogWeedvsNative, 
                       aes(y = ln_Tier_1, ##Change this to variable name
                           x = CentralSpecies)) + ##Change this to variable name
  geom_boxplot(aes(x = factor(CentralSpecies, level=c('Native', 'Weed'))), fill = "chocolate3", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Abundance of Environmental Weeds") + xlab("Central Species") +   ##Change axis titles
  theme(axis.text.x=element_text(size=8, hjust = 0.5, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
EnvWeedPlot3

# There is no difference.


# Now I will have a look and see if there is anything interesting from a paired comparison

# Combine the site and plot number 

library(tidyr)
SurveyData_united <- SurveyData %>%
  unite(Plot, Site, Plot, sep = " - ")

# now subset so that weedlist = 1 (only include environmental weeds)
SurveyDataWeeds <- SurveyData_united[SurveyData_united$WeedList == 1, ]

# Look at a paired T-test
W_values <- SurveyData_united$Tier_1[SurveyData_united$W_N == "W"]
N_values <- SurveyData_united$Tier_1[SurveyData_united$W_N == "N"]

paired_t_test <- t.test(W_values, N_values, paired = TRUE)

print(paired_t_test)

# This does not work because there are a different number of Tier_1 values 
# for each group... but what if I calculate an average first?

# Unite Plot and Weed vs Native columns so that now the plot id is all in one
# column that includes the site, plot number, and whether it is weed or native

library(tidyr)
SurveyData_Combined <- SurveyData_united %>%
  unite(Plot, Plot, W_N, sep = " - ")

# Add a new column for average Tier_1
SurveyData_Averages <- SurveyData_Combined %>%
  group_by(Plot) %>%
  mutate(avg_Tier_1 = mean(Tier_1))

