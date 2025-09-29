#### Biomass and Growth Rate for each plant by treatment group v2####


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

library(readr)
Biomass <- read_csv("Shadehouse/Biomass Experiment 1 Clean.csv")

library(dplyr)

# Removing rows where the species is blank
Biomass<-Biomass[-592, ] 
Biomass<-Biomass[-591, ] 
Biomass<-Biomass[-590, ] 
Biomass<-Biomass[-961, ] 
Biomass<-Biomass[-960, ] 
Biomass<-Biomass[-959, ] 

# Removing rows where the mass is blank
Biomass <- Biomass[!is.na(Biomass$Mass_g), ]

# Remove seedling from tg2, there should be no seedlings in that tg
Biomass<-Biomass[-63, ]

# simplifying the experiment 1 treatment group name to Group and Mass_g to Mass
colnames(Biomass)[5:5] <- c("Group") ## Renaming the columns
colnames(Biomass)[10:10] <- c("Mass") ## Renaming the columns
Biomass$Group <- as.factor(Biomass$Group)

# Removing empty columns
Biomass <- Biomass[, -c(11, 12)]

# ggfortify is a package that works with ggplot2 to make nice plots
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
library("multcomp")
# nlme is used for random effects ANOVA
library("nlme")

#### Have a look at how sapling varies by treatment group ####
Sapling <- Biomass[Biomass$Plant == "ManukaSapling", ]

Sapling$Group <- as.factor(Sapling$Group)

# Wattle monoculture vs the rest
# Need to make a new column that is the average of treatment groups 2,4,5
library(dplyr)

Sapling <- Sapling %>%
  mutate(group_label = case_when(
    Group == 7 ~ "1",
    Group %in% c(2, 4, 5) ~ "2",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# From here, I need to compare the average sapling biomass values for wattlemono
# and wattleco

# Remove NA values
SaplingWattle <- Sapling %>%
  filter(!is.na(group_label))

ggplot(SaplingWattle, aes(x = group_label, y = Mass))+
  geom_boxplot() +
  theme_bw() 
ggplot(SaplingWattle) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SaplingWattle)+
  geom_qq(aes(sample = Mass, color = group_label))

# There are not enough mānuka in group 7, most of them must have survived, so 
# will have to just do RGR and survival for this

# Sapling biomass for monoculture woolly nightshade vs cooccurring woolly
library(dplyr)

Sapling <- Sapling %>%
  mutate(group_label = case_when(
    Group == 6 ~ "WoollyMonoculture",
    Group %in% c(2, 3, 4) ~ "WoollyCooccur",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
SaplingWoolly <- Sapling %>%
  filter(!is.na(group_label))

ggplot(SaplingWoolly, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "#CF597E", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling Biomass") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingWoolly) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SaplingWoolly)+
  geom_qq(aes(sample = Mass, color = group_label))

# Performing the t-test
t.test(Mass ~ group_label, data = SaplingWoolly, var.equal = TRUE)
# did not find a difference
# Welch's t-test
t.test(Mass ~ group_label, data = SaplingWoolly, var.equal = FALSE)

# Sapling biomass for monoculture Privet vs co-occurring privets
library(dplyr)

Sapling <- Sapling %>%
  mutate(group_label = case_when(
    Group == 8 ~ "PrivetMonoculture",
    Group %in% c(2, 3, 5) ~ "PrivetCooccur",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
SaplingPrivet <- Sapling %>%
  filter(!is.na(group_label))

ggplot(SaplingPrivet, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "lightblue3", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling Biomass") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingPrivet) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)


# comparing privet to woolly monoculture
library(dplyr)

Sapling <- Sapling %>%
  mutate(group_label = case_when(
    Group == 8 ~ "PrivetMonoculture",
    Group %in% c(6) ~ "WoollyMonoculture",
    TRUE ~ NA_character_  # Optional: handles other values
  ))
# Remove NA values
SaplingWoollyPrivet <- Sapling %>%
  filter(!is.na(group_label))

ggplot(SaplingWoollyPrivet, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling Biomass") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingWoollyPrivet) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)

# Performing the t-test
t.test(Mass ~ group_label, data = SaplingWoollyPrivet, var.equal = TRUE)
# did not find a difference
# Welch's t-test
t.test(Mass ~ group_label, data = SaplingWoollyPrivet, var.equal = FALSE)


#### Growth Rate ####
library(readr)
Height <- read_csv("Shadehouse/Height Experiment 1 Clean.csv")

# simplifying the experiment 1 treatment group name to Group and Mass_g to Mass
colnames(Height)[4:4] <- c("Group") ## Renaming the columns
Height$Group <- as.factor(Height$Group)

Height$Height1 <- as.numeric(Height$Height1)
Height$Height2 <- as.numeric(Height$Height2)
Height$Height3 <- as.numeric(Height$Height3)
Height$Height4 <- as.numeric(Height$Height4)
Height$Height5 <- as.numeric(Height$Height5)
Height$Height6 <- as.numeric(Height$Height6)
Height$Height7 <- as.numeric(Height$Height7)

# Remove rows with seedlings in treatment group 2
Height<-Height[-610, ]
Height<-Height[-82, ]

# clean up notes
Height$Notes2[Height$Notes2 == "dead, fell out"] <- "dead"
Height$Notes3[Height$Notes3 == "dead, fell out"] <- "dead"
Height$Notes4[Height$Notes4 == "dead, fell out"] <- "dead"
Height$Notes5[Height$Notes5 == "dead, fell out"] <- "dead"
Height$Notes6[Height$Notes6 == "dead, fell out"] <- "dead"
Height$Notes7[Height$Notes7 == "dead, fell out"] <- "dead"

Height$Notes2[Height$Notes2 == "dead, pulled out"] <- "dead"
Height$Notes3[Height$Notes3 == "dead, pulled out"] <- "dead"
Height$Notes4[Height$Notes4 == "dead, pulled out"] <- "dead"
Height$Notes5[Height$Notes5 == "dead, pulled out"] <- "dead"
Height$Notes6[Height$Notes6 == "dead, pulled out"] <- "dead"
Height$Notes7[Height$Notes7 == "dead, pulled out"] <- "dead"

Height$Notes2[Height$Notes2 == "dead, broken"] <- "dead"
Height$Notes3[Height$Notes3 == "dead, broken"] <- "dead"
Height$Notes4[Height$Notes4 == "dead, broken"] <- "dead"
Height$Notes5[Height$Notes5 == "dead, broken"] <- "dead"
Height$Notes6[Height$Notes6 == "dead, broken"] <- "dead"
Height$Notes7[Height$Notes7 == "dead, broken"] <- "dead"

Height$Notes2[Height$Notes2 == "dead and broken"] <- "dead"
Height$Notes3[Height$Notes3 == "dead and broken"] <- "dead"
Height$Notes4[Height$Notes4 == "dead and broken"] <- "dead"
Height$Notes5[Height$Notes5 == "dead and broken"] <- "dead"
Height$Notes6[Height$Notes6 == "dead and broken"] <- "dead"
Height$Notes7[Height$Notes7 == "dead and broken"] <- "dead"


# Remove height measurements if notes = dead for first GR

Height <- Height %>%
  mutate(GR1 = if_else(is.na(Notes2) | Notes2 != "dead", log(Height2) - log(Height1), NA_real_))
Height <- Height %>%
  mutate(GR2 = if_else(is.na(Notes3) | Notes3 != "dead", log(Height3) - log(Height2), NA_real_))
Height <- Height %>%
  mutate(GR3 = if_else(is.na(Notes4) | Notes4 != "dead", log(Height4) - log(Height3), NA_real_))
Height <- Height %>%
  mutate(GR4 = if_else(is.na(Notes5) | Notes5 != "dead", log(Height5) - log(Height4), NA_real_))
Height <- Height %>%
  mutate(GR5 = if_else(is.na(Notes6) | Notes6 != "dead", log(Height6) - log(Height5), NA_real_))
Height <- Height %>%
  mutate(GR6 = if_else(is.na(Notes7) | Notes7 != "dead", log(Height7) - log(Height6), NA_real_))


# Calculate average growth rate for each plant
Height$AverageGR <- rowMeans(Height[, 26:31], na.rm = TRUE)
Height$AverageGR <- Height$AverageGR * 10

# Clean up empty rows
library(dplyr)

Height <- Height %>% 
  filter(!is.na(AverageGR))

Height <- Height %>% 
  filter(!is.na(Group))


#### Have a look at how sapling varies by treatment group ####
Sapling <- Biomass[Biomass$Plant == "ManukaSapling", ]



