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

# Control vs pots with weeds
library(dplyr)

Sapling <- Sapling %>%
  mutate(group_label = case_when(
    Group == 1 ~ "Control",
    Group %in% c(2, 3, 4, 5, 6, 7, 8) ~ "Weeds",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
SaplingControl <- Sapling %>%
  filter(!is.na(group_label))

ggplot(SaplingControl, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling Biomass") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingControl) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SaplingControl)+
  geom_qq(aes(sample = Mass, color = group_label))

# Performing the t-test
t.test(Mass ~ group_label, data = SaplingControl, var.equal = TRUE)
# Welch's t-test
t.test(Mass ~ group_label, data = SaplingControl, var.equal = FALSE)

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

# comparing privet and woolly to all weeds
library(dplyr)

Sapling <- Sapling %>%
  mutate(group_label = case_when(
    Group == 3 ~ "PrivetWoolly",
    Group %in% c(2) ~ "AllWeeds",
    TRUE ~ NA_character_  # Optional: handles other values
  ))
# Remove NA values
SaplingWoollyPrivetAll <- Sapling %>%
  filter(!is.na(group_label))

ggplot(SaplingWoollyPrivetAll, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling Biomass") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingWoollyPrivetAll) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)

# Performing the t-test
t.test(Mass ~ group_label, data = SaplingWoollyPrivetAll, var.equal = TRUE)
# did not find a difference
# Welch's t-test
t.test(Mass ~ group_label, data = SaplingWoollyPrivetAll, var.equal = FALSE)

#### Seedling Biomass Patterns ####
Seedling <- Biomass[Biomass$Plant == "ManukaSeedling", ]

Seedling$Group <- as.factor(Seedling$Group)

# Woolly and Privet compared to Woolly and wattle
library(dplyr)

Seedling <- Seedling %>%
  mutate(group_label = case_when(
    Group == 3 ~ "WoollyPrivet",
    Group %in% c(4) ~ "WoollyWattle",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
SeedlingWoolly <- Seedling %>%
  filter(!is.na(group_label))

ggplot(SeedlingWoolly, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "#DAB1DA", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Seedling Biomass (g)") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SeedlingWoolly) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SeedlingWoolly)+
  geom_qq(aes(sample = Mass, color = group_label))

# Performing the t-test
t.test(Mass ~ group_label, data = SeedlingWoolly, var.equal = TRUE)
# Welch's t-test
t.test(Mass ~ group_label, data = SeedlingWoolly, var.equal = FALSE)

# Woolly and Privet compared to wattle and privet
library(dplyr)

Seedling <- Seedling %>%
  mutate(group_label = case_when(
    Group == 3 ~ "PrivetWoolly",
    Group %in% c(5) ~ "PrivetWattle",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
SeedlingPrivet <- Seedling %>%
  filter(!is.na(group_label))

ggplot(SeedlingPrivet, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "#DAB1DA", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Seedling Biomass (g)") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SeedlingPrivet) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SeedlingPrivet)+
  geom_qq(aes(sample = Mass, color = group_label))

# Performing the t-test
t.test(Mass ~ group_label, data = SeedlingPrivet, var.equal = TRUE)
# Welch's t-test
t.test(Mass ~ group_label, data = SeedlingPrivet, var.equal = FALSE)

# Woolly and Privet compared to wattle and privet
library(dplyr)

Seedling <- Seedling %>%
  mutate(group_label = case_when(
    Group == 4 ~ "WattleWoolly",
    Group %in% c(5) ~ "WattlePrivet",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
SeedlingWattle <- Seedling %>%
  filter(!is.na(group_label))

ggplot(SeedlingWattle, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "#DAB1DA", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Seedling Biomass (g)") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SeedlingWattle) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SeedlingWattle)+
  geom_qq(aes(sample = Mass, color = group_label))

# Performing the t-test
t.test(Mass ~ group_label, data = SeedlingWattle, var.equal = TRUE)
# Welch's t-test
t.test(Mass ~ group_label, data = SeedlingWattle, var.equal = FALSE)


#### Wattle biomass patterns ####
Wattle <- Biomass[Biomass$Plant == "Wattle", ]

Wattle$Group <- as.factor(Wattle$Group)

# Wattle monoculture vs the rest
# Need to make a new column that is the average of treatment groups 2,4,5
library(dplyr)

Wattle <- Wattle %>%
  mutate(group_label = case_when(
    Group == 7 ~ "WattleMonoculture",
    Group %in% c(2, 4, 5) ~ "WattleCooccur",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
WattleMono <- Wattle %>%
  filter(!is.na(group_label))


ggplot(WattleMono, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Wattle Biomass (g)") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(WattleMono) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(WattleMono)+
  geom_qq(aes(sample = Mass, color = group_label))

# Performing the t-test
t.test(Mass ~ group_label, data = WattleMono, var.equal = TRUE)
# Welch's t-test
t.test(Mass ~ group_label, data = WattleMono, var.equal = FALSE)

# Now look at how wattle biomass varies between when it grows with privet vs monoculture
library(dplyr)

Wattle <- Wattle %>%
  mutate(group_label = case_when(
    Group == 7 ~ "WattleMonoculture",
    Group %in% c(5) ~ "WattlePrivet",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
WattlePrivet <- Wattle %>%
  filter(!is.na(group_label))

ggplot(WattlePrivet, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Wattle Biomass (g)") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(WattlePrivet) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(WattlePrivet)+
  geom_qq(aes(sample = Mass, color = group_label))

# Performing the t-test
t.test(Mass ~ group_label, data = WattlePrivet, var.equal = TRUE)
# Welch's t-test
t.test(Mass ~ group_label, data = WattlePrivet, var.equal = FALSE)

# Now look at how wattle biomass varies between when it grows with privet vs woolly
library(dplyr)

Wattle <- Wattle %>%
  mutate(group_label = case_when(
    Group == 4 ~ "WattleWoolly",
    Group %in% c(5) ~ "WattlePrivet",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
WattlePrivetWoolly <- Wattle %>%
  filter(!is.na(group_label))

ggplot(WattlePrivetWoolly, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Wattle Biomass (g)") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(WattlePrivetWoolly) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(WattlePrivetWoolly)+
  geom_qq(aes(sample = Mass, color = group_label))

# Performing the t-test
t.test(Mass ~ group_label, data = WattlePrivetWoolly, var.equal = TRUE)
# Welch's t-test
t.test(Mass ~ group_label, data = WattlePrivetWoolly, var.equal = FALSE)

#### Woolly biomass patterns ####
Woolly <- Biomass[Biomass$Plant == "Nightshade", ]

Woolly$Group <- as.factor(Woolly$Group)

# Woolly monoculture vs the rest
# Need to make a new column that is the average of treatment groups 2,4,5
library(dplyr)

Woolly <- Woolly %>%
  mutate(group_label = case_when(
    Group == 6 ~ "WoollyMonoculture",
    Group %in% c(2, 3, 4) ~ "WoollyCooccur",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
WoollyMono <- Woolly %>%
  filter(!is.na(group_label))


ggplot(WoollyMono, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "#CF597E", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Woolly Biomass (g)") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(WoollyMono) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(WoollyMono)+
  geom_qq(aes(sample = Mass, color = group_label))

# Performing the t-test
t.test(Mass ~ group_label, data = WoollyMono, var.equal = TRUE)
# Welch's t-test
t.test(Mass ~ group_label, data = WoollyMono, var.equal = FALSE)

library(dplyr)

Woolly <- Woolly %>%
  mutate(group_label = case_when(
    Group == 3 ~ "WoollyPrivet",
    Group %in% c(4) ~ "WoollyWattle",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
WoollyWattlePrivet <- Woolly %>%
  filter(!is.na(group_label))


ggplot(WoollyWattlePrivet, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "#CF597E", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Woolly Biomass (g)") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(WoollyWattlePrivet) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(WoollyWattlePrivet)+
  geom_qq(aes(sample = Mass, color = group_label))

# Performing the t-test
t.test(Mass ~ group_label, data = WoollyWattlePrivet, var.equal = TRUE)
# Welch's t-test
t.test(Mass ~ group_label, data = WoollyWattlePrivet, var.equal = FALSE)

#### Privet biomass patterns ####
Privet <- Biomass[Biomass$Plant == "Privet", ]

Privet$Group <- as.factor(Privet$Group)

# Privet monoculture vs the rest
# Need to make a new column that is the average of treatment groups 2,4,5
library(dplyr)

Privet <- Privet %>%
  mutate(group_label = case_when(
    Group == 8 ~ "PrivetMonoculture",
    Group %in% c(2, 3, 5) ~ "PrivetCooccur",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
PrivetMono <- Privet %>%
  filter(!is.na(group_label))


ggplot(PrivetMono, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "orange2", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Privet Biomass (g)") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(PrivetMono) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(PrivetMono)+
  geom_qq(aes(sample = Mass, color = group_label))

# Performing the t-test
t.test(Mass ~ group_label, data = PrivetMono, var.equal = TRUE)
# Welch's t-test
t.test(Mass ~ group_label, data = PrivetMono, var.equal = FALSE)

library(dplyr)

Privet <- Privet %>%
  mutate(group_label = case_when(
    Group == 8 ~ "PrivetMonoculture",
    Group %in% c(5) ~ "PrivetWattle",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
PrivetWattle <- Privet %>%
  filter(!is.na(group_label))


ggplot(PrivetWattle, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "orange2", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Privet Biomass (g)") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(PrivetWattle) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(PrivetWattle)+
  geom_qq(aes(sample = Mass, color = group_label))

# Performing the t-test
t.test(Mass ~ group_label, data = PrivetWattle, var.equal = TRUE)
# Welch's t-test
t.test(Mass ~ group_label, data = PrivetWattle, var.equal = FALSE)

library(dplyr)

Privet <- Privet %>%
  mutate(group_label = case_when(
    Group == 3 ~ "PrivetWoolly",
    Group %in% c(5) ~ "PrivetWattle",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
PrivetWattleWoolly <- Privet %>%
  filter(!is.na(group_label))


ggplot(PrivetWattleWoolly, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "orange2", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Privet Biomass (g)") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(PrivetWattleWoolly) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(PrivetWattleWoolly)+
  geom_qq(aes(sample = Mass, color = group_label))

# Performing the t-test
t.test(Mass ~ group_label, data = PrivetWattleWoolly, var.equal = TRUE)
# Welch's t-test
t.test(Mass ~ group_label, data = PrivetWattleWoolly, var.equal = FALSE)


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


#### Have a look at how sapling RGR varies by treatment group ####
#### Let's look at Sapling height ####
SaplingH <- Height[Height$Plant == "ManukaSapling", ]

# Look at control vs groups that have weeds
SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 1 ~ "Control",
    Group %in% c(2, 3, 4, 5, 6, 7, 8) ~ "Weeds",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
SaplingControl <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingControl,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingControl) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SaplingControl)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingControl, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingControl, var.equal = FALSE)

# compare wattle monoculture to cooccurring wattle pots

SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 7 ~ "WattleMonoculture",
    Group %in% c(2, 4, 5) ~ "WattleCooccur",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# From here, I need to compare the average sapling biomass values for wattlemono
# and wattleco

# Remove NA values
SaplingWattle <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingWattle,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingWattle) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SaplingWattle)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWattle, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingWattle, var.equal = FALSE)

# woolly monoculture vs woolly cooccurence

SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 6 ~ "WoollyMonoculture",
    Group %in% c(2, 3, 4) ~ "WoollyCooccur",
    TRUE ~ NA_character_  # Optional: handles other values
  ))


# Remove NA values
SaplingWoolly <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingWoolly,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingWoolly) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SaplingWoolly)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWoolly, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingWoolly, var.equal = FALSE)

# privet monoculture vs woolly cooccurence

SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 8 ~ "PrivetMonoculture",
    Group %in% c(2, 3, 5) ~ "PrivetCooccur",
    TRUE ~ NA_character_  # Optional: handles other values
  ))


# Remove NA values
SaplingPrivet <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingPrivet,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingPrivet) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SaplingPrivet)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingPrivet, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingPrivet, var.equal = FALSE)

# wattle monoculture vs woolly monoculture

SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 7 ~ "WattleMonoculture",
    Group %in% c(6) ~ "WoollyMonoculture",
    TRUE ~ NA_character_  # Optional: handles other values
  ))


# Remove NA values
SaplingWattleWoolly <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingWattleWoolly,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingWattleWoolly) +
  geom_histogram(aes(AverageGR), binwidth = .1)+
  facet_wrap(~group_label)
ggplot(SaplingWattleWoolly)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWattleWoolly, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingWattleWoolly, var.equal = FALSE)

# privet monoculture vs woolly monoculture

SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 8 ~ "PrivetMonoculture",
    Group %in% c(6) ~ "WoollyMonoculture",
    TRUE ~ NA_character_  # Optional: handles other values
  ))


# Remove NA values
SaplingPrivetWoolly <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingPrivetWoolly,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingPrivetWoolly) +
  geom_histogram(aes(AverageGR), binwidth = .1)+
  facet_wrap(~group_label)
ggplot(SaplingPrivetWoolly)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingPrivetWoolly, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingPrivetWoolly, var.equal = FALSE)

# Wattle + Privet vs All weeds
SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 5 ~ "PrivetWattle",
    Group %in% c(2) ~ "AllWeeds",
    TRUE ~ NA_character_  # Optional: handles other values
  ))


# Remove NA values
SaplingWattlePrivetAll <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingWattlePrivetAll,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingWattlePrivetAll) +
  geom_histogram(aes(AverageGR), binwidth = .1)+
  facet_wrap(~group_label)
ggplot(SaplingWattlePrivetAll)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWattlePrivetAll, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingWattlePrivetAll, var.equal = FALSE)

# Wattle + Wooolly vs All weeds
SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 4 ~ "WoollyWattle",
    Group %in% c(2) ~ "AllWeeds",
    TRUE ~ NA_character_  # Optional: handles other values
  ))


# Remove NA values
SaplingWattleWoollyAll <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingWattleWoollyAll,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingWattleWoollyAll) +
  geom_histogram(aes(AverageGR), binwidth = .1)+
  facet_wrap(~group_label)
ggplot(SaplingWattleWoollyAll)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWattleWoollyAll, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingWattleWoollyAll, var.equal = FALSE)

# Privet + Woolly vs All weeds
SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 3 ~ "PrivetWoolly",
    Group %in% c(2) ~ "AllWeeds",
    TRUE ~ NA_character_  # Optional: handles other values
  ))


# Remove NA values
SaplingPrivetWoollyAll <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingPrivetWoollyAll,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingPrivetWoollyAll) +
  geom_histogram(aes(AverageGR), binwidth = .1)+
  facet_wrap(~group_label)
ggplot(SaplingPrivetWoollyAll)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingPrivetWoollyAll, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingPrivetWoollyAll, var.equal = FALSE)

# Privet + Wattle vs Wattle monoculture
SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 7 ~ "WattleMonoculture",
    Group %in% c(5) ~ "WattlePrivet",
    TRUE ~ NA_character_  # Optional: handles other values
  ))


# Remove NA values
SaplingWattleMonoPrivet <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingWattleMonoPrivet,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingWattleMonoPrivet) +
  geom_histogram(aes(AverageGR), binwidth = .1)+
  facet_wrap(~group_label)
ggplot(SaplingWattleMonoPrivet)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWattleMonoPrivet, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingWattleMonoPrivet, var.equal = FALSE)

# Privet + Wattle vs Privet monoculture
SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 8 ~ "PrivetMonoculture",
    Group %in% c(5) ~ "WattlePrivet",
    TRUE ~ NA_character_  # Optional: handles other values
  ))


# Remove NA values
SaplingPrivetMonoWattle <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingPrivetMonoWattle,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingPrivetMonoWattle) +
  geom_histogram(aes(AverageGR), binwidth = .1)+
  facet_wrap(~group_label)
ggplot(SaplingPrivetMonoWattle)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingPrivetMonoWattle, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingPrivetMonoWattle, var.equal = FALSE)

# Woolly + Wattle vs Privet + wattle
SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 4 ~ "WoollyWattle",
    Group %in% c(5) ~ "WattlePrivet",
    TRUE ~ NA_character_  # Optional: handles other values
  ))


# Remove NA values
SaplingWattlePairs <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingWattlePairs,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingWattlePairs) +
  geom_histogram(aes(AverageGR), binwidth = .1)+
  facet_wrap(~group_label)
ggplot(SaplingWattlePairs)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWattlePairs, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingWattlePairs, var.equal = FALSE)

# Woolly + Wattle vs Privet + wattle
SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 3 ~ "WoollyPrivet",
    Group %in% c(5) ~ "WattlePrivet",
    TRUE ~ NA_character_  # Optional: handles other values
  ))


# Remove NA values
SaplingPrivetPairs <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingPrivetPairs,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingPrivetPairs) +
  geom_histogram(aes(AverageGR), binwidth = .1)+
  facet_wrap(~group_label)
ggplot(SaplingPrivetPairs)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingPrivetPairs, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingPrivetPairs, var.equal = FALSE)

# Woolly + Wattle vs Woolly + Privet
SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 3 ~ "WoollyPrivet",
    Group %in% c(4) ~ "WoollyWattle",
    TRUE ~ NA_character_  # Optional: handles other values
  ))


# Remove NA values
SaplingWoollyPairs <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingWoollyPairs,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingWoollyPairs) +
  geom_histogram(aes(AverageGR), binwidth = .1)+
  facet_wrap(~group_label)
ggplot(SaplingWoollyPairs)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWoollyPairs, var.equal = TRUE)
# Welch's t-test
t.test(AverageGR ~ group_label, data = SaplingWoollyPairs, var.equal = FALSE)
