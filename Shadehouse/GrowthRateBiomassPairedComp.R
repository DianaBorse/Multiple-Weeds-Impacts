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

# Check for homogeneous variance
summ_SaplingControl <- SaplingControl %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_SaplingControl$sd_Mass))/(min(summ_SaplingControl$sd_Mass))
print(ratio)

# ratio > 3 not normal, do welch's
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
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling Biomass") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingWoolly) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SaplingWoolly)+
  geom_qq(aes(sample = Mass, color = group_label))


# Check for homogeneous variance
summ_SaplingWoolly <- SaplingWoolly %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_SaplingWoolly$sd_Mass))/(min(summ_SaplingWoolly$sd_Mass))
print(ratio)

# Performing the t-test
t.test(Mass ~ group_label, data = SaplingWoolly, var.equal = TRUE)

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
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling Biomass") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingPrivet) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)

# Check for homogeneous variance
summ_SaplingPrivet <- SaplingPrivet %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_SaplingPrivet$sd_Mass))/(min(summ_SaplingPrivet$sd_Mass))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(Mass ~ group_label, data = SaplingWoolly, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SaplingWoollyPrivet <- SaplingWoollyPrivet %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_SaplingWoollyPrivet$sd_Mass))/(min(summ_SaplingWoollyPrivet$sd_Mass))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(Mass ~ group_label, data = SaplingWoollyPrivet, var.equal = TRUE)


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

# Check for homogeneous variance
summ_SaplingWoollyPrivetAll <- SaplingWoollyPrivetAll %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_SaplingWoollyPrivetAll$sd_Mass))/(min(summ_SaplingWoollyPrivetAll$sd_Mass))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(Mass ~ group_label, data = SaplingWoollyPrivetAll, var.equal = TRUE)

#### Seedling Biomass Patterns ####
Seedling <- Biomass[Biomass$Plant == "ManukaSeedling", ]

Seedling$Group <- as.factor(Seedling$Group)

# control vs weeds
library(dplyr)

Seedling <- Seedling %>%
  mutate(group_label = case_when(
    Group == 1 ~ "Control",
    Group %in% c(3, 4, 5) ~ "Weeds",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
SeedlingControl <- Seedling %>%
  filter(!is.na(group_label))

ggplot(SeedlingControl, aes(x = group_label, y = Mass))+
  geom_boxplot(fill = "#DAB1DA", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Seedling Biomass (g)") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SeedlingControl) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SeedlingControl)+
  geom_qq(aes(sample = Mass, color = group_label))

# Check for homogeneous variance
summ_SeedlingControl <- SeedlingControl %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_SeedlingControl$sd_Mass))/(min(summ_SeedlingControl$sd_Mass))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(Mass ~ group_label, data = SeedlingControl, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SeedlingWoolly <- SeedlingWoolly %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_SeedlingWoolly$sd_Mass))/(min(summ_SeedlingWoolly$sd_Mass))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(Mass ~ group_label, data = SeedlingWoolly, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SeedlingPrivet <- SeedlingPrivet %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_SeedlingPrivet$sd_Mass))/(min(summ_SeedlingPrivet$sd_Mass))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(Mass ~ group_label, data = SeedlingPrivet, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SeedlingWattle <- SeedlingWattle %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_SeedlingWattle$sd_Mass))/(min(summ_SeedlingWattle$sd_Mass))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(Mass ~ group_label, data = SeedlingWattle, var.equal = TRUE)

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

# Check for homogeneous variance
summ_WattleMono <- WattleMono %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_WattleMono$sd_Mass))/(min(summ_WattleMono$sd_Mass))
print(ratio)

# Ratio >> 3
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

# Check for homogeneous variance
summ_WattlePrivet <- WattlePrivet %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_WattlePrivet$sd_Mass))/(min(summ_WattlePrivet$sd_Mass))
print(ratio)

# Ratio >> 3
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

# Check for homogeneous variance
summ_WattlePrivetWoolly <- WattlePrivetWoolly %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_WattlePrivetWoolly$sd_Mass))/(min(summ_WattlePrivetWoolly$sd_Mass))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(Mass ~ group_label, data = WattlePrivetWoolly, var.equal = TRUE)

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

# Check for homogeneous variance
summ_WoollyMono <- WoollyMono %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_WoollyMono$sd_Mass))/(min(summ_WoollyMono$sd_Mass))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(Mass ~ group_label, data = WoollyMono, var.equal = TRUE)

# Woolly privet vs woolly wattle
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

# Check for homogeneous variance
summ_WoollyWattlePrivet <- WoollyWattlePrivet %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_WoollyWattlePrivet$sd_Mass))/(min(summ_WoollyWattlePrivet$sd_Mass))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(Mass ~ group_label, data = WoollyWattlePrivet, var.equal = TRUE)

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

# Check for homogeneous variance
summ_PrivetMono <- PrivetMono %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_PrivetMono$sd_Mass))/(min(summ_PrivetMono$sd_Mass))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(Mass ~ group_label, data = PrivetMono, var.equal = TRUE)

# Privet monoculture vs Privet and Wattle
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

# Check for homogeneous variance
summ_PrivetWattle <- PrivetWattle %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_PrivetWattle$sd_Mass))/(min(summ_PrivetWattle$sd_Mass))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(Mass ~ group_label, data = PrivetWattle, var.equal = TRUE)

# Privet pair comparisons
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

# Check for homogeneous variance
summ_PrivetWattleWoolly <- PrivetWattleWoolly %>%
  group_by(group_label) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_PrivetWattleWoolly$sd_Mass))/(min(summ_PrivetWattleWoolly$sd_Mass))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(Mass ~ group_label, data = PrivetWattleWoolly, var.equal = TRUE)


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

# Check for homogeneous variance
summ_SaplingControl <- SaplingControl %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingControl$sd_AverageGR))/(min(summ_SaplingControl$sd_AverageGR))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingControl, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SaplingWattle <- SaplingWattle %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingWattle$sd_AverageGR))/(min(summ_SaplingWattle$sd_AverageGR))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWattle, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SaplingWoolly <- SaplingWoolly %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingWoolly$sd_AverageGR))/(min(summ_SaplingWoolly$sd_AverageGR))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWoolly, var.equal = TRUE)

# privet monoculture vs privet cooccurence
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

# Check for homogeneous variance
summ_SaplingPrivet <- SaplingPrivet %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingPrivet$sd_AverageGR))/(min(summ_SaplingPrivet$sd_AverageGR))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingPrivet, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SaplingWattleWoolly <- SaplingWattleWoolly %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingWattleWoolly$sd_AverageGR))/(min(summ_SaplingWattleWoolly$sd_AverageGR))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWattleWoolly, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SaplingPrivetWoolly <- SaplingPrivetWoolly %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingPrivetWoolly$sd_AverageGR))/(min(summ_SaplingPrivetWoolly$sd_AverageGR))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingPrivetWoolly, var.equal = TRUE)

# Privet monoculture vs wattle monoculture
SaplingH <- SaplingH %>%
  mutate(group_label = case_when(
    Group == 8 ~ "PrivetMonoculture",
    Group %in% c(7) ~ "WattleMonoculture",
    TRUE ~ NA_character_  # Optional: handles other values
  ))


# Remove NA values
SaplingPrivetWattle <- SaplingH %>%
  filter(!is.na(group_label))


ggplot(SaplingPrivetWattle,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingPrivetWattle) +
  geom_histogram(aes(AverageGR), binwidth = .1)+
  facet_wrap(~group_label)
ggplot(SaplingPrivetWattle)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_SaplingPrivetWattle <- SaplingPrivetWattle %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingPrivetWattle$sd_AverageGR))/(min(summ_SaplingPrivetWattle$sd_AverageGR))
print(ratio)

# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingPrivetWattle, var.equal = TRUE)


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

# Check for homogeneous variance
summ_SaplingWattlePrivetAll <- SaplingWattlePrivetAll %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingWattlePrivetAll$sd_AverageGR))/(min(summ_SaplingWattlePrivetAll$sd_AverageGR))
print(ratio)


# Ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWattlePrivetAll, var.equal = TRUE)


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

# Check for homogeneous variance
summ_SaplingWattleWoollyAll <- SaplingWattleWoollyAll %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingWattleWoollyAll$sd_AverageGR))/(min(summ_SaplingWattleWoollyAll$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWattleWoollyAll, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SaplingPrivetWoollyAll <- SaplingPrivetWoollyAll %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingPrivetWoollyAll$sd_AverageGR))/(min(summ_SaplingPrivetWoollyAll$sd_AverageGR))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingPrivetWoollyAll, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SaplingWattleMonoPrivet <- SaplingWattleMonoPrivet %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingWattleMonoPrivet$sd_AverageGR))/(min(summ_SaplingWattleMonoPrivet$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWattleMonoPrivet, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SaplingPrivetMonoWattle <- SaplingPrivetMonoWattle %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingPrivetMonoWattle$sd_AverageGR))/(min(summ_SaplingPrivetMonoWattle$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingPrivetMonoWattle, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SaplingWattlePairs <- SaplingWattlePairs %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingWattlePairs$sd_AverageGR))/(min(summ_SaplingWattlePairs$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWattlePairs, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SaplingPrivetPairs <- SaplingPrivetPairs %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingPrivetPairs$sd_AverageGR))/(min(summ_SaplingPrivetPairs$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingPrivetPairs, var.equal = TRUE)

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

# Check for homogeneous variance
summ_SaplingWoollyPairs <- SaplingWoollyPairs %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingWoollyPairs$sd_AverageGR))/(min(summ_SaplingWoollyPairs$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SaplingWoollyPairs, var.equal = TRUE)

#### Growth rate of seedlings ####

SeedlingH <- Height[Height$Plant == "ManukaSeedling", ]

# Look at control vs groups that have weeds
SeedlingH <- SeedlingH %>%
  mutate(group_label = case_when(
    Group == 1 ~ "Control",
    Group %in% c(3, 4, 5) ~ "Weeds",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
SeedlingControl <- SeedlingH %>%
  filter(!is.na(group_label))

ggplot(SeedlingControl,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Seedling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SeedlingControl) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SeedlingControl)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_SeedlingControl <- SeedlingControl %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SeedlingControl$sd_AverageGR))/(min(summ_SeedlingControl$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SeedlingControl, var.equal = TRUE)

# woolly and privet vs woolly and wattle
SeedlingH <- SeedlingH %>%
  mutate(group_label = case_when(
    Group == 3 ~ "WoollyPrivet",
    Group %in% c(4) ~ "WoollyWattle",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
SeedlingWoollyPairs <- SeedlingH %>%
  filter(!is.na(group_label))

ggplot(SeedlingWoollyPairs,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Seedling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SeedlingWoollyPairs) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SeedlingWoollyPairs)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_SeedlingWoollyPairs <- SeedlingWoollyPairs %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SeedlingWoollyPairs$sd_AverageGR))/(min(summ_SeedlingWoollyPairs$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SeedlingWoollyPairs, var.equal = TRUE)

# woolly and privet vs privet and wattle
SeedlingH <- SeedlingH %>%
  mutate(group_label = case_when(
    Group == 3 ~ "PrivetWoolly",
    Group %in% c(5) ~ "PrivetWattle",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
SeedlingPrivetPairs <- SeedlingH %>%
  filter(!is.na(group_label))

ggplot(SeedlingPrivetPairs,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Seedling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SeedlingPrivetPairs) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SeedlingPrivetPairs)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_SeedlingPrivetPairs <- SeedlingPrivetPairs %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SeedlingPrivetPairs$sd_AverageGR))/(min(summ_SeedlingPrivetPairs$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SeedlingPrivetPairs, var.equal = TRUE)

# woolly and wattle vs privet and wattle
SeedlingH <- SeedlingH %>%
  mutate(group_label = case_when(
    Group == 4 ~ "WattleWoolly",
    Group %in% c(5) ~ "WattlePrivet",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
SeedlingWattlePairs <- SeedlingH %>%
  filter(!is.na(group_label))

ggplot(SeedlingWattlePairs,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Seedling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SeedlingWattlePairs) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(SeedlingWattlePairs)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_SeedlingWattlePairs <- SeedlingWattlePairs %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SeedlingWattlePairs$sd_AverageGR))/(min(summ_SeedlingWattlePairs$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = SeedlingWattlePairs, var.equal = TRUE)

#### Wattle RGR ####

WattleH <- Height[Height$Plant == "Wattle", ]

# Look at monoculture vs coocurrence
WattleH <- WattleH %>%
  mutate(group_label = case_when(
    Group == 7 ~ "WattleMonoculture",
    Group %in% c(2, 4, 5) ~ "WattleCooccur",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
WattleMono <- WattleH %>%
  filter(!is.na(group_label))

ggplot(WattleMono,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "pink2", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Wattle RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(WattleMono) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(WattleMono)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_WattleMono <- WattleMono %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_WattleMono$sd_AverageGR))/(min(summ_WattleMono$sd_AverageGR))
print(ratio)

# Ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = WattleMono, var.equal = TRUE)

# Look at wattle + privet vs wattle monoculture
WattleH <- WattleH %>%
  mutate(group_label = case_when(
    Group == 7 ~ "WattleMonoculture",
    Group %in% c(5) ~ "WattlePrivet",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
WattlePrivetMono <- WattleH %>%
  filter(!is.na(group_label))

ggplot(WattlePrivetMono,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "pink2", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Wattle RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(WattlePrivetMono) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(WattlePrivetMono)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_WattlePrivetMono <- WattlePrivetMono %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_WattlePrivetMono$sd_AverageGR))/(min(summ_WattlePrivetMono$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = WattlePrivetMono, var.equal = TRUE)

# Look at wattle + woolly vs wattle + privet
WattleH <- WattleH %>%
  mutate(group_label = case_when(
    Group == 4 ~ "WattleWoolly",
    Group %in% c(5) ~ "WattlePrivet",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
WattlePrivetWoolly <- WattleH %>%
  filter(!is.na(group_label))

ggplot(WattlePrivetWoolly,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "pink2", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Wattle RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(WattlePrivetWoolly) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(WattlePrivetWoolly)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_WattlePrivetWoolly <- WattlePrivetWoolly %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_WattlePrivetWoolly$sd_AverageGR))/(min(summ_WattlePrivetWoolly$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = WattlePrivetWoolly, var.equal = TRUE)

#### Privet RGR ####

PrivetH <- Height[Height$Plant == "Privet", ]

# Privet monoculture vs all groupings with privet
PrivetH <- PrivetH %>%
  mutate(group_label = case_when(
    Group == 8 ~ "PrivetMonoculture",
    Group %in% c(2, 3, 5) ~ "PrivetCooccur",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
PrivetMono <- PrivetH %>%
  filter(!is.na(group_label))

ggplot(PrivetMono,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "orange2", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Wattle RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(PrivetMono) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(PrivetMono)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_PrivetMono <- PrivetMono %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_PrivetMono$sd_AverageGR))/(min(summ_PrivetMono$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = PrivetMono, var.equal = TRUE)

# Privet monoculture vs wattle + privet
PrivetH <- PrivetH %>%
  mutate(group_label = case_when(
    Group == 8 ~ "PrivetMonoculture",
    Group %in% c(5) ~ "PrivetWattle",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
PrivetMonoWattle <- PrivetH %>%
  filter(!is.na(group_label))

ggplot(PrivetMonoWattle,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "orange2", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Wattle RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(PrivetMonoWattle) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(PrivetMonoWattle)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_PrivetMonoWattle <- PrivetMonoWattle %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_PrivetMonoWattle$sd_AverageGR))/(min(summ_PrivetMonoWattle$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = PrivetMonoWattle, var.equal = TRUE)

# Privet woolly vs wattle + privet
PrivetH <- PrivetH %>%
  mutate(group_label = case_when(
    Group == 3 ~ "PrivetWoolly",
    Group %in% c(5) ~ "PrivetWattle",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
PrivetPairs <- PrivetH %>%
  filter(!is.na(group_label))

ggplot(PrivetPairs,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "orange2", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Wattle RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(PrivetPairs) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(PrivetPairs)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_PrivetPairs <- PrivetPairs %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_PrivetPairs$sd_AverageGR))/(min(summ_PrivetPairs$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = PrivetPairs, var.equal = TRUE)

# Privet woolly vs all other privet groups
PrivetH <- PrivetH %>%
  mutate(group_label = case_when(
    Group == 3 ~ "PrivetWoolly",
    Group %in% c(2, 5, 8) ~ "PrivetOthers",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
PrivetWoolly <- PrivetH %>%
  filter(!is.na(group_label))

ggplot(PrivetWoolly,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "orange2", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Wattle RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(PrivetWoolly) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(PrivetWoolly)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_PrivetWoolly <- PrivetWoolly %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_PrivetWoolly$sd_AverageGR))/(min(summ_PrivetWoolly$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = PrivetWoolly, var.equal = TRUE)

#### Woolly Nightshade RGR ####

WoollyH <- Height[Height$Plant == "Nightshade", ]

# Woolly monoculture vs all groupings with Woolly
WoollyH <- WoollyH %>%
  mutate(group_label = case_when(
    Group == 6 ~ "WoollyMonoculture",
    Group %in% c(2, 3, 4) ~ "WoollyCooccur",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
WoollyMono <- WoollyH %>%
  filter(!is.na(group_label))

ggplot(WoollyMono,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "#CF597E", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Woolly RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(WoollyMono) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(WoollyMono)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_WoollyMono <- WoollyMono %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_WoollyMono$sd_AverageGR))/(min(summ_WoollyMono$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = WoollyMono, var.equal = TRUE)

# Woolly with wattle vs woolly with privet
WoollyH <- WoollyH %>%
  mutate(group_label = case_when(
    Group == 3 ~ "WoollyPrivet",
    Group %in% c(4) ~ "WoollyWattle",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# Remove NA values
WoollyPairs <- WoollyH %>%
  filter(!is.na(group_label))

ggplot(WoollyPairs,aes(x = group_label, y = AverageGR))+
  geom_boxplot(fill = "#CF597E", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Woolly RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(WoollyPairs) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~group_label)
ggplot(WoollyPairs)+
  geom_qq(aes(sample = AverageGR, color = group_label))

# Check for homogeneous variance
summ_WoollyPairs <- WoollyPairs %>%
  group_by(group_label) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_WoollyPairs$sd_AverageGR))/(min(summ_WoollyPairs$sd_AverageGR))
print(ratio)

# ratio < 3
# Performing the t-test
t.test(AverageGR ~ group_label, data = WoollyPairs, var.equal = TRUE)
