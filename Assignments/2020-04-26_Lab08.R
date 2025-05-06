#### Lab 8: 1-way ANOVA, continued #### 

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()


#### Problem 15-23 ####
# Complete parts a and c only

# a. This is a planned comparison.
# c. 
Cones <- read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv", col_types = cols(
  habitat = col_factor() ))

#Calculate Summary statistics
Cones <- Cones %>%
  mutate(habitat = fct_recode(habitat, island.absent = "island.absent",
                              island.present = "island.present",
                               mainland.present = "mainland.present" ))

# Look at the data
head(Cones)
summary(Cones)

# Check for normality

ggplot(Cones, aes(x = habitat, y = conemass))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Cones) +
  geom_histogram(aes(conemass), binwidth = 1)+
  facet_wrap(~habitat)
ggplot(Cones)+
  geom_qq(aes(sample = conemass, color = habitat))

# Check for Homogeneous variance
summ_conemass <- Cones %>%
  group_by(habitat) %>% 
  summarise(mean_conemass = mean(conemass),
            sd_conemass = sd(conemass),
            n_conemass = n())
ratio <-(max(summ_conemass$sd_conemass))/(min(summ_conemass$sd_conemass))

# Yes, it is normal enough and has homogeneous variance (<3)
# Set up for ANOVA
model01 <- lm(conemass~habitat, data = Cones)

autoplot(model01)

anova(model01)

summary(model01)

# Perform planned comparison
planned <- glht(model01, linfct = 
                  mcp(habitat = c("island.absent - island.present = 0")))
confint(planned)
summary(planned)

# Perform unplanned comparrison for the others
tukey <- glht(model01, linfct = mcp(habitat = "Tukey"))
summary(tukey)

#### Problem 15-26 ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

# Read in the data
Mosquitos <- read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", col_types = cols(
  treatmentGroup = col_factor() ))
Mosquitos <- Mosquitos %>%
  mutate(crabType = fct_recode(treatmentGroup, Scorpine = "Scorpine",
                              WT = "WT",
                              Control = "Control" ))

# Look at the data
head(Mosquitos)
summary(Mosquitos)

# Check for normality

ggplot(Mosquitos, aes(x = treatmentGroup, y = logSporozoiteNumbers))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Mosquitos) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 1)+
  facet_wrap(~treatmentGroup)
ggplot(Mosquitos)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))

# Check for Homogeneous variance
summ_logSporozoiteNumbers <- Mosquitos %>%
  group_by(treatmentGroup) %>% 
  summarise(mean_logSporozoiteNumbers = mean(logSporozoiteNumbers),
            sd_logSporozoiteNumbers = sd(logSporozoiteNumbers),
            n_logSporozoiteNumbers = n())
ratio <-(max(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))/(min(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))

# Yes, it is normal enough and variance is homogeneous (<3).
# Set up for ANOVA
model02 <- lm(logSporozoiteNumbers~treatmentGroup, data = Mosquitos)

autoplot(model02)

anova(model02)

summary(model02)

# Perform unplanned comparison for the others
tukey <- glht(model02, linfct = mcp(treatmentGroup = "Tukey"))
summary(tukey)

#### Problem 15-30 and/or 15-31 (same data in both problems) ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

# Read in the Data
Crabs <- read_csv("datasets/demos/Crabs.csv", col_types = cols(
  crabType = col_factor() ))

Crabs <- Crabs %>%
  mutate(crabType = fct_recode(crabType, female = "female",
                                     intact.male = "intact.male",
                                     male.minor.removed = "male.minor.removed",
                                    male.major.removed = "male.major.removed"))

# Look at the data
head(Crabs)
summary(Crabs)

# Check for normality

ggplot(Crabs, aes(x = crabType, y = bodyTemperature))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Crabs) +
  geom_histogram(aes(bodyTemperature), binwidth = .1)+
  facet_wrap(~crabType)
ggplot(Crabs)+
  geom_qq(aes(sample = bodyTemperature, color = crabType)) +
          facet_wrap(~crabType)

# Test for homogeneous variance

summ_bodyTemperature <- Crabs %>%
  group_by(crabType) %>%
  summarise(mean_bodyTemperature = mean(bodyTemperature),
            sd_bodyTemperature = sd(bodyTemperature),
            n_bodyTemperature = n())
ratio <-(max(summ_bodyTemperature$sd_bodyTemperature))/(min(summ_bodyTemperature$sd_bodyTemperature))

# Yes, it is normal enough and variance is homogeneous (<3).
# Set up for ANOVA
model03 <- lm(bodyTemperature~crabType, data = Crabs)

autoplot(model03)

anova(model03)

summary(model03)

# Perform unplanned Tukey test
tukey <- glht(model03, linfct = mcp(crabType = "Tukey"))
summary(tukey)
