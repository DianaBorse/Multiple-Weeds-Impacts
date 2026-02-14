#### Mycorrhizae Analysis Experiment 1 ####

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
Myc <- read_csv("Shadehouse/MycorrhizaeDataClean.csv")

# Add room
library(readr)
RoomPot <- read_csv("Shadehouse/RoomPot.csv")

colnames(RoomPot)[1:1] <- c("Pot") ## Renaming the columns
colnames(Myc)[3:3] <- c("Pot") ## Renaming the columns
Myc <- Myc %>%
  left_join(RoomPot %>% dplyr::select(Pot, Room), by = "Pot")

# Now we need to make a distinct name for each subsample of each pot
colnames(Myc)[3:3] <- c("Sample") ## Renaming the columns

Myc$Exp1Group <- factor(Myc$Exp1Group, levels = c("0", "1", "2","3","4", "5", "6", "7", "8"), # order  
                         labels = c("baseline", "m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 

Myc$Plant <- Myc$Sample

# For total myc, at each intersection I need a yes/no 1/0
library(tidyr)
Myc <- Myc %>%
  unite(Subsample, Intersection, Subsample, sep = "-")

Myc <- Myc %>%
  unite(Sample, Subsample, Sample, sep = "-")

Myc <- Myc %>%
  mutate(TotalMyc = if_else(None == "yes", 0, 1))

# try a mixed effects model
library(lme4)

model <- glmer(
  TotalMyc ~ factor(Exp1Group) + (1 | Room/Plant),
  data = Myc,
  family = binomial
)

summary(model)

library(car)
Anova(model, type = 2)

library(emmeans)

emm <- emmeans(model, ~ Exp1Group, type = "response")
emm
MycModel <- pairs(emm, adjust = "tukey")

MycModel <- as.data.frame(MycModel)

library(writexl)

write_xlsx(MycModel, "C:/Users/bella/Documents/MycModel.xlsx")

# Now for arbuscules
# try a mixed effects model
Myc <- Myc %>%
  mutate(Arb = if_else(Arbuscules == "0", 0, 1))

model <- glmer(
  Arb ~ factor(Exp1Group) + (1 | Room/Plant),
  data = Myc,
  family = binomial
)

summary(model)

library(car)
Anova(model, type = 2)

library(emmeans)

emm <- emmeans(model, ~ Exp1Group, type = "response")
emm
ArbModel <- pairs(emm, adjust = "tukey")

ArbModel <- as.data.frame(ArbModel)

library(writexl)

write_xlsx(ArbModel, "C:/Users/bella/Documents/ArbModel.xlsx")

# what if I remove baseline?
Myc_treatments <- Myc %>%
  filter(Exp1Group != "baseline")

# try a mixed effects model
library(lme4)

model <- glmer(
  TotalMyc ~ factor(Exp1Group) + (1 | Room/Plant),
  data = Myc_treatments,
  family = binomial
)

summary(model)

library(car)
Anova(model, type = 2)

library(emmeans)

emm <- emmeans(model, ~ Exp1Group, type = "response")
emm
pairs(emm, adjust = "tukey")


Myc_treatments <- Myc_treatments %>%
  mutate(Arb = if_else(Arbuscules == "0", 0, 1))

model <- glmer(
  Arb ~ factor(Exp1Group) + (1 | Room/Plant),
  data = Myc_treatments,
  family = binomial
)

summary(model)

library(car)
Anova(model, type = 2)

library(emmeans)

emm <- emmeans(model, ~ Exp1Group, type = "response")
emm
pairs(emm, adjust = "tukey")

