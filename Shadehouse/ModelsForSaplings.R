#### Modeling factors associated with RGR, Biomass, and Survival ####
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

#### Sapling Biomass Model ####
# Cleaning up the data 

library(readr)
Biomass <- read_csv("Shadehouse/Biomass Experiment 1 Clean.csv")

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

library(readr)
Media <- read_csv("Shadehouse/SoilTests_Time2.csv")

colnames(Media)[3:3] <- c("Group") ## Renaming the columns
Media$Group <- as.factor(Media$Group)

library(readr)
Nodules <- read_csv("Shadehouse/Nodule data sheet Experiment 1 Clean.csv")
colnames(Nodules)[3:3] <- c("Group") ## Renaming the columns

library(dplyr)

Biomass <- Biomass %>%
  left_join(Nodules %>% dplyr::select(Pot, Nodule_start), by = "Pot") %>%
  mutate(Nodule_start = replace_na(Nodule_start, 0))

Biomass <- Biomass %>%
  left_join(Nodules %>% dplyr::select(Pot, Nodule_finish), by = "Pot") %>%
  mutate(Nodule_finish = replace_na(Nodule_finish, 0))

# Compute averages and join
Biomass <- Biomass %>%
  left_join(
    Media %>%
      group_by(Group) %>%
      summarise(Avg_Nitrate_T2 = mean(Nitrate_T2, na.rm = TRUE)),
    by = "Group"
  )
# Compute averages and join Ammonium
Biomass <- Biomass %>%
  left_join(
    Media %>%
      group_by(Group) %>%
      summarise(Avg_Ammonium_T2 = mean(Ammonium_T2, na.rm = TRUE)),
    by = "Group"
  )
# Compute averages and join Phosphorus
Biomass <- Biomass %>%
  left_join(
    Media %>%
      group_by(Group) %>%
      summarise(Avg_Phosphorus_T2 = mean(Phosphorus_T2, na.rm = TRUE)),
    by = "Group"
  )
# Compute averages and join Potassium
Biomass <- Biomass %>%
  left_join(
    Media %>%
      group_by(Group) %>%
      summarise(Avg_Potassium_T2 = mean(Potassium_T2, na.rm = TRUE)),
    by = "Group"
  )
# Compute averages and join Carbon Nitrogen Ratio
Biomass <- Biomass %>%
  left_join(
    Media %>%
      group_by(Group) %>%
      summarise(Avg_C_NRatio_T2 = mean(C_NRatio_T2, na.rm = TRUE)),
    by = "Group"
  )

# Add presence and absence of weeds 
Biomass <- Biomass %>%
  mutate(Woolly = if_else(Group %in% c(2, 3, 4, 6), 1, 0))

Biomass <- Biomass %>%
  mutate(Wattle = if_else(Group %in% c(2, 4, 5, 7), 1, 0))

Biomass <- Biomass %>%
  mutate(Privet = if_else(Group %in% c(2, 3, 5, 8), 1, 0))

# Add room
library(readr)
RoomPot <- read_csv("Shadehouse/RoomPot.csv")

colnames(RoomPot)[1:1] <- c("Pot") ## Renaming the columns
Biomass <- Biomass %>%
  left_join(RoomPot %>% dplyr::select(Pot, Room), by = "Pot")


#### Sapling Model ####

# include only saplings
Sapling <- Biomass[Biomass$Plant == "ManukaSapling", ]

Sapling$Group <- as.numeric(Sapling$Group)


colnames(Sapling)[13:17] <- c("Nitrate","Ammonium", "Phosphorus", "Potassium",
                                       "C_N") ## Renaming the columns

# Load required package
library(vegan)
library(dplyr)

# try a mixed effects model
library(lme4)
library(MuMIn)

model.full <- lmer(Mass ~ factor(Group) + (1 | Room),
                    data = Sapling,
                    na.action = na.fail)   # critical for dredge
summary(model.full)
ranef(model.full)

Sapling$pred_fixed <- predict(model.full, re.form = NA)
Sapling$pred_random <- predict(model.full)
library(ggplot2)

ggplot(Sapling, aes(x = Group, y = Mass, color = Room)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = pred_random, group = Room), linetype = "solid") +   # random effects
  geom_line(aes(y = pred_fixed, group = 1), color = "black", size = 1.2, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Fixed vs Random Effect Predictions",
       y = "Mass", x = "Group")

# change group names
Sapling$Group <- factor(Sapling$Group, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), # order  
                                         labels = c("m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 

# look at the effects
# Fit a model 
M2 <- lmer(Mass ~ factor(Group) + (1 | Room), data = Sapling)

summary(M2)
# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M2, type = 3) 
emm <- emmeans(M2, ~ Group)   
SaplingBiomassComparison <- pairs(emm, adjust = "tukey")

SaplingBiomassComparison <- as.data.frame(SaplingBiomassComparison)

library(writexl)

write_xlsx(SaplingBiomassComparison, "C:/Users/bella/Documents/SaplingBiomassComparison.xlsx")

#### RGR ####
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

#### Let's look at Sapling height ####
SaplingH <- Height[Height$Plant == "ManukaSapling", ]

Sapling <- Sapling %>%
  left_join(dplyr::select(SaplingH, Pot, AverageGR), by = "Pot")

Sapling <- Sapling %>% 
  filter(!is.na(AverageGR))

# Load required package
library(vegan)
library(dplyr)

# change group names
SaplingH$Group <- factor(SaplingH$Group, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), # order  
                        labels = c("m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 


# look at the effects
# Fit a model 
M1 <- lmer(AverageGR ~  factor(Group) +  (1 | Room), data = Sapling)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3)
emm <- emmeans(M1, ~ Group)    
SaplingRGRComparison <- pairs(emm, adjust = "tukey")

SaplingRGRComparison <- as.data.frame(SaplingRGRComparison)

library(writexl)

write_xlsx(SaplingRGRComparison, "C:/Users/bella/Documents/SaplingRGRComparison.xlsx")

#### Survival ####
# make a column that surviving y/n
Height <- Height %>%
  mutate(Survive = ifelse(is.na(Notes7), 1,
                          ifelse(Notes7 == "dead", 0, 1)))

SaplingH <- Height[Height$Plant == "ManukaSapling", ]

Sapling <- Sapling %>%
  left_join(dplyr::select(SaplingH, Pot, Survive), by = "Pot")

# Load required package
library(vegan)
library(dplyr)

# look at the effects
# Fit a model 
M1 <- lmer(Survive ~  factor(Group) +  (1 | Room), data = Sapling)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 

# Estimated marginal means and pairwise comparisons
library(emmeans)
emm <- emmeans(M1, ~ Group | Room)        # treatment effects within each room
pairs(emm, adjust = "mvt")

#### Woolly Nightshade Biomass ####

# include only saplings
Woolly <- Biomass[Biomass$Plant == "Nightshade", ]

Woolly$Group <- as.numeric(Woolly$Group)


colnames(Woolly)[13:17] <- c("Nitrate","Ammonium", "Phosphorus", "Potassium",
                              "C_N") ## Renaming the columns

# Load required package
library(vegan)
library(dplyr)

# change group names
Woolly$Group <- factor(Woolly$Group, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), # order  
                        labels = c("m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 


# look at the effects
# Fit a model 
M1 <- lmer(Mass ~  factor(Group) +  (1 | Room), data = Woolly)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 
emm <- emmeans(M1, ~ Group)  
WoollyBiomassComparison <- pairs(emm, adjust = "tukey")

WoollyBiomassComparison <- as.data.frame(WoollyBiomassComparison)

library(writexl)

write_xlsx(WoollyBiomassComparison, "C:/Users/bella/Documents/WoollyBiomassComparison.xlsx")

# Estimated marginal means and pairwise comparisons
summ_Woolly <- Woolly %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass))
print(summ_Woolly)

summ_WoollyRoom <- Woolly %>%
  group_by(Room) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass))
print(summ_WoollyRoom)

#### Woolly Nightshade RGR ####
WoollyH <- Height[Height$Plant == "Nightshade", ]

WoollyH <- WoollyH %>% 
  filter(!is.na(AverageGR))

# I need to add room to WoollyH
WoollyH <- WoollyH %>%
  left_join(dplyr::select(RoomPot, Pot, Room), by = "Pot")

WoollyH$Group <- as.numeric(WoollyH$Group)

# Load required package
library(vegan)
library(dplyr)

# change group names
WoollyH$Group <- factor(WoollyH$Group, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), # order  
                       labels = c("m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 

# look at the effects
# Fit a model 
M1 <- lmer(AverageGR ~  factor(Group) +  (1 | Room), data = WoollyH)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 
emm <- emmeans(M1, ~ Group)  
WoollyRGRComparison <- pairs(emm, adjust = "tukey")

WoollyRGRComparison <- as.data.frame(WoollyRGRComparison)

library(writexl)

write_xlsx(WoollyRGRComparison, "C:/Users/bella/Documents/WoollyRGRComparison.xlsx")

summ_WoollyH <- WoollyH %>%
  group_by(Group) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR))
print(summ_WoollyH)

summ_WoollyHRoom <- WoollyH %>%
  group_by(Room) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR))
print(summ_WoollyHRoom)

#### Tree Privet Biomass ####
# include only privet
Privet <- Biomass[Biomass$Plant == "Privet", ]

Privet$Group <- as.numeric(Privet$Group)


colnames(Privet)[13:17] <- c("Nitrate","Ammonium", "Phosphorus", "Potassium",
                             "C_N") ## Renaming the columns

# Load required package
library(vegan)
library(dplyr)

# change group names
Privet$Group <- factor(Privet$Group, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), # order  
                        labels = c("m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 

# look at the effects
# Fit a model 
M1 <- lmer(Mass ~  factor(Group) +  (1 | Room), data = Privet)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 
emm <- emmeans(M1, ~ Group)  
PrivetBiomassComparison <- pairs(emm, adjust = "tukey")

PrivetBiomassComparison <- as.data.frame(PrivetBiomassComparison)

library(writexl)

write_xlsx(PrivetBiomassComparison, "C:/Users/bella/Documents/PrivetBiomassComparison.xlsx")

summ_Privet <- Privet %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass))
print(summ_Privet)

summ_PrivetRoom <- Privet %>%
  group_by(Room) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass))
print(summ_PrivetRoom)

#### Privet RGR ####
PrivetH <- Height[Height$Plant == "Privet", ]

PrivetH <- PrivetH %>% 
  filter(!is.na(AverageGR))

# I need to add room to PrivetH
PrivetH <- PrivetH %>%
  left_join(dplyr::select(RoomPot, Pot, Room), by = "Pot")

PrivetH$Group <- as.numeric(PrivetH$Group)
# Load required package
library(vegan)
library(dplyr)

v

# look at the effects
# Fit a model 
M1 <- lmer(AverageGR ~  factor(Group) +  (1 | Room), data = PrivetH)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 
emm <- emmeans(M1, ~ Group)  
PrivetRGRComparison <- pairs(emm, adjust = "tukey")

PrivetRGRComparison <- as.data.frame(PrivetRGRComparison)

library(writexl)

write_xlsx(PrivetRGRComparison, "C:/Users/bella/Documents/PrivetRGRComparison.xlsx")


summ_PrivetH <- PrivetH %>%
  group_by(Group) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR))
print(summ_PrivetH)

summ_PrivetHRoom <- PrivetH %>%
  group_by(Room) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR))
print(summ_PrivetHRoom)

#### Wattle Biomass ####
# include only privet
Wattle <- Biomass[Biomass$Plant == "Wattle", ]

Wattle$Group <- as.numeric(Wattle$Group)


colnames(Wattle)[13:17] <- c("Nitrate","Ammonium", "Phosphorus", "Potassium",
                             "C_N") ## Renaming the columns

# Load required package
library(vegan)
library(dplyr)

# change group names
Wattle$Group <- factor(Wattle$Group, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), # order  
                        labels = c("m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 

# look at the effects
# Fit a model 
M1 <- lmer(Mass ~  factor(Group) +  (1 | Room), data = Wattle)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 
emm <- emmeans(M1, ~ Group)  
WattleBiomassComparison <- pairs(emm, adjust = "tukey")

WattleBiomassComparison <- as.data.frame(WattleBiomassComparison)

library(writexl)

write_xlsx(WattleBiomassComparison, "C:/Users/bella/Documents/WattleBiomassComparison.xlsx")

summ_Wattle <- Wattle %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            se_Mass = sd(Mass)/sqrt(n()))
print(summ_Wattle)

summ_WattleRoom <- Wattle %>%
  group_by(Room) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass))
print(summ_WattleRoom)

#### Wattle RGR ####
WattleH <- Height[Height$Plant == "Wattle", ]

WattleH <- WattleH %>% 
  filter(!is.na(AverageGR))

# I need to add room to WattleH
WattleH <- WattleH %>%
  left_join(dplyr::select(RoomPot, Pot, Room), by = "Pot")

WattleH$Group <- as.numeric(WattleH$Group)
# Load required package
library(vegan)
library(dplyr)

# change group names
WattleH$Group <- factor(WattleH$Group, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), # order  
                       labels = c("m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 

# look at the effects
# Fit a model 
M1 <- lmer(AverageGR ~  factor(Group) +  (1 | Room), data = WattleH)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 
emm <- emmeans(M1, ~ Group) 
WattleRGRComparison <- pairs(emm, adjust = "tukey")

WattleRGRComparison <- as.data.frame(WattleRGRComparison)

library(writexl)

write_xlsx(WattleRGRComparison, "C:/Users/bella/Documents/WattleRGRComparison.xlsx")

summ_WattleH <- WattleH %>%
  group_by(Group) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR))
print(summ_WattleH)

summ_WattleHRoom <- WattleH %>%
  group_by(Room) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR))
print(summ_WattleHRoom)

#### Seedling Biomass ####
# include only seedlings
Seedling <- Biomass[Biomass$Plant == "ManukaSeedling", ]

Seedling$Group <- as.numeric(Seedling$Group)


colnames(Seedling)[13:17] <- c("Nitrate","Ammonium", "Phosphorus", "Potassium",
                             "C_N") ## Renaming the columns
Seedling<-Seedling[-81, ] 

# Load required package
library(vegan)
library(dplyr)

# change group names
Seedling$Group <- factor(Seedling$Group, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), # order  
                        labels = c("m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 

# look at the effects
# Fit a model 
M1 <- lmer(Mass ~  factor(Group) +  (1 | Room), data = Seedling)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 

# Estimated marginal means and pairwise comparisons
library(emmeans)
emm <- emmeans(M1, ~ Group)  
SeedlingBiomassComparison <- pairs(emm, adjust = "tukey")

SeedlingBiomassComparison <- as.data.frame(SeedlingBiomassComparison)

library(writexl)

write_xlsx(SeedlingBiomassComparison, "C:/Users/bella/Documents/SeedlingBiomassComparison.xlsx")


summ_Seedling <- Seedling %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass))
print(summ_Seedling)

summ_SeedlingRoom <- Seedling %>%
  group_by(Room) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass))
print(summ_SeedlingRoom)

#### Seedling RGR ####
SeedlingH <- Height[Height$Plant == "ManukaSeedling", ]

SeedlingH <- SeedlingH %>% 
  filter(!is.na(AverageGR))

# I need to add room to SeedlingH
SeedlingH <- SeedlingH %>%
  left_join(dplyr::select(RoomPot, Pot, Room), by = "Pot")

SeedlingH$Group <- as.numeric(SeedlingH$Group)
# Load required package
library(vegan)
library(dplyr)

# change group names
SeedlingH$Group <- factor(SeedlingH$Group, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), # order  
                         labels = c("m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 

# look at the effects
# Fit a model 
# Fit a model 
M1 <- lmer(AverageGR ~  factor(Group) +  (1 | Room), data = SeedlingH)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 

# Estimated marginal means and pairwise comparisons
library(emmeans)
emm <- emmeans(M1, ~ Group)  
SeedlingRGRComparison <- pairs(emm, adjust = "tukey")

SeedlingRGRComparison <- as.data.frame(SeedlingRGRComparison)

library(writexl)

write_xlsx(SeedlingRGRComparison, "C:/Users/bella/Documents/SeedlingRGRComparison.xlsx")

summ_SeedlingH <- SeedlingH %>%
  group_by(Group) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR))
print(summ_SeedlingH)

summ_SeedlingHRoom <- SeedlingH %>%
  group_by(Room) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR))
print(summ_SeedlingHRoom)

#### look at Nodules ####
# look at the effects
# Fit a model 
Biomass <- Biomass %>%
  mutate(Nodule_change = Nodule_finish - Nodule_start)

# remove treatment groups that do not have nodules
Nodules <- Biomass %>% filter(Group %in% c("2", "4", "5", "7"))

M1 <- lmer(Nodule_change ~  factor(Group) +  (1 | Room), data = Nodules)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 
emm <- emmeans(M1, ~ Group)        # treatment effects within each room
NodulesOutput <- pairs(emm, adjust = "tukey")

NodulesOutput <- as.data.frame(NodulesOutput)

library(writexl)

write_xlsx(NodulesOutput, "C:/Users/bella/Documents/NodulesOutput.xlsx")

summ_Nodules <- Nodules %>%
  group_by(Group) %>% 
  summarise(mean_Nodules = mean(Nodule_change),
            sd_Nodules = sd(Nodule_change), 
            se_Nodule_change = sd(Nodule_change)/sqrt(n()))
print(summ_Nodules)
ratio <-(max(summ_Nodules$sd_Nodules))/(min(summ_Nodules$sd_Nodules))
print(ratio)

summ_Nodules <- as.data.frame(summ_Nodules)

library(writexl)

write_xlsx(summ_Nodules, "C:/Users/bella/Documents/summ_Nodules.xlsx")

# try a transformation
Nodules <- Nodules %>%
  mutate(sqrtNodule_change = sqrt(Nodule_change))

summ_Nodules <- Nodules %>%
  group_by(Group) %>% 
  summarise(mean_Nodules = mean(logNodule_change),
            sd_Nodules = sd(logNodule_change))
print(summ_Nodules)
ratio <-(max(summ_Nodules$sd_Nodules))/(min(summ_Nodules$sd_Nodules))
print(ratio)


summ_NodulesRoom <- Nodules %>%
  group_by(Room) %>% 
  summarise(mean_Nodules = mean(Nodule_change),
            sd_Nodules = sd(Nodule_change))
print(summ_NodulesRoom)

Nodules$Group <- factor(Nodules$Group, levels = c("2", "4", "5", "7"), # order  
                         labels = c("nwp", "mnw", "mwp", "w")) # labels 

NodulePlot <- ggplot(data = Nodules, 
                  aes(x = factor(Group), y = Nodule_change, fill = Group)) +
  geom_boxplot(notch = TRUE, varwidth = TRUE) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.6, width = 0.2) +
  ylab("Change in number of nodules") +
  xlab("Treatment Group") +
  scale_fill_manual(values = c( "nwp" = "#CF597E", 
                                "mnw" = "lavender", "mwp" = "lavender", "w" = "#E9A96C" )) +
  theme(axis.text.x = element_text(size = 10, color = 'black'),
        axis.text.y = element_text(size = 15, hjust = 1, colour = 'black'), 
        axis.title = element_text(size = 17, face = "bold"),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
NodulePlot

# That is not very easy to see