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

# which plant has the highest biomass
# reduce to just seedlings
Weeds <- Biomass[Biomass$Plant != "ManukaSapling", ]
# Fit a model 
library(lme4)
M2 <- lmer(Mass ~ factor(Plant) + (1 | Room), data = Weeds)

summary(M2)
# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M2, type = 3) 
library(emmeans)
emm <- emmeans(M2, ~ Plant)   
pairs(emm, adjust = "tukey")

# calculate means
summ_weeds <- Weeds %>%
  group_by(Plant) %>%
  summarise(mean_Mass = mean(Mass),
            median_Mass = median(Mass),
            IQR_Mass = IQR(Mass),
            sd_Mass = sd(Mass),
            var_Mass = var(Mass),
            se_Mass = sd(Mass)/sqrt(189))
print(summ_weeds)

# everything had significantly higher biomass than seedlings

#### Sapling Model ####

# include only saplings
Sapling <- Biomass[Biomass$Plant == "ManukaSapling", ]

# Check for normal distribution homogenous variance
ggplot(Sapling, aes(x = Group, y = Mass))+
  geom_boxplot(fill = "#33B08D", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Mānuka sapling RGR ln(mm/month)") + xlab("Treatment") +   ##Change axis titles
  theme(axis.text.x=element_text(size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black")) +  ##If you want to add an axis colour
theme_classic() 
ggplot(Sapling) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Sapling) +
  geom_histogram(aes(Mass), binwidth = 1)
ggplot(Sapling)+
  geom_qq(aes(sample = Mass, color = Group))

# Check for homogeneous variance
summ_Sapling <- Sapling %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_Sapling$sd_Mass))/(min(summ_Sapling$sd_Mass))
print(ratio)

# try a mutation
Sapling <- Sapling %>%
  mutate(logMass = log(Mass))

# Check for homogeneous variance
summ_Sapling <- Sapling %>%
  group_by(Group) %>% 
  summarise(mean_logMass = mean(logMass),
            sd_logMass = sd(logMass),
            n_logMass = n())
ratio <-(max(summ_Sapling$sd_logMass))/(min(summ_Sapling$sd_logMass))
print(ratio)

ggplot(Sapling,aes(x = Group, y = logMass))+
  geom_boxplot(fill = "#33B08D", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Mānuka sapling logMass)") + xlab("Treatment") +   ##Change axis titles
  theme(axis.text.x=element_text(size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
theme_classic() 
ggplot(Sapling) +
  geom_histogram(aes(logMass), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Sapling) +
  geom_histogram(aes(logMass), binwidth = 1)
ggplot(Sapling)+
  geom_qq(aes(sample = logMass, color = Group))


# change group names
Sapling$Group <- factor(Sapling$Group, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), # order  
                                         labels = c("m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 

# look at the effects
# Fit a model 
M2 <- lmer(logMass ~ factor(Group) + (1 | Room), data = Sapling)

summary(M2)
# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M2, type = 3) 
emm <- emmeans(M2, ~ Group)   
SaplingBiomassComparison <- pairs(emm, adjust = "tukey")

SaplingBiomassComparison <- as.data.frame(SaplingBiomassComparison)

# library(writexl)
# 
# write_xlsx(SaplingBiomassComparison, "C:/Users/bella/Documents/SaplingBiomassComparison.xlsx")

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

# this is needed for running the model for RGR across all species, but will throw
# off the rest of the code.
# # I need to add room 
# Height <- Height %>%
#   left_join(dplyr::select(RoomPot, Pot, Room), by = "Pot")


# which plant has the highest biomass
# reduce to just seedlings
# Fit a model 
library(lme4)
M2 <- lmer(AverageGR ~ factor(Plant) +  (1 | Room), data = Height)

summary(M2)

res <- residuals(M2)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")
# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M2, type = 3) 
library(emmeans)
emm <- emmeans(M2, ~ Plant)   
RGRComparisons <- pairs(emm, adjust = "tukey")

RGRComparisons <- as.data.frame(RGRComparisons)

# library(writexl)

#write_xlsx(RGRComparisons, "C:/Users/bella/Documents/RGRComparisons.xlsx")

# calculate means
summ_height <- Height %>%
  group_by(Plant) %>%
  summarise(mean_AverageGR = mean(AverageGR),
            median_AverageGR = median(AverageGR),
            IQR_AverageGR = IQR(AverageGR),
            sd_AverageGR = sd(AverageGR),
            var_AverageGR = var(AverageGR),
            se_AverageGR = sd(AverageGR)/sqrt(189))
print(summ_height)


#### Let's look at Sapling height ####
SaplingH <- Height[Height$Plant == "ManukaSapling", ]

# Remove outlier -4 RGR
SaplingH<-SaplingH[-53, ]

# I need to add room 
SaplingH <- SaplingH %>%
  left_join(dplyr::select(RoomPot, Pot, Room), by = "Pot")

SaplingH <- SaplingH %>% 
  filter(!is.na(AverageGR))

# Load required package
library(vegan)
library(dplyr)

# change group names
SaplingH$Group <- factor(SaplingH$Group, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), # order  
                       labels = c("m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 


# Check for normal distribution homogenous variance

ggplot(SaplingH,aes(x = Group, y = AverageGR))+
  geom_boxplot(fill = "#33B08D", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Mānuka sapling RGR ln(mm/month)") + xlab("Treatment Group") +   ##Change axis titles
  theme(axis.text.x=element_text(size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
  theme_classic() 
ggplot(SaplingH) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~Group)
ggplot(SaplingH) +
  geom_histogram(aes(AverageGR), binwidth = 1)
ggplot(SaplingH)+
  geom_qq(aes(sample = AverageGR, color = Group))

# Check for homogeneous variance
summ_SaplingH <- SaplingH %>%
  group_by(Group) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingH$sd_AverageGR))/(min(summ_SaplingH$sd_AverageGR))
print(ratio)

# look at the effects
# Fit a model 
M1 <- lmer(AverageGR ~  factor(Group) +  (1 | Room), data = SaplingH)

summary(M1)

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3)
emm <- emmeans(M1, ~ Group)    
SaplingRGRComparison <- pairs(emm, adjust = "tukey")

SaplingRGRComparison <- as.data.frame(SaplingRGRComparison)
# 
# library(writexl)
# 
# write_xlsx(SaplingRGRComparison, "C:/Users/bella/Documents/SaplingRGRComparison.xlsx")

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

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

# transform
hist(Woolly$Mass)

# Define a custom function
cuberoot <- function(x) {
  sign(x) * abs(x)^(1/3)
}

#try a transformation
Woolly <- Woolly %>%
  mutate(cubeMass = cuberoot(Mass))

hist(Woolly$cubeMass)

# Fit a model 
M1 <- lmer(cubeMass ~  factor(Group) +  (1 | Room), data = Woolly)

summary(M1)

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 
emm <- emmeans(M1, ~ Group)  
WoollyBiomassComparison <- pairs(emm, adjust = "tukey")

WoollyBiomassComparison <- as.data.frame(WoollyBiomassComparison)

# library(writexl)
# 
# write_xlsx(WoollyBiomassComparison, "C:/Users/bella/Documents/WoollyBiomassComparison.xlsx")
#Visualisation
Woolly$Group <- as.factor(Woolly$Group)

# Add a column that is the mass of each sample / mean control mass
groupn_mean <- mean(Woolly$cubeMass[Woolly$Group == "n"], na.rm = TRUE)

Woolly$ResponseRatio <- Woolly$cubeMass / groupn_mean

# filter to remove baseline for plot
WoollyPlot <- Woolly[Woolly$Group != "n", ]


WoollyPlot %>%
  ggplot(aes(x = Group, y = ResponseRatio)) +
  geom_boxplot(fill = "#CF597E", varwidth = TRUE, notch = TRUE) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  ylab("Woolly nightshade cube root Biomass mixture / control (g)") +
  xlab("Treatment") +
  theme_classic()

# log response ratio
# Woolly
#Function to compute lnRR using metafor for a given plant and control group
library(metafor)
compute_lnRR <- function(Biomass, plant_name = "Nightshade", control_group = 6) {
  # Filter for specified plant
  df_filtered <- Biomass %>% filter(Plant == plant_name)
  
  df_filtered <- df_filtered %>%
    mutate(cubeMass = cuberoot(Mass))
  
  
  # Split into treatment and control
  control <- df_filtered %>% filter(Group == control_group)
  treatments <- df_filtered %>% filter(Group != control_group)
  
  # Summarise group stats
  group_stats <- treatments %>%
    group_by(Group) %>%
    summarise(
      m1i = mean(cubeMass),
      sd1i = sd(cubeMass),
      n1i = n(),
      .groups = "drop"
    ) %>%
    mutate(
      m2i = mean(control$cubeMass),
      sd2i = sd(control$cubeMass),
      n2i = nrow(control)
    )
  
  # Compute lnRR and sampling variance
  escalc_results <- escalc(
    measure = "ROM",
    m1i = group_stats$m1i,
    sd1i = group_stats$sd1i,
    n1i = group_stats$n1i,
    m2i = group_stats$m2i,
    sd2i = group_stats$sd2i,
    n2i = group_stats$n2i
  )
  
  # Combine with group info
  results <- bind_cols(group_stats["Group"], escalc_results)
  return(results)
}

# Run the function
lnrr_output <- compute_lnRR(Biomass, plant_name = "Nightshade", control_group = 6)
print(lnrr_output)

# Fix up the treatment groups
lnrr_output$Group <- factor(lnrr_output$Group, levels = c("2", "3", "4", "6"), # order  
                            labels = c("nbp", "np", "nb", "n")) # labels 

# Plot lnRR with 95% CI
ggplot(lnrr_output, aes(x = factor(Group), y = yi)) +
  geom_point(shape = 16, size = 3) +
  geom_errorbar(aes(ymin = yi - 1.96 * sqrt(vi), ymax = yi + 1.96 * sqrt(vi)), width = 0.2) +
  labs(
    x = "Treatment",
    y = "lnRR_mixed/monoculture woolly nightshade cube root biomass (g)"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 10),
    axis.title = element_text(face = "bold")
  )


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

hist(WoollyH$AverageGR)

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

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 
emm <- emmeans(M1, ~ Group)  
WoollyRGRComparison <- pairs(emm, adjust = "tukey")

WoollyRGRComparison <- as.data.frame(WoollyRGRComparison)

# library(writexl)
# 
# write_xlsx(WoollyRGRComparison, "C:/Users/bella/Documents/WoollyRGRComparison.xlsx")

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

NightshadePlot <- ggplot(data = WoollyH, 
                         aes(y = AverageGR, ##Change this to variable name
                             x = Group)) + ##Change this to variable name
  geom_boxplot(fill = "#CF597E", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Relative growth rate ln(mm/month)h") + xlab("Treatment") +   ##Change axis titles
  theme(axis.text.x=element_text(size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
NightshadePlot

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

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

hist(Privet$Mass)

#try a transformation
Privet <- Privet %>%
  mutate(cubeMass = cuberoot(Mass))

hist(Privet$cubeMass)

M1 <- lmer(cubeMass ~  factor(Group) +  (1 | Room), data = Privet)

summary(M1)

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 
emm <- emmeans(M1, ~ Group)  
PrivetBiomassComparison <- pairs(emm, adjust = "tukey")

PrivetBiomassComparison <- as.data.frame(PrivetBiomassComparison)

# library(writexl)
# 
# write_xlsx(PrivetBiomassComparison, "C:/Users/bella/Documents/PrivetBiomassComparison.xlsx")

#Visualisation
Privet$Group <- as.factor(Privet$Group)

# Add a column that is the mass of each sample / mean control mass
groupn_mean <- mean(Privet$cubeMass[Privet$Group == "p"], na.rm = TRUE)

Privet$ResponseRatio <- Privet$cubeMass / groupn_mean

# filter to remove baseline for plot
PrivetPlot <- Privet[Privet$Group != "p", ]


PrivetPlot %>%
  ggplot(aes(x = Group, y = ResponseRatio)) +
  geom_boxplot(fill = "#E57F6C", varwidth = TRUE, notch = TRUE) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  ylab("Tree privet cube root Biomass mixture / control (g)") +
  xlab("Treatment") +
  theme_classic()

# log response ratio
# Privet
#Function to compute lnRR using metafor for a given plant and control group
library(metafor)
compute_lnRR <- function(Biomass, plant_name = "Privet", control_group = 8) {
  # Filter for specified plant
  df_filtered <- Biomass %>% filter(Plant == plant_name)
  
  df_filtered <- df_filtered %>%
    mutate(cubeMass = cuberoot(Mass))
  
  
  # Split into treatment and control
  control <- df_filtered %>% filter(Group == control_group)
  treatments <- df_filtered %>% filter(Group != control_group)
  
  # Summarise group stats
  group_stats <- treatments %>%
    group_by(Group) %>%
    summarise(
      m1i = mean(cubeMass),
      sd1i = sd(cubeMass),
      n1i = n(),
      .groups = "drop"
    ) %>%
    mutate(
      m2i = mean(control$cubeMass),
      sd2i = sd(control$cubeMass),
      n2i = nrow(control)
    )
  
  # Compute lnRR and sampling variance
  escalc_results <- escalc(
    measure = "ROM",
    m1i = group_stats$m1i,
    sd1i = group_stats$sd1i,
    n1i = group_stats$n1i,
    m2i = group_stats$m2i,
    sd2i = group_stats$sd2i,
    n2i = group_stats$n2i
  )
  
  # Combine with group info
  results <- bind_cols(group_stats["Group"], escalc_results)
  return(results)
}

# Run the function
lnrr_output <- compute_lnRR(Biomass, plant_name = "Privet", control_group = 8)
print(lnrr_output)

# Fix up the treatment groups
lnrr_output$Group <- factor(lnrr_output$Group, levels = c("2", "3", "5", "8"), # order  
                            labels = c("nbp", "np", "bp", "p")) # labels 

# Plot lnRR with 95% CI
ggplot(lnrr_output, aes(x = factor(Group), y = yi)) +
  geom_point(shape = 16, size = 3) +
  geom_errorbar(aes(ymin = yi - 1.96 * sqrt(vi), ymax = yi + 1.96 * sqrt(vi)), width = 0.2) +
  labs(
    x = "Treatment",
    y = "lnRR_mixed/monoculture tree privet cube root biomass (g)"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 10),
    axis.title = element_text(face = "bold")
  )


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

hist(PrivetH$AverageGR)

# I need to add room to PrivetH
PrivetH <- PrivetH %>%
  left_join(dplyr::select(RoomPot, Pot, Room), by = "Pot")

PrivetH$Group <- as.numeric(PrivetH$Group)
# Load required package
library(vegan)
library(dplyr)

# look at the effects
# Fit a model 
M1 <- lmer(AverageGR ~  factor(Group) +  (1 | Room), data = PrivetH)

summary(M1)

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 
emm <- emmeans(M1, ~ Group)  
PrivetRGRComparison <- pairs(emm, adjust = "tukey")

PrivetRGRComparison <- as.data.frame(PrivetRGRComparison)

# library(writexl)
# 
# write_xlsx(PrivetRGRComparison, "C:/Users/bella/Documents/PrivetRGRComparison.xlsx")

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

# Fix up the treatment groups
PrivetH$Group <- factor(PrivetH$Group, levels = c("2", "3", "5", "8"), # order  
                        labels = c("nbp", "np", "bp", "p")) # labels 

PrivetPlot <- ggplot(data = PrivetH, 
                     aes(y = AverageGR, ##Change this to variable name
                         x = Group)) + ##Change this to variable name
  geom_boxplot(fill = "#E57F6C", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Relative growth rate ln(mm/month)") + xlab("Treatment") +   ##Change axis titles
  theme(axis.text.x=element_text(size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
PrivetPlot 


#### Wattle Biomass ####

Wattle <- Biomass[Biomass$Plant == "Wattle", ]

Wattle$Group <- as.numeric(Wattle$Group)


colnames(Wattle)[13:17] <- c("Nitrate","Ammonium", "Phosphorus", "Potassium",
                             "C_N") ## Renaming the columns

hist(Wattle$Mass)

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

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

#try a transformation
Wattle <- Wattle %>%
  mutate(cubeMass = cuberoot(Mass))

hist(Wattle$cubeMass)

# look at the effects
# Fit a model
M1 <- lmer(cubeMass ~  factor(Group) +  (1 | Room), data = Wattle)

summary(M1)

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

#try log + 1
Wattle <- Wattle %>%
  mutate(logMass = log(Mass + 1))

hist(Wattle$logMass)


# # Type II/III tests (handle unbalanced designs)
# library(car)
# Anova(M1, type = 3)
# emm <- emmeans(M1, ~ Group)
# WattleBiomassComparison <- pairs(emm, adjust = "tukey")
# 
# WattleBiomassComparison <- as.data.frame(WattleBiomassComparison)
# 
# library(writexl)
# 
# write_xlsx(WattleBiomassComparison, "C:/Users/bella/Documents/WattleBiomassComparison.xlsx")

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

# Kruskal-wallis for each room
Room546Wattle <- Wattle %>% filter(Room == 546)

kruskal.test(Mass ~ Group, data = Room546Wattle)

# Kruskal-wallis for each room
Room547Wattle <- Wattle %>% filter(Room == 547)

kruskal.test(Mass ~ Group, data = Room547Wattle)

# Kruskal-wallis for each room
Room544Wattle <- Wattle %>% filter(Room == 544)

kruskal.test(Mass ~ Group, data = Room544Wattle)

hist(Room544Wattle$Mass)
hist(Room546Wattle$Mass)
hist(Room547Wattle$Mass)

ggplot(Room544Wattle, aes(x = Group, y = Mass))+
  geom_boxplot() +
  theme_bw() 
ggplot(Room546Wattle, aes(x = Group, y = Mass))+
  geom_boxplot() +
  theme_bw() 
ggplot(Room547Wattle, aes(x = Group, y = Mass))+
  geom_boxplot() +
  theme_bw() 

# look at differences within rooms
library(FSA)
dt <- dunnTest(Mass ~ Group,
               data = Room546Wattle,
               method = "bonferroni")

dunn_tbl <- dt$res %>%
  mutate(
    Comparison =
      dplyr::case_when(
        # Case 2: FSA output already has a Comparison column
        "Comparison" %in% names(.) ~ Comparison
      ),
    Z = round(Z, 3),
    P.unadj = round(P.unadj, 4),
    P.adj = round(P.adj, 4)
  ) %>%
  select(Comparison, Z, P.unadj, P.adj)

gt_tbl <- dunn_tbl %>%
  gt() %>%
  tab_header(
    title = "Pairwise Dunn Test with Bonferroni Correction"
  ) %>%
  cols_label(
    Comparison = "Group Comparison",
    Z = "Z Statistic",
    P.unadj = "Unadjusted p",
    P.adj = "Bonferroni-adjusted p"
  ) %>%
  fmt_number(
    columns = c(Z, P.unadj, P.adj),
    decimals = 4
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

print(gt_tbl)

gtsave(gt_tbl, "dunn_table.html")

library(writexl)

write_xlsx(dunn_tbl, "C:/Users/bella/Documents/DunnTableRoom546WattleMass.xlsx")

# dunnTest automatically performs pairwise comparisons

dt <- dunnTest(Mass ~ Group,
               data = Room547Wattle,
               method = "bonferroni")

dunn_tbl <- dt$res %>%
  mutate(
    Comparison =
      dplyr::case_when(
        # Case 2: FSA output already has a Comparison column
        "Comparison" %in% names(.) ~ Comparison
      ),
    Z = round(Z, 3),
    P.unadj = round(P.unadj, 4),
    P.adj = round(P.adj, 4)
  ) %>%
  select(Comparison, Z, P.unadj, P.adj)

gt_tbl <- dunn_tbl %>%
  gt() %>%
  tab_header(
    title = "Pairwise Dunn Test with Bonferroni Correction"
  ) %>%
  cols_label(
    Comparison = "Group Comparison",
    Z = "Z Statistic",
    P.unadj = "Unadjusted p",
    P.adj = "Bonferroni-adjusted p"
  ) %>%
  fmt_number(
    columns = c(Z, P.unadj, P.adj),
    decimals = 4
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

print(gt_tbl)

gtsave(gt_tbl, "dunn_table.html")

library(writexl)

write_xlsx(dunn_tbl, "C:/Users/bella/Documents/DunnTableRoom547WattleMass.xlsx")

dt <- dunnTest(Mass ~ Group,
               data = Room544Wattle,
               method = "bonferroni")

dunn_tbl <- dt$res %>%
  mutate(
    Comparison =
      dplyr::case_when(
        # Case 2: FSA output already has a Comparison column
        "Comparison" %in% names(.) ~ Comparison
      ),
    Z = round(Z, 3),
    P.unadj = round(P.unadj, 4),
    P.adj = round(P.adj, 4)
  ) %>%
  select(Comparison, Z, P.unadj, P.adj)

gt_tbl <- dunn_tbl %>%
  gt() %>%
  tab_header(
    title = "Pairwise Dunn Test with Bonferroni Correction"
  ) %>%
  cols_label(
    Comparison = "Group Comparison",
    Z = "Z Statistic",
    P.unadj = "Unadjusted p",
    P.adj = "Bonferroni-adjusted p"
  ) %>%
  fmt_number(
    columns = c(Z, P.unadj, P.adj),
    decimals = 4
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

print(gt_tbl)

gtsave(gt_tbl, "dunn_table.html")

library(writexl)

write_xlsx(dunn_tbl, "C:/Users/bella/Documents/DunnTableRoom544WattleMass.xlsx")


#### Wattle RGR ####
# checked, normal enough!
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


hist(WattleH$AverageGR)

# all good, very normal
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
# 
# library(writexl)
# 
# write_xlsx(WattleRGRComparison, "C:/Users/bella/Documents/WattleRGRComparison.xlsx")

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

library(ggplot2)
WattlePlot <- ggplot(data = WattleH, 
                     aes(y = AverageGR, ##Change this to variable name
                         x = Group)) + ##Change this to variable name
  geom_boxplot(fill = "#E9A96C", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Relative growth rate ln(mm/month)") + xlab("Treatment") +   ##Change axis titles
  theme(axis.text.x=element_text(size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
WattlePlot

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

# check for normality
hist(Seedling$Mass)

# try a transformation
Seedling <- Seedling %>%
  mutate(logMass = log(Mass))


hist(Seedling$logMass)

# just for weeds
SeedlingMassWeeds <- Seedling[Seedling$Group != "m", ]

hist(SeedlingMassWeeds$logMass)

# look at the effects
# Fit a model 
M1 <- lmer(logMass ~  factor(Group) +  (1 | Room), data = SeedlingMassWeeds)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 

# Estimated marginal means and pairwise comparisons
library(emmeans)
emm <- emmeans(M1, ~ Group)  
SeedlingBiomassComparison <- pairs(emm, adjust = "tukey")

SeedlingBiomassComparison <- as.data.frame(SeedlingBiomassComparison)

# library(writexl)
# 
# write_xlsx(SeedlingBiomassComparison, "C:/Users/bella/Documents/SeedlingBiomassComparison.xlsx")

# boxplot
ggplot(Seedling, aes(x = Group, y = logMass))+
  geom_boxplot() +
  theme_bw() 

summ_Seedling <- Seedling %>%
  group_by(Group) %>% 
  summarise(meanlog_Mass = mean(logMass),
            sd_logMass = sd(logMass))
print(summ_Seedling)

summ_SeedlingRoom <- Seedling %>%
  group_by(Room) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass))
print(summ_SeedlingRoom)

#Visualisation
Seedling$Group <- as.factor(Seedling$Group)

# Add a column that is the mass of each sample / mean control mass
groupn_mean <- mean(Seedling$logMass[Seedling$Group == "m"], na.rm = TRUE)

Seedling$ResponseRatio <- Seedling$logMass / groupn_mean

# filter to remove baseline for plot
SeedlingPlot <- Seedling[Seedling$Group != "m", ]


SeedlingPlot %>%
  ggplot(aes(x = Group, y = ResponseRatio)) +
  geom_boxplot(fill = "#82C782", varwidth = TRUE, notch = FALSE) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  ylab("Mānuka seedling log biomass mixture / control (g)") +
  xlab("Treatment") +
  theme(axis.text.x=element_text(size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=10, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=12,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))  

# log response ratio
# Seedling
#Function to compute lnRR using metafor for a given plant and control group
library(metafor)
compute_lnRR <- function(Seedling, control_group = "m") {

  # Split into treatment and control
  control <- Seedling %>% filter(Group == control_group)
  treatments <- Seedling %>% filter(Group != control_group)
  
  # Summarise group stats
  group_stats <- treatments %>%
    group_by(Group) %>%
    summarise(
      m1i = mean(logMass),
      sd1i = sd(logMass),
      n1i = n(),
      .groups = "drop"
    ) %>%
    mutate(
      m2i = mean(control$logMass),
      sd2i = sd(control$logMass),
      n2i = nrow(control)
    )
  
  # Compute lnRR and sampling variance
  escalc_results <- escalc(
    measure = "ROM",
    m1i = group_stats$m1i,
    sd1i = group_stats$sd1i,
    n1i = group_stats$n1i,
    m2i = group_stats$m2i,
    sd2i = group_stats$sd2i,
    n2i = group_stats$n2i
  )
  
  # Combine with group info
  results <- bind_cols(group_stats["Group"], escalc_results)
  return(results)
}

# Run the function
lnrr_output <- compute_lnRR(Seedling, control_group = "m")
print(lnrr_output)

# # Fix up the treatment groups
# lnrr_output$Group <- factor(lnrr_output$Group, levels = c("2", "3", "4", "6"), # order  
#                             labels = c("nbp", "np", "nb", "n")) # labels 

# Plot lnRR with 95% CI
ggplot(lnrr_output, aes(x = factor(Group), y = yi)) +
  geom_point(shape = 16, size = 3) +
  geom_errorbar(aes(ymin = yi - 1.96 * sqrt(vi), ymax = yi + 1.96 * sqrt(vi)), width = 0.2) +
  labs(
    x = "Treatment Group",
    y = "lnRR_mixed/monoculture Seedling log root biomass (g)"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 10),
    axis.title = element_text(face = "bold")
  )


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

# check for normality
hist(SeedlingH$AverageGR)

# look at the effects
# Fit a model 
# Fit a model 
M1 <- lmer(AverageGR ~  factor(Group) +  (1 | Room), data = SeedlingH)

summary(M1)

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

#try a transformation
SeedlingH <- SeedlingH %>%
  mutate(cubeGR = cuberoot(AverageGR))

hist(SeedlingH$cubeGR)

# Fit a model 
M1 <- lmer(cubeGR ~  factor(Group) +  (1 | Room), data = SeedlingH)

summary(M1)

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

# try log
SeedlingH <- SeedlingH %>%
  mutate(logGR = log(AverageGR + 1))

hist(SeedlingH$logGR)

# Fit a model 
M1 <- lmer(logGR ~  factor(Group) +  (1 | Room), data = SeedlingH)

summary(M1)

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

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

SeedlingPlot <- ggplot(data = SeedlingH, 
                       aes(y = logGR, ##Change this to variable name
                           x = Group)) + ##Change this to variable name
  geom_boxplot(fill = "#82C782", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Mānuka seedling log(RGR ln(mm/month))") + xlab("Treatment") +   ##Change axis titles
  theme(axis.text.x=element_text(size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
SeedlingPlot 


#### look at Nodules ####
# look at the effects
# Fit a model 
Biomass <- Biomass %>%
  mutate(Nodule_change = Nodule_finish - Nodule_start)

# remove treatment groups that do not have nodules
Nodules <- Biomass %>% filter(Group %in% c("2", "4", "5", "7"))
Nodules <- Nodules %>% filter(Plant == "Wattle")

Nodules$Group <- factor(Nodules$Group, levels = c("2", "4", "5", "7"), # order  
                        labels = c("nbp", "nb", "bp", "b")) # labels 

# # Kruskal-wallis for each room
# Room546Nodules <- Nodules %>% filter(Room == 546)
# 
# kruskal.test(Nodule_change ~ Group, data = Room546Nodules)
# 
# # Kruskal-wallis for each room
# Room547Nodules <- Nodules %>% filter(Room == 547)
# 
# kruskal.test(Nodule_change ~ Group, data = Room547Nodules)
# 
# # Kruskal-wallis for each room
# Room544Nodules <- Nodules %>% filter(Room == 544)
# 
# kruskal.test(Nodule_change ~ Group, data = Room544Nodules)
# 
# # look at differences within rooms
# 
# dt <- dunnTest(Nodule_change ~ Group,
#                data = Room546Nodules,
#                method = "bonferroni")
# 
# dunn_tbl <- dt$res %>%
#   mutate(
#     Comparison =
#       dplyr::case_when(
#         # Case 2: FSA output already has a Comparison column
#         "Comparison" %in% names(.) ~ Comparison
#       ),
#     Z = round(Z, 3),
#     P.unadj = round(P.unadj, 4),
#     P.adj = round(P.adj, 4)
#   ) %>%
#   select(Comparison, Z, P.unadj, P.adj)
# 
# gt_tbl <- dunn_tbl %>%
#   gt() %>%
#   tab_header(
#     title = "Pairwise Dunn Test with Bonferroni Correction"
#   ) %>%
#   cols_label(
#     Comparison = "Group Comparison",
#     Z = "Z Statistic",
#     P.unadj = "Unadjusted p",
#     P.adj = "Bonferroni-adjusted p"
#   ) %>%
#   fmt_number(
#     columns = c(Z, P.unadj, P.adj),
#     decimals = 4
#   ) %>%
#   tab_style(
#     style = cell_text(weight = "bold"),
#     locations = cells_column_labels()
#   )
# 
# print(gt_tbl)
# 
# gtsave(gt_tbl, "dunn_table.html")
# 
# library(writexl)
# 
# write_xlsx(dunn_tbl, "C:/Users/bella/Documents/DunnTableRoom546NodulesNodule_change.xlsx")
# 
# # dunnTest automatically performs pairwise comparisons
# 
# dt <- dunnTest(Nodule_change ~ Group,
#                data = Room547Nodules,
#                method = "bonferroni")
# 
# dunn_tbl <- dt$res %>%
#   mutate(
#     Comparison =
#       dplyr::case_when(
#         # Case 2: FSA output already has a Comparison column
#         "Comparison" %in% names(.) ~ Comparison
#       ),
#     Z = round(Z, 3),
#     P.unadj = round(P.unadj, 4),
#     P.adj = round(P.adj, 4)
#   ) %>%
#   select(Comparison, Z, P.unadj, P.adj)
# 
# gt_tbl <- dunn_tbl %>%
#   gt() %>%
#   tab_header(
#     title = "Pairwise Dunn Test with Bonferroni Correction"
#   ) %>%
#   cols_label(
#     Comparison = "Group Comparison",
#     Z = "Z Statistic",
#     P.unadj = "Unadjusted p",
#     P.adj = "Bonferroni-adjusted p"
#   ) %>%
#   fmt_number(
#     columns = c(Z, P.unadj, P.adj),
#     decimals = 4
#   ) %>%
#   tab_style(
#     style = cell_text(weight = "bold"),
#     locations = cells_column_labels()
#   )
# 
# print(gt_tbl)
# 
# gtsave(gt_tbl, "dunn_table.html")
# 
# library(writexl)
# 
# write_xlsx(dunn_tbl, "C:/Users/bella/Documents/DunnTableRoom547NodulesNodule_change.xlsx")
# 
# dt <- dunnTest(Nodule_change ~ Group,
#                data = Room544Nodules,
#                method = "bonferroni")
# 
# dunn_tbl <- dt$res %>%
#   mutate(
#     Comparison =
#       dplyr::case_when(
#         # Case 2: FSA output already has a Comparison column
#         "Comparison" %in% names(.) ~ Comparison
#       ),
#     Z = round(Z, 3),
#     P.unadj = round(P.unadj, 4),
#     P.adj = round(P.adj, 4)
#   ) %>%
#   select(Comparison, Z, P.unadj, P.adj)
# 
# gt_tbl <- dunn_tbl %>%
#   gt() %>%
#   tab_header(
#     title = "Pairwise Dunn Test with Bonferroni Correction"
#   ) %>%
#   cols_label(
#     Comparison = "Group Comparison",
#     Z = "Z Statistic",
#     P.unadj = "Unadjusted p",
#     P.adj = "Bonferroni-adjusted p"
#   ) %>%
#   fmt_number(
#     columns = c(Z, P.unadj, P.adj),
#     decimals = 4
#   ) %>%
#   tab_style(
#     style = cell_text(weight = "bold"),
#     locations = cells_column_labels()
#   )
# 
# print(gt_tbl)
# 
# gtsave(gt_tbl, "dunn_table.html")
# 
# library(writexl)
# 
# write_xlsx(dunn_tbl, "C:/Users/bella/Documents/DunnTableRoom544NodulesNodule_change.xlsx")

# 
M1 <- lmer(Nodule_change ~  factor(Group) +  (1 | Room), data = Nodules)

summary(M1)

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

hist(Nodules$Nodule_change)

# try a transformation
Nodules <- Nodules %>%
  mutate(logNodules = log(Nodule_change + 1))

hist(Nodules$logNodules)

# Define a custom function
cuberoot <- function(x) {
  sign(x) * abs(x)^(1/3)
}

#try a transformation
Nodules <- Nodules %>%
  mutate(cubeNodules = cuberoot(Nodule_change))

hist(Nodules$cubeNodules)

M1 <- lmer(cubeNodules ~  factor(Group) +  (1 | Room), data = Nodules)

summary(M1)

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")


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
  summarise(mean_Nodules = mean(cubeNodules),
            sd_Nodules = sd(cubeNodules), 
            se_cubeNodules = sd(cubeNodules)/sqrt(n()))
print(summ_Nodules)
ratio <-(max(summ_Nodules$sd_Nodules))/(min(summ_Nodules$sd_Nodules))
print(ratio)

summ_Nodules <- as.data.frame(summ_Nodules)

library(writexl)

write_xlsx(summ_Nodules, "C:/Users/bella/Documents/summ_Nodules.xlsx")


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