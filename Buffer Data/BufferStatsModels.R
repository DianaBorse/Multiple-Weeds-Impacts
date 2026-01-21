#### Buffer general stats and models #### 

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

library(readr)
Bufferdata <- read_csv("Buffer Data/Bufferdata.csv")

# remove rows that do not include surveys
Bufferdata <- Bufferdata %>% filter(EndOutcome != "Q")
Bufferdata <- Bufferdata %>% filter(EndOutcome != "NV")
Bufferdata <- Bufferdata %>% filter(EndOutcome != "NH")
Bufferdata <- Bufferdata %>% filter(EndOutcome != "CB")
Bufferdata <- Bufferdata %>% filter(EndOutcome != "NA")

# remove outliers
Bufferdata<-Bufferdata[-817, ] 
Bufferdata<-Bufferdata[-36, ] 

# Unite coordinate columns
library(tidyr)
Bufferdata <- Bufferdata %>%
  unite(x, y, x, sep = " - ")

# Next I need to give each site a unique numerical ID because the co-occurrence 
# matrix requires this
Bufferdata$x <- as.numeric(as.factor(Bufferdata$x))

# Rename the x column to a more logical name: site
colnames(Bufferdata)[53] <- c("site") ## Renaming the columns

# I need to change the current df so that it is presence/absence for each species
# first, i will reduce to only one column for each species
Bufferdata <- subset(Bufferdata, select = -c(OldestGrowthStageHoneysuckle, Area_m2Honeysuckle, HeightMetresHoneysuckle, OldestGrowthStageMadeiraVine,
                                             Area_m2MadeiraVine, HeightMetresMadeiraVine, Area_m2Periwinkle, HeightMetresPeriwinkle, OldestGrowthStageEnglishIvy, Area_m2EnglishIvy, 
                                             HeightMetresEnglishIvy, OldestGrowthStageBlueMorningGlory, Area_m2BlueMorningGlory, HeightMetresBlueMorningGlory, OldestGrowthStageJasmine, 
                                             Area_m2Jasmine, HeightMetresJasmine, OldestGrowthStageCoastalBanksia, Area_m2CoastalBanksia, HeightMetresCoastalBanksia, OldestGrowthStageRhamnus, 
                                             Area_m2Rhamnus, HeightMetresRhamnus, OldestGrowthStageWoolly, Area_m2Woolly, HeightMetresWoolly,
                                             Area_m2BushyAsparagus, HeightMetresBushyAsparagus, OldestGrowthStageClimbingAsparagus, Area_m2ClimbingAsparagus, HeightMetresClimbingAsparagus,
                                             OldestGrowthStageGinger, Area_m2Ginger, Height_metresGinger, OldestGrowthStageMothPlant, Area_m2MothPlant, HeightMetresMothPlant, CreationDate, EndOutcome))

colnames(Bufferdata)[1:13] <- c("A.sericifera", "H.gardnerianum", "A.scandens", "A.densiflorus", "S.mauritianum", "R.alaternus", 
                                "B.integrifolia", "J.polyanthum", "I.tricolor", "H.helix", "V.major", "A.cordifolia", "L.japonica") ## Renaming the columns

# Now I need any rows that are NA to be 0 and any with text to be 1
Bufferdata[, 1:13] <- lapply(Bufferdata[, 1:13], function(col) {
  ifelse(!is.na(col), "1", NA) })
Bufferdata <- Bufferdata %>%
  mutate_all(~ replace(., is.na(.), 0))


# remove duplicates
Bufferdata <- Bufferdata[!duplicated(Bufferdata$site), ]

Bufferdata = as.data.frame(Bufferdata)

# Now I want to see what the most common species richness was across the sites
# I need to calculate sp. richness for each site which is the number of columns
# with a 1 in it

Bufferdata$Richness <- rowSums(Bufferdata[, 1:13] == "1")

summary_Buffer <- Bufferdata %>%
  summarise(mean_Richness = mean(Richness),
            median_Richness = median(Richness),
            IQR_Richness = IQR(Richness),
            sd_Richness = sd(Richness),
            var_Richness = var(Richness),
            se_Richness = sd(Richness)/sqrt(n()),
            n_Richness = n())

ggplot(Bufferdata) +
  geom_histogram(aes(Richness), binwidth = 1, color = "orange3", fill = "orange") +
  scale_x_continuous(breaks = seq(0, 13, by = 1),
  limits = c(-0.5, 13)) +
  ylab("Count of gardens") +
  xlab("Number of weeds present") +
  theme_classic()

# calculate the probability of having 0, 1, 2 - 8 weeds

library(dplyr)

richness_probs <- Bufferdata %>%
  count(Richness) %>%                 # counts of each value
  mutate(probability = n / sum(n))    # convert to probabilities

#### distance to nearest SEA analysis ####

library(readr)
SEABuffer <- read_csv("Buffer Data/DistanceSEABuffer.csv")

# Next I need to give each site a unique numerical ID because the co-occurrence 
# matrix requires this
SEABuffer$x...4 <- as.numeric(as.factor(SEABuffer$x...4))

# Rename the x column to a more logical name: site
colnames(SEABuffer)[4] <- c("site") ## Renaming the columns

# I need to change the current df so that it is presence/absence for each species
# first, i will reduce to only one column for each species
SEABuffer <- subset(SEABuffer, select = -c(OldestGrowthStageHoneysuckle, Area_m2Honeysuckle, HeightMetresHoneysuckle, OldestGrowthStageMadeiraVine,
                                             Area_m2MadeiraVine, HeightMetresMadeiraVine, Area_m2Periwinkle, HeightMetresPeriwinkle, OldestGrowthStageEnglishIvy, Area_m2EnglishIvy, 
                                             HeightMetresEnglishIvy, OldestGrowthStageBlueMorningGlory, Area_m2BlueMorningGlory, HeightMetresBlueMorningGlory, OldestGrowthStageJasmine, 
                                             Area_m2Jasmine, HeightMetresJasmine, OldestGrowthStageCoastalBanksia, Area_m2CoastalBanksia, HeightMetresCoastalBanksia, OldestGrowthStageRhamnus, 
                                             Area_m2Rhamnus, HeightMetresRhamnus, OldestGrowthStageWoolly, Area_m2Woolly, HeightMetresWoolly,
                                             Area_m2BushyAsparagus, HeightMetresBushyAsparagus, OldestGrowthStageClimbingAsparagus, Area_m2ClimbingAsparagus, HeightMetresClimbingAsparagus,
                                             OldestGrowthStageGinger, Area_m2Ginger, Height_metresGinger, OldestGrowthStageMothPlant, Area_m2MothPlant, HeightMetresMothPlant, CreationDate, EndOutcome, x...57, y, y_xy))

colnames(SEABuffer)[4:16] <- c("A.sericifera", "H.gardnerianum", "A.scandens", "A.densiflorus", "S.mauritianum", "R.alaternus", 
                                "B.integrifolia", "J.polyanthum", "I.tricolor", "H.helix", "V.major", "A.cordifolia", "L.japonica") ## Renaming the columns

# Now I need any rows that are NA to be 0 and any with text to be 1
SEABuffer[, 4:16] <- lapply(SEABuffer[, 4:16], function(col) {
  ifelse(!is.na(col), "1", NA) })
SEABuffer <- SEABuffer %>%
  mutate_all(~ replace(., is.na(.), 0))

# remove duplicates
SEABuffer <- SEABuffer[!duplicated(SEABuffer$site), ]

SEABuffer = as.data.frame(SEABuffer)

# Now I want to see what the most common species richness was across the sites
# I need to calculate sp. richness for each site which is the number of columns
# with a 1 in it

SEABuffer$Richness <- rowSums(SEABuffer[, 4:16] == "1")

lmSEA <- lm(Richness ~ DistanceSEA, data = SEABuffer)

summary(lmSEA)

plot(SEABuffer$Richness, pch = 16, col = "blue")
abline(lmSEA)

ggplot(SEABuffer, aes(DistanceSEA, Richness)) +
  scale_x_continuous(breaks = seq(0, 300, by = 50)) +
  scale_y_continuous(breaks = seq(0, 13, by = 1)) +
  geom_point(color = "orange") +
  geom_smooth(method = lm, color = "orange4", fill = "orange") +
  ylab("Number of weeds present") +
  xlab("Distance to nearest SEA in m") +
  theme_classic()


# Let's look at what species were most common closest to SEAs
library(dplyr)
library(tidyr)
SEABuffer$R.alaternus <- as.character(as.factor(SEABuffer$R.alaternus))

Weeds <- SEABuffer %>%
  pivot_longer(
    cols = 4:16,
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(value == 1) %>%
  select(-value)

# What were the most common weeds overall
top_counts <- Weeds |>
  dplyr::count(variable, sort = TRUE) |>
  dplyr::slice_head(n = 5)

print(top_counts)

# Great, now we have the weeds present for each plot, let's look at the five most
# common for plots with distance to sea of 0
CloseWeeds <- subset(Weeds, DistanceSEA == 0)

top_5_values <- names(sort(table(CloseWeeds$variable), decreasing = TRUE))[1:5] 

print(top_5_values)

top_counts <- CloseWeeds |>
  dplyr::count(variable, sort = TRUE) |>
  dplyr::slice_head(n = 5)

print(top_counts)

# Figure out the farthest weeds

# let's see what species are co-occurring when the richness is 1
lowrichness <- subset(Weeds, Richness == 1)

top_5_values <- names(sort(table(lowrichness$variable), decreasing = TRUE))[1:5] 

print(top_5_values)

top_counts <- lowrichness |>
  dplyr::count(variable, sort = TRUE) |>
  dplyr::slice_head(n = 5)

print(top_counts)

# Let's look at the higher richness
highrichness <- Weeds %>%
  filter(Richness %in% c("6", "7", "8"))

top_counts <- highrichness |>
  dplyr::count(variable, sort = TRUE) |>
  dplyr::slice_head(n = 5)

print(top_counts)
