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
  scale_x_continuous(breaks = seq(0, 8, by = 1),
  limits = c(-0.5, 8.5)) +
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
SEABuffer$y_xy...4 <- as.numeric(as.factor(SEABuffer$y_xy...4))

# Rename the x column to a more logical name: site
colnames(SEABuffer)[4] <- c("site") ## Renaming the columns

# I need to make a copy of SEA buffer that removes seedlings
SEABufferNoSeedlings <- subset(SEABuffer, select = -c(Flowers_Seeds_PodsHoneysuckle, Area_m2Honeysuckle, HeightMetresHoneysuckle, Flowers_Seeds_PodsMadeiraVine,
                                           Area_m2MadeiraVine, HeightMetresMadeiraVine, Area_m2Periwinkle, HeightMetresPeriwinkle, Flowers_Seeds_PodsEnglishIvy, Area_m2EnglishIvy, 
                                           HeightMetresEnglishIvy,Flowers_Seeds_PodsBlueMorningGlory, Area_m2BlueMorningGlory, HeightMetresBlueMorningGlory, Flowers_Seeds_PodsJasmine, 
                                           Area_m2Jasmine, HeightMetresJasmine, Flowers_Seeds_PodsCoastalBanksia, Area_m2CoastalBanksia, HeightMetresCoastalBanksia, Flowers_Seeds_PodsRhamnus, 
                                           Area_m2Rhamnus, HeightMetresRhamnus, Flowers_Seeds_PodsWoolly, Area_m2Woolly, HeightMetresWoolly,
                                           Area_m2BushyAsparagus, HeightMetresBushyAsparagus, Flowers_Seeds_PodsClimbingAsparagus, Area_m2ClimbingAsparagus, HeightMetresClimbingAsparagus,
                                           Flowers_Seeds_PodsGinger, Area_m2Ginger, Height_metresGinger, Flowers_Seeds_PodsMothPlant, Area_m2MothPlant, HeightMetresMothPlant, EndOutcome, y_xy...56, y, x))

colnames(SEABufferNoSeedlings)[5:17] <- c("A.sericifera", "H.gardnerianum", "A.scandens", "A.densiflorus", "S.mauritianum", "R.alaternus", 
                               "B.integrifolia", "J.polyanthum", "I.tricolor", "H.helix", "V.major", "A.cordifolia", "L.japonica") ## Renaming the columns

SEABufferNoSeedlings[, 5:17] <- lapply(SEABufferNoSeedlings[, 4:16], \(x) ifelse(x == "Seedling", 0, x))

SEABufferNoSeedlings[, 5:17] <- lapply(SEABufferNoSeedlings[, 4:16], function(col) {
  ifelse(!is.na(col), "1", NA) })
SEABufferNoSeedlings <- SEABufferNoSeedlings %>%
  mutate_all(~ replace(., is.na(.), 0))

# remove duplicates
SEABufferNoSeedlings <- SEABufferNoSeedlings |>
  dplyr::arrange(site, CreationDate) |>
  dplyr::filter(!duplicated(site)) |>
  as.data.frame()

# Now I want to see what the most common species richness was across the sites
# I need to calculate sp. richness for each site which is the number of columns
# with a 1 in it

SEABufferNoSeedlings$Richness <- rowSums(SEABufferNoSeedlings[, 5:17] == "1")

lmSEA <- lm(Richness ~ DistanceSEA, data = SEABufferNoSeedlings)

summary(lmSEA)

plot(SEABufferNoSeedlings$Richness, pch = 16, col = "blue")
abline(lmSEA)

ggplot(SEABufferNoSeedlings, aes(DistanceSEA, Richness)) +
  scale_x_continuous(breaks = seq(0, 300, by = 50)) +
  scale_y_continuous(breaks = seq(0, 13, by = 1)) +
  geom_point(color = "orange") +
  geom_smooth(method = lm, color = "orange4", fill = "orange") +
  ylab("Number of weeds present") +
  xlab("Distance to nearest SEA in m") +
  theme_classic()

# I need to change the current df so that it is presence/absence for each species
# first, i will reduce to only one column for each species
SEABuffer <- subset(SEABuffer, select = -c(OldestGrowthStageHoneysuckle, Area_m2Honeysuckle, HeightMetresHoneysuckle, OldestGrowthStageMadeiraVine,
                                             Area_m2MadeiraVine, HeightMetresMadeiraVine, Area_m2Periwinkle, HeightMetresPeriwinkle, OldestGrowthStageEnglishIvy, Area_m2EnglishIvy, 
                                             HeightMetresEnglishIvy, OldestGrowthStageBlueMorningGlory, Area_m2BlueMorningGlory, HeightMetresBlueMorningGlory, OldestGrowthStageJasmine, 
                                             Area_m2Jasmine, HeightMetresJasmine, OldestGrowthStageCoastalBanksia, Area_m2CoastalBanksia, HeightMetresCoastalBanksia, OldestGrowthStageRhamnus, 
                                             Area_m2Rhamnus, HeightMetresRhamnus, OldestGrowthStageWoolly, Area_m2Woolly, HeightMetresWoolly,
                                             Area_m2BushyAsparagus, HeightMetresBushyAsparagus, OldestGrowthStageClimbingAsparagus, Area_m2ClimbingAsparagus, HeightMetresClimbingAsparagus,
                                             OldestGrowthStageGinger, Area_m2Ginger, Height_metresGinger, OldestGrowthStageMothPlant, Area_m2MothPlant, HeightMetresMothPlant, CreationDate, EndOutcome, y_xy...56, y, x))

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

# Calculate summ stats for distance to nearest SEA
summary_SEABuffer <- SEABuffer %>%
  summarise(mean_DistanceSEA = mean(DistanceSEA),
            median_DistanceSEA = median(DistanceSEA),
            IQR_DistanceSEA = IQR(DistanceSEA),
            sd_DistanceSEA = sd(DistanceSEA),
            var_DistanceSEA = var(DistanceSEA),
            se_DistanceSEA = sd(DistanceSEA)/sqrt(n()),
            n_DistanceSEA = n())

# Make one with species
# names of species into the row names
#CloseWeeds2 <- CloseWeeds2[, c(14, setdiff(1:ncol(CloseWeeds2), 14))]
StackData <- SEABuffer
StackData <- subset(StackData, select = -c(ID, DistanceSEA)) 
StackData$site <- as.numeric(StackData$site)

row.names(StackData) <- StackData$site 

# make the columns numeric
StackData$A.sericifera <- as.numeric(StackData$A.sericifera)
StackData$H.gardnerianum <- as.numeric(StackData$H.gardnerianum)
StackData$A.scandens <- as.numeric(StackData$A.scandens)
StackData$A.densiflorus <- as.numeric(StackData$A.densiflorus)
StackData$S.mauritianum <- as.numeric(StackData$S.mauritianum)
StackData$R.alaternus <- as.numeric(StackData$R.alaternus)
StackData$B.integrifolia <- as.numeric(StackData$B.integrifolia)
StackData$J.polyanthum <- as.numeric(StackData$J.polyanthum)
StackData$I.tricolor <- as.numeric(StackData$I.tricolor)
StackData$H.helix <- as.numeric(StackData$H.helix)
StackData$A.cordifolia <- as.numeric(StackData$A.cordifolia)
StackData$L.japonica <- as.numeric(StackData$L.japonica)
StackData$V.major <- as.numeric(StackData$V.major)

StackData <- StackData %>%
  pivot_longer(
    cols = 2:14,
    names_to = "variable",
    values_to = "value"
  )  %>% filter(value == 1) %>%
  select(-value)

# need a column for count of sites
StackData <- StackData %>%
  group_by(Richness) %>%
  mutate(count = n_distinct(site)) %>%
  ungroup()

okabe_ito_13 <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
  "#D55E00", "#CC79A7", "#999999",   # original 8
  "#A6761D", "#1B9E77", "#7570B3", "#E7298A", "#66A61E"  # added distinct tones
)
library(colorspace)

qual_13 <- qualitative_hcl(
  n = 13,
  palette = "Dark3"   # or "Set3", "Harmonic", "Dynamic"
)

props <- StackData %>%
  group_by(Richness, variable) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()


ggplot(StackData, aes(fill = variable, y = count, x = Richness, binwidth = 1)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_continuous(breaks = seq(0, 9, by = 1),
                     limits = c(0.5, 8.5)) +
  scale_fill_manual(values = okabe_ito_13) +
  theme_classic()

ggplot(props, aes(fill = variable, y = prop, x = Richness, binwidth = 1)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_continuous(breaks = seq(0, 9, by = 1),
                     limits = c(0.5, 8.5)) +
  ylab("Proportion of total weeds") +
  xlab("Richness") +
  scale_fill_manual(values = okabe_ito_13) +
  theme(axis.text.x=element_text(size=14, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=14, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=16,face="bold"), #Change axis title text font etc
       #legend.title = element_text("species"), #If you want to remove the legend
      #  legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))  

# some analysis of whether these proportions are different between the proportions 
# for each richness
library(dplyr)
library(purrr)
library(broom)

# Step 1: compute proportions
props <- StackData %>%
  group_by(Richness, variable) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Step 2: run ANOVA for each variable
anova_results <- props %>%
  group_by(variable) %>%
  do(tidy(aov(prop ~ Richness, data = .))) %>%
  ungroup()

anova_results

anova_two_way <- aov(prop ~ Richness, data = props)
summary(anova_two_way)

library(emmeans)
emm <- emmeans(anova_two_way, ~ Richness)  
proportions <- pairs(emm, adjust = "tukey")

library(dplyr)
library(purrr)
library(broom)

# counts per Richness × variable
counts <- StackData %>%
  count(Richness, variable, name = "k") %>%
  group_by(Richness) %>%
  mutate(n = sum(k)) %>%   # total for that richness
  ungroup()

# fit binomial GLM for each variable
glm_results <- counts %>%
  group_by(variable) %>%
  do(tidy(glm(cbind(k, n - k) ~ Richness, family = binomial, data = .))) %>%
  ungroup()

glm_results <- as.data.frame(glm_results)

#### commmon species ####
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
  dplyr::slice_head(n = 15)

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

# How many properties is this?
CloseWeeds %>% 
  summarise(n_unique_sites = n_distinct(site))

# Figure out the farthest weeds
library(dplyr)

FarWeeds <- Weeds %>%
 filter(DistanceSEA > 50)

top_counts <- FarWeeds |>
  dplyr::count(variable, sort = TRUE) |>
  dplyr::slice_head(n = 5)

print(top_counts)

# How many properties is this?
FarWeeds %>% 
  summarise(n_unique_sites = n_distinct(site))

# let's see what species are co-occurring when the richness is 1
lowrichness <- subset(Weeds, Richness == 1)

top_5_values <- names(sort(table(lowrichness$variable), decreasing = TRUE))[1:5] 

print(top_5_values)

top_counts <- lowrichness |>
  dplyr::count(variable, sort = TRUE) |>
  dplyr::slice_head(n = 15)

print(top_counts)

# How many properties is this?
lowrichness %>% 
  summarise(n_unique_sites = n_distinct(site))

# 2 or more weeds 
cooccurring <- subset(Weeds, Richness != 1)

top_counts <- cooccurring |>
  dplyr::count(variable, sort = TRUE) |>
  dplyr::slice_head(n = 13)

print(top_counts)

# How many properties is this?
cooccurring %>% 
  summarise(n_unique_sites = n_distinct(site))

# Let's look at the higher richness
highrichness <- Weeds %>%
  filter(Richness %in% c("6", "7", "8"))

top_counts <- highrichness |>
  dplyr::count(variable, sort = TRUE) |>
  dplyr::slice_head(n = 5)

print(top_counts)

# How many properties is this?
highrichness %>% 
  summarise(n_unique_sites = n_distinct(site))

# let's see what species are co-occurring when the richness is 2
pairrichness <- subset(Weeds, Richness == 2)

top_counts <- pairrichness |>
  dplyr::count(variable, sort = TRUE) |>
  dplyr::slice_head(n = 5)

print(top_counts)

# How many properties is this?
pairrichness %>% 
  summarise(n_unique_sites = n_distinct(site))

#### more co-occurrence matrices ####

#### for weeds closer to SEA ####
CloseWeeds2 <- subset(SEABuffer, DistanceSEA == 0)

CloseWeeds2 <- subset(CloseWeeds2, select = -c(ID, DistanceSEA, Richness)) 

# names of species into the row names
#CloseWeeds2 <- CloseWeeds2[, c(14, setdiff(1:ncol(CloseWeeds2), 14))]
CloseWeeds2$site <- as.numeric(CloseWeeds2$site)

row.names(CloseWeeds2) <- CloseWeeds2$site 
# Remove the first column from the data frame 
CloseWeeds2 <- CloseWeeds2[, -1]

# make the columns numeric
CloseWeeds2$A.sericifera <- as.numeric(CloseWeeds2$A.sericifera)
CloseWeeds2$H.gardnerianum <- as.numeric(CloseWeeds2$H.gardnerianum)
CloseWeeds2$A.scandens <- as.numeric(CloseWeeds2$A.scandens)
CloseWeeds2$A.densiflorus <- as.numeric(CloseWeeds2$A.densiflorus)
CloseWeeds2$S.mauritianum <- as.numeric(CloseWeeds2$S.mauritianum)
CloseWeeds2$R.alaternus <- as.numeric(CloseWeeds2$R.alaternus)
CloseWeeds2$B.integrifolia <- as.numeric(CloseWeeds2$B.integrifolia)
CloseWeeds2$J.polyanthum <- as.numeric(CloseWeeds2$J.polyanthum)
CloseWeeds2$I.tricolor <- as.numeric(CloseWeeds2$I.tricolor)
CloseWeeds2$H.helix <- as.numeric(CloseWeeds2$H.helix)
CloseWeeds2$A.cordifolia <- as.numeric(CloseWeeds2$A.cordifolia)
CloseWeeds2$L.japonica <- as.numeric(CloseWeeds2$L.japonica)

# transpose
CloseWeeds2T <- t(CloseWeeds2)
CloseWeeds2T = as.data.frame(CloseWeeds2T)
dim(CloseWeeds2T)

CloseWeeds2T[is.na(CloseWeeds2T)] <- 0 
CloseWeeds2T[CloseWeeds2T != 0] <- 1
CloseWeeds2T <- CloseWeeds2T[rowSums(CloseWeeds2T) > 1, ]

install.packages("cooccur")
library(cooccur)

cooccur.Survey <- cooccur(CloseWeeds2T,
                          type = "spp_site",
                          thresh = FALSE,
                          spp_name = TRUE)
class(cooccur.Survey)

summary(cooccur.Survey)
cooccur(mat = CloseWeeds2T, type = "spp_site", thresh = FALSE, spp_names = TRUE)

Prob_table <- prob.table(cooccur.Survey)
str(CloseWeeds2T) 
summary(CloseWeeds2T)
# plot the co-occurrence matrix, need to make the plot into an object so that the legend can be removed

plot <- plot(cooccur.Survey, legend = NULL, plotrand = "FALSE") # add "plotrand = TRUE" to include completely random species

# Remove legend 
plot + theme(legend.position = "none") + ggtitle(NULL) + 
  scale_fill_manual(values = c("#C75DAA","#E5E5E5", "#90CBCD")) #Change axis title text font etc

#### for weeds further from SEA ####
FarWeeds2 <- subset(SEABuffer, DistanceSEA > 50)

FarWeeds2 <- subset(FarWeeds2, select = -c(site, ID, Richness)) 

# names of species into the row names
FarWeeds2 <- FarWeeds2[, c(14, setdiff(1:ncol(FarWeeds2), 14))]

row.names(FarWeeds2) <- FarWeeds2$site 
# Remove the first column from the data frame 
FarWeeds2 <- FarWeeds2[, -1]

# make the columns numeric
FarWeeds2$A.sericifera <- as.numeric(FarWeeds2$A.sericifera)
FarWeeds2$H.gardnerianum <- as.numeric(FarWeeds2$H.gardnerianum)
FarWeeds2$A.scandens <- as.numeric(FarWeeds2$A.scandens)
FarWeeds2$A.densiflorus <- as.numeric(FarWeeds2$A.densiflorus)
FarWeeds2$S.mauritianum <- as.numeric(FarWeeds2$S.mauritianum)
FarWeeds2$R.alaternus <- as.numeric(FarWeeds2$R.alaternus)
FarWeeds2$B.integrifolia <- as.numeric(FarWeeds2$B.integrifolia)
FarWeeds2$J.polyanthum <- as.numeric(FarWeeds2$J.polyanthum)
FarWeeds2$I.tricolor <- as.numeric(FarWeeds2$I.tricolor)
FarWeeds2$H.helix <- as.numeric(FarWeeds2$H.helix)
FarWeeds2$A.cordifolia <- as.numeric(FarWeeds2$A.cordifolia)
FarWeeds2$V.major <- as.numeric(FarWeeds2$V.major)

# transpose
FarWeeds2T <- t(FarWeeds2)
FarWeeds2T = as.data.frame(FarWeeds2T)
dim(FarWeeds2T)

FarWeeds2T[is.na(FarWeeds2T)] <- 0 
FarWeeds2T[FarWeeds2T != 0] <- 1
FarWeeds2T <- FarWeeds2T[rowSums(FarWeeds2T) > 1, ]

library(cooccur)

cooccur.Survey <- cooccur(FarWeeds2T,
                          type = "spp_site",
                          thresh = FALSE,
                          spp_name = TRUE)
class(cooccur.Survey)

summary(cooccur.Survey)
cooccur(mat = FarWeeds2T, type = "spp_site", thresh = FALSE, spp_names = TRUE)

Prob_table <- prob.table(cooccur.Survey)
str(FarWeeds2T) 
summary(FarWeeds2T)
# plot the co-occurrence matrix, need to make the plot into an object so that the legend can be removed

plot <- plot(cooccur.Survey, legend = NULL, plotrand = "FALSE") # add "plotrand = TRUE" to include completely random species

# Remove legend 
plot + theme(legend.position = "none") + ggtitle(NULL) + 
  scale_fill_manual(values = c("#C75DAA","#E5E5E5", "#90CBCD")) #Change axis title text font etc

#### 2 co-occurring species ####
Pairs <- subset(SEABuffer, Richness == 2)

Pairs <- subset(Pairs, select = -c(ID, Richness, DistanceSEA)) 

# names of species into the row names
Pairs <- Pairs[, c(14, setdiff(1:ncol(Pairs), 14))]

row.names(Pairs) <- Pairs$site 
# Remove the first column from the data frame 
Pairs <- Pairs[, -1]

# make the columns numeric
Pairs$A.sericifera <- as.numeric(Pairs$A.sericifera)
Pairs$H.gardnerianum <- as.numeric(Pairs$H.gardnerianum)
Pairs$A.scandens <- as.numeric(Pairs$A.scandens)
Pairs$A.densiflorus <- as.numeric(Pairs$A.densiflorus)
Pairs$S.mauritianum <- as.numeric(Pairs$S.mauritianum)
Pairs$R.alaternus <- as.numeric(Pairs$R.alaternus)
Pairs$B.integrifolia <- as.numeric(Pairs$B.integrifolia)
Pairs$J.polyanthum <- as.numeric(Pairs$J.polyanthum)
Pairs$I.tricolor <- as.numeric(Pairs$I.tricolor)
Pairs$H.helix <- as.numeric(Pairs$H.helix)
Pairs$A.cordifolia <- as.numeric(Pairs$A.cordifolia)
Pairs$V.major <- as.numeric(Pairs$V.major)

# transpose
PairsT <- t(Pairs)
PairsT = as.data.frame(PairsT)
dim(PairsT)

PairsT[is.na(PairsT)] <- 0 
PairsT[PairsT != 0] <- 1
PairsT <- PairsT[rowSums(PairsT) > 1, ]

library(cooccur)

cooccur.Survey <- cooccur(PairsT,
                          type = "spp_site",
                          thresh = FALSE,
                          spp_name = TRUE)
class(cooccur.Survey)

summary(cooccur.Survey)
cooccur(mat = PairsT, type = "spp_site", thresh = FALSE, spp_names = TRUE)

Prob_table <- prob.table(cooccur.Survey)
str(PairsT) 
summary(PairsT)
# plot the co-occurrence matrix, need to make the plot into an object so that the legend can be removed

plot <- plot(cooccur.Survey, legend = NULL, plotrand = "FALSE") # add "plotrand = TRUE" to include completely random species

# Remove legend 
plot + theme(legend.position = "none") + ggtitle(NULL) + 
  scale_fill_manual(values = c("#C75DAA","#E5E5E5", "#90CBCD")) #Change axis title text font etc
