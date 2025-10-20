#### GLM ####

# This is my most up-to-date GLM with model selection

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

library(dplyr)

#library(readr)
#SurveyData <- read_csv("SurveyData_Clean.csv")

library(readr)
SurveyData <- read_csv("Fieldwork/SurveyData_Clean_WN_removed.csv")

#library(readr)
#PlotData <- read_csv("PlotData_Clean.csv")

library(readr)
PlotData <- read_csv("Fieldwork/PlotData_Clean_WN_removed.csv")

# The plot needs to be numeric, so I need to change the Plot names to unique
# numeric variables, site needs to be given a number value as does W_N

# This code gives the combined plot names a simple, unique, numeric ID, but I 
# would like clearer ID's so I will use another method
# SurveyData_Combined$Plot <- as.numeric(as.factor(SurveyData_Combined$Plot))

# Assigning the Site a unique numeric value
SurveyData$Site <- as.numeric(as.factor(SurveyData$Site))
# Assigning W and N unique numeric values (weed is now 2, native is now 1)
SurveyData$W_N <- as.numeric(as.factor(SurveyData$W_N))

# Now combine this into unique numerical plot names
library(tidyr)
SurveyData_Combined <- SurveyData %>%
  unite(Plot, Site, Plot, sep = "-")

# Unite Plot and Weed vs Native columns so that now the plot id is all in one
# column that includes the site, plot number, and whether it is weed or native
library(tidyr)
SurveyData_Combined <- SurveyData_Combined %>%
  unite(Plot, Plot, W_N, sep = "-")

# I need to add columns for the count of the weeds 150 and over in each plot
# Calculate the number of wn over 150 for each plot (tier_4)
#TallWeeds <- SurveyData_Combined %>%
#  mutate(SOLmau = ifelse(ScientificName == "Solanum mauritianum", Tier_4, 0))

# For privet
#TallWeeds <- TallWeeds %>%
#  mutate(LIGluc = ifelse(ScientificName == "Ligustrum lucidum", Tier_4, 0))

# For wattle
#TallWeeds <- TallWeeds %>%
#  mutate(PARlop = ifelse(ScientificName == "Paraserianthes lophantha", Tier_4, 0))

# Remove all other columns
#library(dplyr)
#TallWeeds <- TallWeeds %>%
#  select(Plot, SOLmau, LIGluc, PARlop)

# Only include rows with unique values for Plot
#TallWeeds <- TallWeeds %>%
#  distinct(Plot, .keep_all = TRUE)

# Calculate Shannon diversity index of native species > 50 cm tall
# tier 3 will include both those in tier 3 and 4, so I need native sp. diversity 
# in tier 3

# Load required package
library(vegan)
library(dplyr)

# Filter for WeedList == 0
filtered_SurveyData <- SurveyData_Combined %>% 
  filter(WeedList == 0)

# Step 2: Summarize abundance per species per plot
#abundance_matrix <- filtered_SurveyData %>%
#  group_by(Plot, ScientificName) %>%
#  summarise(Abundance = sum(Tier_3, na.rm = TRUE), .groups = "drop") %>%
#  tidyr::pivot_wider(names_from = ScientificName, values_from = Abundance, values_fill = 0)

# This summarizes abundance per species per plot by form-based resident species calculations
abundance_matrix <- filtered_SurveyData %>%
  mutate(Abundance = case_when(
    GrowthHabit %in% c("Tree", "Shrub") ~ Tier_3,
    GrowthHabit %in% c("Vine", "Grass", "Forb") ~ Tier_2,
    TRUE ~ NA_real_  # Handles unexpected GrowthHabit values
  )) %>%
  group_by(Plot, ScientificName) %>%
  summarise(Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ScientificName, values_from = Abundance, values_fill = 0)

# Step 3: Calculate Shannon diversity per plot
# Remove Plot column for diversity calculation, but keep it for merging
plot_ids <- abundance_matrix$Plot
abundance_matrix_numeric <- abundance_matrix %>% select(-Plot)

shannon_values <- diversity(abundance_matrix_numeric, index = "shannon")

# View result
print(shannon_values)

# Step 4: Create dataframe and merge with PlotData
shannon_by_plot <- data.frame(Plot = plot_ids, NativeDiversity = shannon_values)

# Let's try weed diversity of resident species.
# Filter for WeedList == 1
filtered_SurveyDataWeed <- SurveyData_Combined %>% 
  filter(WeedList == 1)

#abundance_matrix <- SurveyData_Combined %>%
#  group_by(Plot, ScientificName) %>%
#  summarise(Abundance = sum(Tier_3, na.rm = TRUE), .groups = "drop") %>%
#  tidyr::pivot_wider(names_from = ScientificName, values_from = Abundance, values_fill = 0)

# This summarizes abundance per species per plot by form-based resident species calculations
abundance_matrix <- filtered_SurveyDataWeed %>%
  mutate(Abundance = case_when(
    GrowthHabit %in% c("Tree", "Shrub") ~ Tier_3,
    GrowthHabit %in% c("Vine", "Grass", "Forb") ~ Tier_2,
    TRUE ~ NA_real_  # Handles unexpected GrowthHabit values
  )) %>%
  group_by(Plot, ScientificName) %>%
  summarise(Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ScientificName, values_from = Abundance, values_fill = 0)


# Step 3: Calculate Shannon diversity per plot
# Remove Plot column for diversity calculation, but keep it for merging
plot_ids <- abundance_matrix$Plot
abundance_matrix_numeric <- abundance_matrix %>% select(-Plot)

shannon_values <- diversity(abundance_matrix_numeric, index = "shannon")

# View result
print(shannon_values)

# Step 4: Create dataframe and merge with PlotData
shannon_by_plot_weed <- data.frame(Plot = plot_ids, WeedDiversity = shannon_values)


# subset to just look at the weeds
# Subset the data to only include species that are weeds using the WeedList Column
SurveyData_Combined <- SurveyData_Combined[SurveyData_Combined$WeedList == 1, ]

# I need to make this data only include seedlings. Therefore, I need to remove
# the rows that only include species > 51 cm.

#SurveyData_Combined$seedlings <- SurveyData_Combined$Tier_1 - SurveyData_Combined$Tier_3

# Instead of Shannon Diversity, can see if simple richness of resident species
# is a good predictor of weed seedling richness
# Filter for WeedList == 0
filtered_SurveyData <- SurveyData_Combined %>% 
  filter(WeedList == 0)

library(dplyr)

SurveyData_CombinedNativeRichness <- filtered_SurveyData %>%
  mutate(ResdidentNatives = case_when(
    GrowthHabit %in% c("Tree", "Shrub") ~ Tier_3,
    GrowthHabit %in% c("Forb", "Vine", "Grass") ~ Tier_2,
    TRUE ~ NA_real_  # fallback for unexpected GrowthHabit values
  ))

# Calculate richness value
library(dplyr)
RichnessNatives <- SurveyData_CombinedNativeRichness %>% group_by(Plot) %>% summarize(unique_values = n_distinct(ScientificName))

# Now for resident weed richness
filtered_SurveyData <- SurveyData_Combined %>% 
  filter(WeedList == 1)

library(dplyr)

SurveyData_CombinedWeedRichness <- filtered_SurveyData %>%
  mutate(ResdidentWeeds = case_when(
    GrowthHabit %in% c("Tree", "Shrub") ~ Tier_3,
    GrowthHabit %in% c("Forb", "Vine", "Grass") ~ Tier_2,
    TRUE ~ NA_real_  # fallback for unexpected GrowthHabit values
  ))

# Calculate richness value
library(dplyr)
RichnesResidentsWeeds <- SurveyData_CombinedNativeRichness %>% group_by(Plot) %>% summarize(unique_values = n_distinct(ScientificName))

# rename the column
colnames(RichnesResidentsWeeds)[2] <- c("RichnessResidentWeeds") ## Renaming the columns# rename the column
# Now let's use a form-specific cutoff for seedling status
library(dplyr)

SurveyData_Combined <- SurveyData_Combined %>%
  mutate(seedlings = case_when(
    GrowthHabit %in% c("Tree", "Shrub") ~ Tier_1 - Tier_3,
    GrowthHabit %in% c("Forb", "Vine", "Grass") ~ Tier_1 - Tier_2,
    TRUE ~ NA_real_  # fallback for unexpected GrowthHabit values
  ))

# Remove any rows for which seedlings is empty so that richness is only calculated 
# From seedlings
SurveyData_Combined <- SurveyData_Combined %>% 
  filter(seedlings != 0)

# Calculate richness value
library(dplyr)
RichnessWeed <- SurveyData_Combined %>% group_by(Plot) %>% summarize(unique_values = n_distinct(ScientificName))

# rename the column
colnames(RichnessWeed)[2] <- c("Richness") ## Renaming the columns# rename the column


# Repeat for plot data
# Assigning the Site a unique numeric value
PlotData$Site <- as.numeric(as.factor(PlotData$Site))
# Assigning W and N unique numeric values (weed is now 2, native is now 1)
PlotData$Weed_Native <- as.numeric(as.factor(PlotData$Weed_Native))

# Create another Column for WeedvsNative central species
PlotData$WN <- PlotData$Weed_Native
# Making a column for site so I can add it as a factor
PlotData$Place <- PlotData$Site


# Now combine this into unique numerical plot names
library(tidyr)
PlotData_Combined <- PlotData %>%
  unite(Plot, Site, Plot, sep = "-")

# Unite Plot and Weed vs Native columns so that now the plot id is all in one
# column that includes the site, plot number, and whether it is weed or native
library(tidyr)
PlotData_Combined <- PlotData_Combined %>%
  unite(Plot, Plot, Weed_Native, sep = "-")

#PlotData_Combined <- left_join(TallWeeds, PlotData_Combined, by = "Plot")

# Add diversity to plot data
PlotData_Combined <- PlotData_Combined %>%
  left_join(shannon_by_plot, by = "Plot")

PlotData_Combined <- PlotData_Combined %>%
  mutate(NativeDiversity = if_else(is.na(NativeDiversity), 0, NativeDiversity))

# Add weed diversity as well
PlotData_Combined <- PlotData_Combined %>%
  left_join(shannon_by_plot_weed, by = "Plot")

PlotData_Combined <- PlotData_Combined %>%
  mutate(WeedDiversity = if_else(is.na(WeedDiversity), 0, WeedDiversity))

# Now I need to only include the environmental variables that I want to include
# for the GLM

PlotData_Combined <- subset(PlotData_Combined, select = -c(Date, CentralSpecies, CanopyCover_App, Waypoint, CoverGrass,
                                                           Topography, ParentMaterial, Notes))

# Need to simplify the column names 
colnames(PlotData_Combined)[2:20] <- c("Housing","PopnHist", "PopnCurr", "DwellingsCurr",
                                       "Canopy", "Height", "DBH",
                                       "Slope", "Erosion", "Disturbance",
                                       "Pests", "Litter", "East", "South", 
                                       "Vascular", "NonVascular", "LitterCover",
                                       "Bare", "Debris") ## Renaming the columns



# Include Richness in the plot data
Env_Species <- left_join(RichnessWeed, PlotData_Combined, by = "Plot")  # Preserves all rows from df1


#### PCA ####
library(factoextra)

Env_Species.pca <- prcomp(PlotData_Combined[,c("Height", "DBH", 
                                               "Slope", "Canopy", "East", "South", "Vascular", "NonVascular", "LitterCover",
                                               "Bare", "Debris", "Erosion", "Disturbance",
                                               "Pests", "Litter", "Housing","PopnHist", "PopnCurr", "DwellingsCurr", "WN", 
                                               "Place", "NativeDiversity", "WeedDiversity")], center = TRUE,scale. = TRUE,tol = 0.1)
summary(Env_Species.pca)
Env_Species.pca


#this generates the PC scores for each plot
axes_Env_Species.pca <- predict(Env_Species.pca, newdata = Env_Species)
#making sure it worked
head(axes_Env_Species.pca, 4)

#creating a new dataframe that adds the the PC scores to the end
df_Env_Species.pca <- cbind(Env_Species, axes_Env_Species.pca)

fviz_eig(Env_Species.pca,addlabels = TRUE) #scree plot

eig.val <- get_eigenvalue(Env_Species.pca) #getting eighvalue from each pca
eig.val

pca.var <- get_pca_var(Env_Species.pca)
pca.var$contrib
pca.var$coord
pca.var$cos2


# % contribution of the variables 
fviz_pca_var(Env_Species.pca, axes = c(1, 2), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


library(MASS) ## do to the GLM
RichnessGLM <- glm.nb(Richness ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                        PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + 
                        PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + 
                        PC20 + PC21 + PC22,
                      data = df_Env_Species.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(RichnessGLM)


#### AIC for model selection ####

library(MuMIn)
options(na.action = "na.fail") #Must run this code once to use dredge
model.full <- lm(Richness ~ Height + DBH + Slope + Canopy + Vascular + NonVascular+
                   LitterCover + Bare + Debris + Erosion + Disturbance + Pests + WN +
                   Litter + Housing + PopnHist + PopnCurr + DwellingsCurr + Place + NativeDiversity + WeedDiversity, data = df_Env_Species.pca)

# Richness GLM
RichnessGLM2 <- glm.nb(Richness ~ Height + DBH + Slope + Canopy + Vascular + NonVascular+
                         LitterCover + Bare + Debris + Erosion + Disturbance + Pests + WN +
                         Litter + Housing + PopnHist + PopnCurr + DwellingsCurr + Place + NativeDiversity + WeedDiversity, data = df_Env_Species.pca)

summary(RichnessGLM2)

# Look for Multicolliniarity
library(car)
vif(model.full)

# Output shows that Vascular, LitterCover, Bar, PopnCurr, and DwellingsCurr all
# have values greater than 5, so these should be omitted

# Creates a visualization
vif_values <- vif(model.full)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)

# look at how they correlate
data <- df_Env_Species.pca[ , c("LitterCover", "Bare", "Vascular", "PopnCurr", "DwellingsCurr")]
cor(data)

# vascular and litter cover are highly correlated. Vascular is a more significant 
# factor, so I will keep vascular cover and remove litter cover and bare soil
# Current population and the current dwellings are highly correlated. I will remove 
# current dwellings because this is somewhat covered by housing density within 250 m
# and current population is more comparable to 96 population
# Bare soil is highly correlated with several other factors, so I will remove it 
# as well

# Re-doing the multicolinarity tests with the reduced factor set
# Make a new model with at least one of the correlated factors omitted 
model.full <- glm(Richness ~ Height + DBH + Slope + Canopy + Vascular + Pests + WN +
                  Erosion + Disturbance + Litter + Housing + PopnHist + 
                  PopnCurr + Place + NativeDiversity + WeedDiversity, family = "poisson", data = df_Env_Species.pca)

# Look for Multicolliniarity
library(car)
vif(model.full)

# look at how they correlate
data <- df_Env_Species.pca[ , c("Height", "DBH", "Slope", "Canopy", "Vascular", "Pests", "WN",
                                "Erosion", "Disturbance", "Litter", "Housing", "PopnHist", "PopnCurr", "Place", "NativeDiversity", "WeedDiversity")]
cor(data)


# Run all iterations of the model
dredge <- dredge(model.full, rank = "AICc", extra = c("R^2", adjRsq = function(x) summary(x)$adj.r.squared))

head(dredge, 10)

# Let's try it with just the factors from the top model of that result: housing 
# density, 2023 population, 1996 population, SOLmau, PARlop, Slope, Vascular veg
#model.full <- glm(Richness ~ Housing +
 #                  PopnHist + PopnCurr + Vascular +
#                   Height + Erosion + Slope,
#                  family = "poisson", data = df_Env_Species.pca)

#dredge <- dredge(model.full, rank = "AICc", extra = c("R^2", adjRsq = function(x) summary(x)$adj.r.squared))

#head(dredge, 10)

# Install and load the writexl package
library(writexl)

write_xlsx(dredge, "C:/Users/bella/Documents/dredgeresultsGLM9Oct.xlsx")
# This gives the simplest possible model which includes Housing Density, 96 Population, 
# LitterCover, PARlop, SOLmau, and Slope

# Compare factors to how they relate to richness
library(MASS) ## do to the GLM
RichnessGLM <- glm.nb(Richness ~ Housing +
                        Vascular +  Slope + WeedDiversity,
                      data = df_Env_Species.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(RichnessGLM)

#### Visualizing the results ####
library(lattice)
library(R2jags)
source(file = "GLM Book Resources/HighstatLibV10.R")
source(file = "GLM Book Resources/MCMCSupportHighstatV4.R")

MyVar <- c("Housing", "Slope", "Vascular", "WeedDiversity")
Mydotplot(Env_Species[MyVar])

corvif(Env_Species[MyVar])

Myxyplot(df_Env_Species.pca, MyVar, "Richness")
# unclear why, but this only works with the pca dataframe

# Fit a model 
M1 <- glm(Richness ~ Housing +
            Vascular + Slope + WeedDiversity,
          family = "poisson", data = Env_Species)

summary(M1)

# look at the effects
library(effects)

eff <- allEffects(M1)
summary(eff)
plot(eff)


percent_change <- (exp(coef(M1)["Housing"])- 1)*100
print(percent_change)
(exp(coef(M1)["Vascular"])- 1)*100
(exp(coef(M1)["Slope"])- 1)*100
(exp(coef(M1)["WeedDiversity"])- 1)*100


# Housing density effects plot
plot(Effect("Housing", M1), xlab = "Housing density within 250 meters",
     ylab = "Weed richness", main = "", colors = "royalblue", cex.lab = 2)

# 96 population effects plot
#plot(Effect("PopnHist", M1), xlab = "Population of the area 1996",
#     ylab = "Weed richness", main = "", colors = "pink3")

# 23 population effects plot
plot(Effect("Vascular", M1), xlab = "Cover of vascular vegetation in the plot",
     ylab = "Weed richness", main = "", colors = "#660099", cex.lab = 2)

# Vascular Vegetation effects plot
#plot(Effect("Vascular", M1), xlab = "Cover of vascular vegetation",
#     ylab = "Weed richness", main = "", colors = "#043927")

# Woolly Nightshade effects plot
#plot(Effect("SOLmau", M1), xlab = "Number of S. mauritianum taller than 150 cm",
#     ylab = "Weed richness", main = "", colors = "#882265")

# Brush Wattle effects plot
#plot(Effect("PARlop", M1), xlab = "Number of P. lophantha taller than 150 cm",
#     ylab = "Weed richness", main = "", colors = "orange3")

# Slope effects plot
plot(Effect("Slope", M1), xlab = "Slope of the plot",
     ylab = "Weed richness", main = "", colors = "maroon", cex.lab = 2)

# Diversity effects plot
plot(Effect("WeedDiversity", M1), xlab = "Diversity of resident weeds in the plot",
     ylab = "Weed richness", main = "", colors = "darkgreen", cex.lab = 2)

# can do for nb glm as well
library(MASS)
M2 <- glm.nb(Richness ~ Housing +
               PopnHist + PopnCurr + Vascular +
               SOLmau + PARlop + Slope,
               data = Env_Species)
E2 <- resid(M2, type = "pearson")
N <- nrow(Env_Species)
p <- length(coef(M2)) + 1
sum(E2^2) / (N - p)

summary(M2)
drop1(M2, test = "Chi")


# exponentiating the estimates# exponentiating the estimTRUEates
exp(coef(M1))

exp(confint(RichnessGLM))


library(DHARMa)
testOverdispersionParametric(M1)


library(performance)
check_overdispersion(M1)





#### Old plotting ####
# Making a plot

#find the range of each variable
range(Env_Species$Housing)
range(Env_Species$PopnHist)
range(Env_Species$PopnCurr)
range(Env_Species$Vascular)
range(Env_Species$SOLmau)
range(Env_Species$PARlop)
range(Env_Species$Slope)

# Calculate the means
mean_Housing <- mean(Env_Species$Housing) # Housing Mean
print(mean_Housing)
mean_PopnHist <- mean(Env_Species$PopnHist) # 96 Population Mean
print(mean_PopnHist)
mean_Vascular <- mean(Env_Species$Vascular) # Vascular mean
print(mean_Vascular)
mean_SOLmau <- mean(Env_Species$SOLmau) # Woolly mean
print(mean_SOLmau)
mean_PARlop <- mean(Env_Species$PARlop) # Wattle Mean
print(mean_PARlop)
mean_Slope <- mean(Env_Species$Slope) # Slope mean
print(mean_Slope)
mean_PopnCurr <- mean(Env_Species$PopnCurr) # Current Popn mean
print(mean_PopnCurr)

# Housing Plot
plot(x = Env_Species$Housing, y = Env_Species$Richness, 
     xlab = "Housing density within 250 meters",
     ylab = "Weed richness", type = "n")
MyData <- data.frame(Housing = seq(0, 258, length = 139),
                     PopnHist = 3996.604, Vascular = 36.49683, SOLmau = 0.04316547, 
                     PARlop = 0.02877698, Slope = 13.8, PopnCurr = 2509.273)
P1 <- predict(M1, newdata = MyData, 
              type = "link", se = TRUE)
lines(x = MyData$Housing, y = exp(P1$fit), col = "royalblue", lwd = 2)
lines(x = MyData$Housing,
      y = exp(P1$fit + 2 * P1$se.fit), col = "deepskyblue", lty = 2)
lines(x = MyData$Housing,
      y = exp(P1$fit - 2 * P1$se.fit), col = "deepskyblue", lty = 2)
text(x = 175, y = 7, labels = paste0("Exponent =  1.003"), 
     pos = 4, col = "royalblue", cex = 1)

# 96 Population Plot
plot(x = Env_Species$PopnHist, y = Env_Species$Richness, 
     xlab = "Population of the area 1996",
     ylab = "Weed richness", type = "n")
MyData <- data.frame(PopnHist = seq(1608, 7350, length = 139),
                     Housing = 95.29496, Vascular = 36.49683, SOLmau = 0.04316547, 
                     PARlop = 0.02877698, Slope = 13.8, PopnCurr = 2509.273)
P1 <- predict(M1, newdata = MyData, 
              type = "link", se = TRUE)
lines(x = MyData$PopnHist, y = exp(P1$fit), col = "pink3", lwd = 2)
lines(x = MyData$PopnHist,
      y = exp(P1$fit + 2 * P1$se.fit), col = "pink2", lty = 2)
lines(x = MyData$PopnHist,
      y = exp(P1$fit - 2 * P1$se.fit), col = "pink2", lty = 2)
text(x = 6000, y = 7, labels = paste0("Exponent =  1.000"), 
     pos = 4, col = "#043927", cex = 1)

# 23 Population Plot
plot(x = Env_Species$PopnCurr, y = Env_Species$Richness, 
     xlab = "Population of the area 2023",
     ylab = "Weed richness", type = "n")
MyData <- data.frame(PopnCurr = seq(264, 4194, length = 139),
                     Housing = 95.29496, Vascular = 36.49683, SOLmau = 0.04316547, 
                     PARlop = 0.02877698, Slope = 13.8, PopnHist = 3996.604)
P1 <- predict(M1, newdata = MyData, 
              type = "link", se = TRUE)
lines(x = MyData$PopnCurr, y = exp(P1$fit), col = "#660099", lwd = 2)
lines(x = MyData$PopnCurr,
      y = exp(P1$fit + 2 * P1$se.fit), col = "#CC33FF", lty = 2)
lines(x = MyData$PopnCurr,
      y = exp(P1$fit - 2 * P1$se.fit), col = "#CC33FF", lty = 2)
text(x = 6000, y = 7, labels = paste0("Exponent =  1.000"), 
     pos = 4, col = "#043927", cex = 1)

# Vascular Vegetation Plot
plot(x = Env_Species$Vascular, y = Env_Species$Richness, 
     xlab = "Cover of vascular vegetation",
     ylab = "Weed richness", type = "n")
MyData <- data.frame(Vascular = seq(2.25, 84.75, length = 139),
                     Housing = 95.29496, PopnHist = 3996.604, SOLmau = 0.04316547, 
                     PARlop = 0.02877698, Slope = 13.8, PopnCurr = 2509.273)
P1 <- predict(M1, newdata = MyData, 
              type = "link", se = TRUE)
lines(x = MyData$Vascular, y = exp(P1$fit), col = "#043927", lwd = 2)
lines(x = MyData$Vascular,
      y = exp(P1$fit + 2 * P1$se.fit), col = "#50C878", lty = 2)
lines(x = MyData$Vascular,
      y = exp(P1$fit - 2 * P1$se.fit), col = "#50C878", lty = 2)
text(x = 60, y = 7, labels = paste0("Exponent =  1.004"), 
     pos = 4, col = "pink3", cex = 1)


# Woolly Nightshade Plot
plot(x = Env_Species$SOLmau, y = Env_Species$Richness, 
     xlab = "Number of S. mauritianum taller than 150 cm",
     ylab = "Weed richness", type = "n")
MyData <- data.frame(SOLmau = seq(0, 4, length = 139),
                     Housing = 95.29496, PopnHist = 3996.604, Vascular = 36.49683, 
                     PARlop = 0.02877698, Slope = 13.8, PopnCurr = 2509.273)
P1 <- predict(M1, newdata = MyData, 
              type = "link", se = TRUE)
lines(x = MyData$SOLmau, y = exp(P1$fit), col = "#882265", lwd = 2)
lines(x = MyData$SOLmau,
      y = exp(P1$fit + 2 * P1$se.fit), col = "#882240", lty = 2)
lines(x = MyData$SOLmau,
      y = exp(P1$fit - 2 * P1$se.fit), col = "#882240", lty = 2)
text(x = 2.75, y = 7, labels = paste0("Exponent =  1.24"), 
     pos = 4, col = "#882265", cex = 1)

# Brush wattle Plot
plot(x = Env_Species$PARlop, y = Env_Species$Richness, 
     xlab = "Number of P. lophantha taller than 150 cm",
     ylab = "Weed richness", type = "n")
MyData <- data.frame(PARlop = seq(0, 4, length = 139),
                     Housing = 95.29496, PopnHist = 3996.604, Vascular = 36.49683, 
                     SOLmau = 0.04316547, Slope = 13.8, PopnCurr = 2509.273)
P1 <- predict(M1, newdata = MyData, 
              type = "link", se = TRUE)
lines(x = MyData$PARlop, y = exp(P1$fit), col = "#332288", lwd = 2)
lines(x = MyData$PARlop,
      y = exp(P1$fit + 2 * P1$se.fit), col = "#332299", lty = 2)
lines(x = MyData$PARlop,
      y = exp(P1$fit - 2 * P1$se.fit), col = "#332299", lty = 2)
text(x = 2.75, y = 7, labels = paste0("Exponent =  0.702"), 
     pos = 4, col = "#332288", cex = 1)

# Slope Plot
plot(x = Env_Species$Slope, y = Env_Species$Richness, 
     xlab = "Slope of the Plot",
     ylab = "Weed richness", type = "n")
MyData <- data.frame(Slope = seq(0, 65, length = 139),
                     Housing = 95.29496, PopnHist = 3996.604, Vascular = 36.49683, 
                     SOLmau = 0.04316547, PARlop = 0.02877698, PopnCurr = 2509.273)
P1 <- predict(M1, newdata = MyData, 
              type = "link", se = TRUE)
lines(x = MyData$Slope, y = exp(P1$fit), col = "orange3", lwd = 2)
lines(x = MyData$Slope,
      y = exp(P1$fit + 2 * P1$se.fit), col = "orange", lty = 2)
lines(x = MyData$Slope,
      y = exp(P1$fit - 2 * P1$se.fit), col = "orange", lty = 2)
text(x = 50, y = 7.25, labels = paste0("Exponent =  1.016"), 
     pos = 4, col = "orange3", cex = 1)



# Visualizations with the raw data (not needed)

# Solanum mauritianum
ggplot(data = Env_Species, mapping = aes(x = SOLmau, y = Richness, colour = "#882265",  size=0.1)) + 
  geom_abline(colour = "#882265",  size=0.1) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Environmental Weed Seedling Richness") + xlab("Solanum mauritianum over 150 cm") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))    #If you want to remove background) +#If you want to remove the legend

# Linear regression
library(ggpmisc)

ggplot(data = Env_Species, aes(SOLmau, Richness)) +
  geom_jitter(color="#882265", size=0.4, alpha=0.9) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE) +
  geom_smooth(method = "lm", level = 0.95, color = "#882265",  fill = "#882240") +  # Add regression line
  labs(x = "Solanum mauritianum over 150 cm in the Plot", y = "Environmental Weed Seedling Richness") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))    #If you want to remove background) +#If you want to remove the legend

# Paraserianthes lophantha
ggplot(data = Env_Species, mapping = aes(x = PARlop, y = Richness, colour = "#882265",  size=0.1)) + 
  geom_abline(colour = "#882265",  size=0.1) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Environmental Weed Seedling Richness") + xlab("Paraserianthes lophantha over 150 cm") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))    #If you want to remove background) +#If you want to remove the legend

# Linear regression
library(ggpmisc)

ggplot(data = Env_Species, aes(PARlop, Richness)) +
  geom_jitter(color="#332288", size=0.4, alpha=0.9) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
               formula = y ~ x, parse = TRUE) +
  geom_smooth(method = "lm", level = 0.95, color = "#332288",  fill = "#332299") +  # Add regression line
  labs(x = "Paraserianthes lophantha over 150 cm in the Plot", y = "Environmental Weed Seedling Richness") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))    #If you want to remove background) +#If you want to remove the legend

# Housing
ggplot(data = Env_Species, aes(Housing, Richness)) +
  geom_jitter(color="royalblue", size=0.4, alpha=0.9) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE) +
  geom_smooth(method = "lm", level = 0.95, color = "royalblue", fill = "deepskyblue") +  # Add regression line
  labs(x = "No. of Houses withing 250 m of the site", y = "Environmental Weed Seedling Richness") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))    #If you want to remove background) +#If you want to remove the legend


# 96 Population
ggplot(data = Env_Species, aes(PopnHist, Richness)) +
  geom_jitter(color="#043927", size=0.4, alpha=0.9) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE) +
  geom_smooth(method = "lm", level = 0.95, color = "#043927", fill = "#50C878") +  # Add regression line
  labs(x = "Population of the area of the site in 1996", y = "Environmental Weed Seedling Richness") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))    #If you want to remove background) +#If you want to remove the legend

# Slope
ggplot(data = Env_Species, aes(Slope, Richness)) +
  geom_jitter(color="orange3", size=0.4, alpha=0.9) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE) +
  geom_smooth(method = "lm", level = 0.95, color = "orange3", fill = "orange") +  # Add regression line
  labs(x = "Slope of the Plot", y = "Environmental Weed Seedling Richness") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))    #If you want to remove background) +#If you want to remove the legend

# Vascular
ggplot(data = Env_Species, aes(Vascular, Richness)) +
  geom_jitter(color="pink4", size=0.4, alpha=0.9) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE) +
  geom_smooth(method = "lm", level = 0.95, color = "pink4", fill = "pink") +  # Add regression line
  labs(x = "Percentage cover of vascular vegetation of the Plot", y = "Environmental Weed Seedling Richness") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))    #If you want to remove background) +#If you want to remove the legend



# Three dimensional plot
library(plotly)
??plot_ly
fig <- plot_ly(df_Env_Species.pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~PC3, type = "scatter3d", mode = "markers", linetypes = NULL, repel = TRUE) %>%
  layout(title = "3D PCA Visualization")

fig

#### PCA with reduced factors ####
Env_Species.pca <- prcomp(PlotData_Combined[,c("Slope", "Vascular",
                          "Housing","PopnHist", "SOLmau", "PARlop")], center = TRUE,scale. = TRUE,tol = 0.1)
summary(Env_Species.pca)
Env_Species.pca


#this generates the PC scores for each plot
axes_Env_Species.pca <- predict(Env_Species.pca, newdata = Env_Species)
#making sure it worked
head(axes_Env_Species.pca, 4)

#creating a new dataframe that adds the the PC scores to the end
df_Env_Species.pca <- cbind(Env_Species, axes_Env_Species.pca)

fviz_eig(Env_Species.pca,addlabels = TRUE) #scree plot

eig.val <- get_eigenvalue(Env_Species.pca) #getting eighvalue from each pca
eig.val

pca.var <- get_pca_var(Env_Species.pca)
pca.var$contrib
pca.var$coord
pca.var$cos2


# % contribution of the variables 
fviz_pca_var(Env_Species.pca, axes = c(1, 3), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# GLM for richness and dimensions
library(MASS) ## do to the GLM
RichnessGLM <- glm.nb(Richness ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6,
                      data = df_Env_Species.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(RichnessGLM)


