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
TallWeeds <- SurveyData_Combined %>%
  mutate(SOLmau = ifelse(ScientificName == "Solanum mauritianum", Tier_4, 0))

# For privet
TallWeeds <- TallWeeds %>%
  mutate(LIGluc = ifelse(ScientificName == "Ligustrum lucidum", Tier_4, 0))

# For wattle
TallWeeds <- TallWeeds %>%
  mutate(PARlop = ifelse(ScientificName == "Paraserianthes lophantha", Tier_4, 0))

# Remove all other columns
library(dplyr)
TallWeeds <- TallWeeds %>%
  select(Plot, SOLmau, LIGluc, PARlop)

# Only include rows with unique values for Plot
TallWeeds <- TallWeeds %>%
  distinct(Plot, .keep_all = TRUE)

# subset to just look at the weeds
# Subset the data to only include species that are weeds using the WeedList Column
SurveyData_Combined <- SurveyData_Combined[SurveyData_Combined$WeedList == 1, ]

# I need to make this data only include seedlings. Therefore, I need to remove
# the rows that only include species > 51 cm.

SurveyData_Combined$seedlings <- SurveyData_Combined$Tier_1 - SurveyData_Combined$Tier_3

# Remove any rows for which seedlings is empty so that richness is only calculated 
# From seedlings
# Assuming your data frame is named 'df'
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

# Now combine this into unique numerical plot names
library(tidyr)
PlotData_Combined <- PlotData %>%
  unite(Plot, Site, Plot, sep = "-")

# Unite Plot and Weed vs Native columns so that now the plot id is all in one
# column that includes the site, plot number, and whether it is weed or native
library(tidyr)
PlotData_Combined <- PlotData_Combined %>%
  unite(Plot, Plot, Weed_Native, sep = "-")

PlotData_Combined <- left_join(TallWeeds, PlotData_Combined, by = "Plot")

# Now I need to only include the environmental variables that I want to include
# for the GLM

PlotData_Combined <- subset(PlotData_Combined, select = -c(Date, CentralSpecies, CanopyCover_App, Waypoint, CoverGrass,
                                                           Topography, ParentMaterial, Notes))

# Need to simplify the column names 
colnames(PlotData_Combined)[5:23] <- c("Housing","PopnHist", "PopnCurr", "DwellingsCurr",
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
                                               "Pests", "Litter", "Housing","PopnHist", "PopnCurr", "DwellingsCurr", "WN", "SOLmau", "LIGluc", "PARlop")], center = TRUE,scale. = TRUE,tol = 0.1)
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
fviz_pca_var(Env_Species.pca, axes = c(1, 4), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


library(MASS) ## do to the GLM
RichnessGLM <- glm.nb(Richness ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                        PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + 
                        PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19,
                      data = df_Env_Species.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(RichnessGLM)


#### AIC for model selection ####

library(MuMIn)
options(na.action = "na.fail") #Must run this code once to use dredge
model.full <- lm(Richness ~ Height + DBH + Slope + Canopy + Vascular + NonVascular+
                   LitterCover + Bare + Debris + Erosion + Disturbance + Pests +
                   Litter + Housing + PopnHist + PopnCurr + DwellingsCurr + WN + SOLmau + LIGluc + PARlop, data = df_Env_Species.pca)

# Richness GLM
RichnessGLM2 <- glm.nb(Richness ~ Height + DBH + Slope + Canopy + Vascular + NonVascular+
                         LitterCover + Bare + Debris + Erosion + Disturbance + Pests +
                         Litter + Housing + PopnHist + PopnCurr + DwellingsCurr + WN + SOLmau + LIGluc + PARlop, data = df_Env_Species.pca)

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
model.full <- lm(Richness ~ Height + DBH + Slope + Canopy + NonVascular + Vascular +
                  Debris + Erosion + Disturbance + Pests +
                   Litter + Housing + PopnHist + PopnCurr + WN + SOLmau + LIGluc + PARlop, data = df_Env_Species.pca)

# Look for Multicolliniarity
library(car)
vif(model.full)

# look at how they correlate
data <- df_Env_Species.pca[ , c("Height", "DBH", "Slope", "Canopy", "NonVascular", 
                                "LitterCover", "Debris", "Erosion", "Disturbance", 
                                 "Pests", "Litter", "Housing", "PopnHist", "PopnCurr", "WN")]
cor(data)


# Run all iterations of the model
dredge <- dredge(model.full, rank = "AIC", extra = c("R^2", adjRsq = function(x) summary(x)$adj.r.squared))

head(dredge, 10)

# Install and load the writexl package
install.packages("writexl")
library(writexl)

write_xlsx(dredge, "C:/Users/bella/Documents/dredgeresults.xlsx")

# This gives the simplest possible model which includes Housing Density, 96 Population, 
# LitterCover, PARlop, SOLmau, and Slope

# Compare factors to how they relate to richness
library(MASS) ## do to the GLM
RichnessGLM <- glm.nb(Richness ~ Housing +
                        PopnHist + Vascular +
                        SOLmau + PARlop + Slope,
                      data = df_Env_Species.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(RichnessGLM)


# exponentiating the estimates
exp(coef(RichnessGLM))

exp(confint(RichnessGLM))

# Make some visualizations with the significant ones
# Housing, PopnHist, SOLmau, and Slope

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


