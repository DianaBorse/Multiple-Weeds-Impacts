#### GLM ####

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

# subset to just look at the weeds
# Subset the data to only include species that are weeds using the WeedList Column
SurveyData_Combined <- SurveyData_Combined[SurveyData_Combined$WeedList == 1, ]

# I need to make this data only include seedlings. Therefore, I need to remove
# the rows that only include species > 51 cm.

SurveyData_Combined$seedlings <- SurveyData_Combined$Tier_1 - SurveyData_Combined$Tier_3

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

# Now I need to only include the environmental variables that I want to include
# for the GLM

PlotData_Combined <- subset(PlotData_Combined, select = -c(Date, CanopyCover_App, Waypoint, CoverGrass,
                                                           Topography, ParentMaterial, Notes))

# Need to simplify the column names 
colnames(PlotData_Combined)[3:21] <- c("Housing","PopnHist", "PopnCurr", "DwellingsCurr",
                                       "Canopy", "Height", "DBH",
                                       "Slope", "Erosion", "Disturbance",
                                       "Pests", "Litter", "East", "South", 
                                       "Vascular", "NonVascular", "LitterCover",
                                       "Bare", "Debris") ## Renaming the columns


# Include Richness
PlotData_Combined <- left_join(RichnessWeed, PlotData_Combined, by = "Plot")  # Preserves all rows from df1

SurveyData_Combined$seedlings_sqrt <- sqrt(SurveyData_Combined$seedlings)


# Filter down to only native species
# Remove native species plots
# Remove rows where Group is "Native"

SurveyData_Combined_Weeds <- SurveyData_Combined %>% 
  filter(CentralSpecies != "Native")

# This code gives 0 values when species are not present in a plot
Survey_wide <- SurveyData_Combined_Weeds %>%
  pivot_wider(names_from = ScientificName, 
              values_from = seedlings_sqrt, 
              id_cols = Plot) %>%
  mutate_all(~ replace(., is.na(.), 0))

# instead of being a tibble, I wanted to convert it back to a data frame
Survey_wide = as.data.frame(Survey_wide)

# Remove native species from plot data
# Load necessary library
library(dplyr)

# Assuming your data frame is named 'df'
PlotData_Combined <- PlotData_Combined %>% 
  filter(CentralSpecies %in% c("Solanum mauritianum", "Ligustrum lucidum", "Paraserianthes lophantha"))

# Left Join
Env_Species <- Survey_wide %>% 
  left_join(PlotData_Combined, by = "Plot")

# Needs to remove the first column of numbers as row names and make the Scientific 
# names of species into the row names
#row.names(Env_Species) <- Env_Species$Plot 
# Remove the first column from the data frame 
#Env_Species <- Env_Species[, -1]

#Quick checks for empty rows or columns...
rowSums(Env_Species)
colSums(Env_Species)

# Remove empty columns
# Survey_wide <- Survey_wide[, colSums(Survey_wide) != 0]
# Survey_wide <- Survey_wide[rowSums(Survey_wide) != 0, ]

#rowSums(Survey_wide)
#colSums(Survey_wide)

# Try removing outlier
# Survey_wide<-Survey_wide[-68, ] # this gets rid of the the row that looks to be an outlier
# Survey_wide<-Survey_wide[-1, ] # this gets rid of the the row that looks to be an outlier

# Remove plots that were removed from the survey
Env_Species<-Env_Species[-75, ] # this gets rid of the the outlier
Env_Species<-Env_Species[-68, ] # this gets rid of the the outlier
Env_Species<-Env_Species[-67, ] # this gets rid of the the outlier
Env_Species<-Env_Species[-62, ] # this gets rid of the the outlier
Env_Species<-Env_Species[-53, ] # this gets rid of the the outlier
Env_Species<-Env_Species[-24, ] # this gets rid of the the outlier
Env_Species<-Env_Species[-15, ] # this gets rid of the the outlier
Env_Species<-Env_Species[-12, ] # this gets rid of the the outlier
Env_Species<-Env_Species[-1, ] # this gets rid of the the outlier

# Needs to remove the first column of numbers as row names and make the Scientific 
# names of species into the row names
row.names(Env_Species) <- Env_Species$Plot 
# Remove the first column from the data frame 
Env_Species <- Env_Species[, -1]


# instead of being a tibble, I wanted to convert it back to a data frame
Env_Species = as.data.frame(Env_Species)


#### PCA ####
library(factoextra)

Env_Species.pca <- prcomp(PlotData_Combined[,c("Height", "DBH",
                       "Slope", "Canopy", "East", "South", "Vascular", "NonVascular", "LitterCover",
                       "Bare", "Debris", "Erosion", "Disturbance",
                       "Pests", "Litter", "Housing","PopnHist", "PopnCurr", "DwellingsCurr", "WN")], center = TRUE,scale. = TRUE,tol = 0.1)

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

pca.var<- get_pca_var(Env_Species.pca)
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
                        PC13 + PC14 + PC15 + PC16 + PC17 + PC18,
                      data = df_Env_Species.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(RichnessGLM)

#### AIC for model selection ####

library(MuMIn)
options(na.action = "na.fail") #Must run this code once to use dredge
model.full <- lm(Richness ~ Height + DBH + Slope + Canopy + Vascular + NonVascular+
                   LitterCover + Bare + Debris + Erosion + Disturbance + Pests +
                   Litter + Housing + PopnHist + PopnCurr + DwellingsCurr + WN, data = df_Env_Species.pca)

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

# vascular and litter cover are highly correlated. Vascular is a less significant 
# factor, so I will keep litter cover and remove vascular
# Current population and the current dwellings are highly correlated. I will remove 
# current dwellings because this is somewhat covered by housing density within 250 m
# and current population is more comparable to 96 population
# Bare soil is highly correlated with several other factors, so I will remove it 
# as well

# Make a new model with at least one of the correlated factors omitted 
model.full <- lm(Richness ~ Height + DBH + Slope + Canopy + NonVascular + LitterCover +
                    Debris + Erosion + Disturbance + Pests +
                   Litter + Housing + PopnHist + PopnCurr + WN, data = df_Env_Species.pca)


# Run all iterations of the model
dredge <- dredge(model.full, rank = "AIC", extra = c("R^2", adjRsq = function(x) summary(x)$adj.r.squared))

head(dredge, 10)

#### PCA with selected factors ####

library(factoextra)

Env_Species.pca <- prcomp(Env_Species[,c("Housing", "LitterCover",
                       "PopnHist", "Erosion")], center = TRUE,scale. = TRUE,tol = 0.1)

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

pca.var<- get_pca_var(Env_Species.pca)
pca.var$contrib
pca.var$coord
pca.var$cos2


# % contribution of the variables 
fviz_pca_var(Env_Species.pca,  axes = c(1, 2), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = " ")

??fviz_pca_var
# Need to compare the dimensions to how they relate to richness
library(MASS) ## do to the GLM
RichnessGLM <- glm.nb(Richness ~ PC1 + PC2 +
                       PC3 + PC4 +
                       PC5 + PC6 +
                       PC7 + PC8 + PC9 + PC10,
                     data = df_Env_Species.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(RichnessGLM)

# Can see that PC1 and PC3 are the ones that relate significantly to weed species richness
# Both are negatively associated with weed species richness, so anything negatively
# associated with either PC1 or PC3 is positively associated with weed species richness
# and vice-versa

# Let's have a look at those values again with this in mind
summary(Env_Species.pca)
Env_Species.pca

# Compare factors to how they relate to richness
library(MASS) ## do to the GLM
RichnessGLM <- glm.nb(Richness ~ Housing + Canopy +
                        Erosion + Disturbance +
                        Litter + LitterCover +
                        Bare + Debris + Vascular + WN,
                      data = df_Env_Species.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(RichnessGLM)

# Narrow to only the values over 0.5 on the PCA
RichnessGLM <- glm.nb(Richness ~ Housing + Vascular +
                        LitterCover,
                      data = df_Env_Species.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(RichnessGLM)

# Housing density and litter cover are the only significant factors influencing 
# weed species richness

# Three dimensional plot
library(plotly)
??plot_ly
fig <- plot_ly(df_Env_Species.pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~PC3, type = "scatter3d", mode = "markers", linetypes = NULL, repel = TRUE) %>%
  layout(title = "3D PCA Visualization")

fig