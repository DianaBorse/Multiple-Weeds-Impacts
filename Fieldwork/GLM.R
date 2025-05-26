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

# Need to subset to only include the environmental weeds
# Subset the data to only include species that are weeds using the Status column
SurveyData_Combined_subset <- SurveyData_Combined[SurveyData_Combined$Status == 1, ]

# Calculate richness value
library(dplyr)
RichnessWeed <- SurveyData_Combined_subset %>% group_by(Plot) %>% summarize(unique_values = n_distinct(ScientificName))

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


# I want to look at species richness rather than composition, so that means that
# I need to make it presence/absence
PresenceAbsence <-SurveyData_Combined_subset %>%
  pivot_wider(id_cols = Plot, names_from=ScientificName, values_from=ScientificName,
              values_fn=function(x) any(unique(x) == x) * 1, values_fill = 0)

# instead of being a tibble, I wanted to convert it back to a data frame
PresenceAbsence_df = as.data.frame(PresenceAbsence)

library(dplyr)
Env_Species <- left_join(PresenceAbsence, PlotData_Combined, by = "Plot")  # Preserves all rows from df1

# Include Richness
Env_Species <- left_join(RichnessWeed, PlotData_Combined, by = "Plot")  # Preserves all rows from df1

# instead of being a tibble, I wanted to convert it back to a data frame
Env_Species = as.data.frame(Env_Species)


# Needs to remove the first column of numbers as row names and make the Scientific 
# names of species into the row names
row.names(Env_Species) <- Env_Species$Plot 
# Remove the first column from the data frame 
Env_Species <- Env_Species[, -1]

# Needs to remove the first column of numbers as row names and make the Scientific 
# names of species into the row names
row.names(PresenceAbsence_df) <- PresenceAbsence_df$Plot 
# Remove the first column from the data frame 
PresenceAbsence_df <- PresenceAbsence_df[, -1]


# Back to the Survey data
#Quick checks for empty rows or columns...
rowSums(PresenceAbsence_df)
colSums(PresenceAbsence_df)


#### PCA ####
library(factoextra)

Env_Species.pca <- prcomp(Env_Species[,c("Height", "DBH",
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

# Run all iterations of the model
dredge <- dredge(model.full, rank = "AIC", extra = c("R^2", adjRsq = function(x) summary(x)$adj.r.squared))

# might be too many, will try with fewer
library(MuMIn)
options(na.action = "na.fail") #Must run this code once to use dredge
model.full <- lm(Richness ~ Height + DBH + Slope + Canopy + Vascular + Disturbance + Pests +
                   Litter + Housing + PopnHist + PopnCurr + DwellingsCurr + WN, data = df_Env_Species.pca)

# Run all iterations of the model
dredge <- dredge(model.full, rank = "AIC", extra = c("R^2", adjRsq = function(x) summary(x)$adj.r.squared))

head(dredge, 4)

#### PCA with selected factors ####

Env_Species <- subset(Env_Species, select = -c(East, South, NonVascular, DBH,
                                                           Pests, Slope, Height))


library(factoextra)

Env_Species.pca <- prcomp(Env_Species[,c("Housing", "Height",
                       "PopnHist", "Slope", "Vascular")], center = TRUE,scale. = TRUE,tol = 0.1)

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