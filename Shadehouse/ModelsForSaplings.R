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
  left_join(Nodules %>% select(Pot, Nodule_start), by = "Pot") %>%
  mutate(Nodule_start = replace_na(Nodule_start, 0))

Biomass <- Biomass %>%
  left_join(Nodules %>% select(Pot, Nodule_finish), by = "Pot") %>%
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


#### Sapling Model ####

# include only saplings
Sapling <- Biomass[Biomass$Plant == "ManukaSapling", ]

Sapling$Group <- as.numeric(Sapling$Group)


colnames(Sapling)[13:17] <- c("Nitrate","Ammonium", "Phosphorus", "Potassium",
                                       "C_N") ## Renaming the columns

# Load required package
library(vegan)
library(dplyr)

library(factoextra)

Sapling.pca <- prcomp(Sapling[,c("Group", "Nodule_start", "Nodule_finish", 
                                  "Nitrate", "Ammonium", "Phosphorus", 
                                  "Potassium", "C_N", "Woolly", "Wattle", "Privet")], center = TRUE,scale. = TRUE,tol = 0.1)
summary(Sapling.pca)
Sapling.pca


#this generates the PC scores for each plot
axes_Sapling.pca <- predict(Sapling.pca, newdata = Sapling)
#making sure it worked
head(axes_Sapling.pca, 4)

#creating a new dataframe that adds the the PC scores to the end
df_Sapling.pca <- cbind(Sapling, axes_Sapling.pca)

fviz_eig(Sapling.pca,addlabels = TRUE) #scree plot

eig.val <- get_eigenvalue(Sapling.pca) #getting eighvalue from each pca
eig.val

pca.var <- get_pca_var(Sapling.pca)
pca.var$contrib
pca.var$coord
pca.var$cos2


# % contribution of the variables 
fviz_pca_var(Sapling.pca, axes = c(1, 2), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


library(MASS) ## do to the GLM
RichnessGLM <- glm.nb(Mass ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                        PC7 + PC8 +PC9,
                      data = df_Sapling.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(RichnessGLM)

#### AICc for model selection
library(MuMIn)
options(na.action = "na.fail") #Must run this code once to use dredge
model.full <- lm(Mass ~ Nodule_start + Nodule_finish + Nitrate + Ammonium + Phosphorus+
                   Potassium + C_N + Woolly, data = df_Sapling.pca)

# Mass GLM
MassGLM2 <- glm.nb(Mass ~ Nodule_start + Nodule_finish + Nitrate + Ammonium + Phosphorus+
                         Potassium + C_N, data = df_Sapling.pca)

summary(MassGLM2)

# Look for Multicolliniarity
library(car)
car::vif(model.full)

alias(model.full)
# Output shows that Vascular, LitterCover, Bar, PopnCurr, and DwellingsCurr all
# have values greater than 5, so these should be omitted

# Creates a visualization
vif_values <- car::vif(model.full)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)

# look at how they correlate
data <- df_Sapling.pca[ , c("Phosphorus", "Nitrate", "Ammonium")]
cor(data)

# Let's remove Phosphorus because it makes sense that it would be opposite to Ns
model.full <- lm(Mass ~ Group + Nodule_start + Nodule_finish + Nitrate + Ammonium +
                   Potassium + C_N + Woolly, data = df_Sapling.pca)

# Look for Multicolliniarity
car::vif(model.full)

# All are below 5 now, acceptable
# Run all iterations of the model
dredge <- dredge(model.full, rank = "AICc", extra = c("R^2", adjRsq = function(x) summary(x)$adj.r.squared))

head(dredge, 10)

# look at the effects
# Fit a model 
M1 <- glm(Mass ~  Nodule_finish +  Potassium,
          family = "poisson", data = Sapling)

summary(M1)
library(effects)

eff <- allEffects(M1)
summary(eff)
plot(eff)

(exp(coef(M1)["Nodule_finish"])- 1)*100
(exp(coef(M1)["Potassium"])- 1)*100

# Nodule effects plot
plot(Effect("Nodule_finish", M1), xlab = "Nodules at the end of the experiment",
     ylab = "Mānuka mass (g)", main = "", colors = "royalblue", cex.lab = 2)

# Potassium effects
plot(Effect("Potassium", M1), xlab = "Potassium in the media at the end of the experiment",
     ylab = "Mānuka mass (g)", main = "", colors = "pink3")
