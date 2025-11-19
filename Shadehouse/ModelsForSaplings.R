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

library(factoextra)

Sapling.pca <- prcomp(Sapling[,c("Group", "Room")], center = TRUE,scale. = TRUE,tol = 0.1)
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
MassGLM <- lm(Mass ~ PC1 + PC2,
                      data = df_Sapling.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(MassGLM)

#### AICc for model selection
library(MuMIn)
options(na.action = "na.fail") #Must run this code once to use dredge
model.full <- lm(Mass ~ factor(Group) + factor(Room), data = df_Sapling.pca)
summary(model.full)

# Look for Multicolliniarity
library(car)
car::vif(model.full)

# look at the effects
# Fit a model 
M2 <- lm(Mass ~ factor(Group) + factor(Room), data = Sapling)

summary(M2)
# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M2, type = 3) 

# Estimated marginal means and pairwise comparisons
library(emmeans)
emm <- emmeans(M2, ~ Group | Room)        # treatment effects within each room
pairs(emm, adjust = "tukey")

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

library(factoextra)

Sapling.pca <- prcomp(Sapling[,c("Group", "Room")], center = TRUE,scale. = TRUE,tol = 0.1)
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
RGRGLM <- lm(AverageGR ~ PC1 + PC2,
                      data = df_Sapling.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(RGRGLM)

#### AICc for model selection
library(MuMIn)
options(na.action = "na.fail") #Must run this code once to use dredge
model.full <- lm(AverageGR ~ factor(Group) + factor(Room), data = df_Sapling.pca)

# Look for Multicolliniarity
library(car)
car::vif(model.full)

# look at the effects
# Fit a model 
M1 <- lm(AverageGR ~  factor(Group) +  factor(Room), data = Sapling)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 

# Estimated marginal means and pairwise comparisons
library(emmeans)
emm <- emmeans(M2, ~ Group | Room)        # treatment effects within each room
pairs(emm, adjust = "tukey")

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

library(factoextra)

Sapling.pca <- prcomp(Sapling[,c("Group", "Room")], center = TRUE,scale. = TRUE,tol = 0.1)
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
RGRGLM <- lm(Survive ~ PC1 + PC2,
             data = df_Sapling.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(RGRGLM)

#### AICc for model selection
library(MuMIn)
options(na.action = "na.fail") #Must run this code once to use dredge
model.full <- glm(Survive ~ factor(Group) + factor(Room), family = binomial, data = df_Sapling.pca)

# Look for Multicolliniarity
library(car)
car::vif(model.full)

# look at the effects
# Fit a model 
M1 <- glm(Survive ~  factor(Group) +  factor(Room), family = binomial, data = Sapling)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 

# Estimated marginal means and pairwise comparisons
library(emmeans)
emm <- emmeans(M2, ~ Group | Room)        # treatment effects within each room
pairs(emm, adjust = "tukey")

#### Woolly Nightshade Biomass ####

# include only saplings
Woolly <- Biomass[Biomass$Plant == "Nightshade", ]

Woolly$Group <- as.numeric(Woolly$Group)


colnames(Woolly)[13:17] <- c("Nitrate","Ammonium", "Phosphorus", "Potassium",
                              "C_N") ## Renaming the columns

# Load required package
library(vegan)
library(dplyr)

library(factoextra)

Woolly.pca <- prcomp(Woolly[,c("Group", "Room")], center = TRUE,scale. = TRUE,tol = 0.1)
summary(Woolly.pca)
Woolly.pca


#this generates the PC scores for each plot
axes_Woolly.pca <- predict(Woolly.pca, newdata = Woolly)
#making sure it worked
head(axes_Woolly.pca, 4)

#creating a new dataframe that adds the the PC scores to the end
df_Woolly.pca <- cbind(Woolly, axes_Woolly.pca)

fviz_eig(Woolly.pca,addlabels = TRUE) #scree plot

eig.val <- get_eigenvalue(Woolly.pca) #getting eighvalue from each pca
eig.val

pca.var <- get_pca_var(Woolly.pca)
pca.var$contrib
pca.var$coord
pca.var$cos2


# % contribution of the variables 
fviz_pca_var(Woolly.pca, axes = c(1, 2), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


library(MASS) ## do to the GLM
MassGLM <- lm(Mass ~ PC1 + PC2,
              data = df_Woolly.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(MassGLM)

#### AICc for model selection
library(MuMIn)
options(na.action = "na.fail") #Must run this code once to use dredge
model.full <- lm(Mass ~ factor(Group) + factor(Room), data = df_Woolly.pca)
summary(model.full)

# Look for Multicolliniarity
library(car)
car::vif(model.full)

# look at the effects
# Fit a model 
M2 <- lm(Mass ~ factor(Group) + factor(Room), data = Woolly)

summary(M2)
# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M2, type = 3) 

# Estimated marginal means and pairwise comparisons
library(emmeans)
emm <- emmeans(M2, ~ Group | Room)        # treatment effects within each room
pairs(emm, adjust = "tukey")

#### Woolly Nightshade RGR ####
WoollyH <- Height[Height$Plant == "Nightshade", ]

Woolly <- Woolly %>%
  left_join(dplyr::select(WoollyH, Pot, AverageGR), by = "Pot")

Woolly <- Woolly %>% 
  filter(!is.na(AverageGR))


# Load required package
library(vegan)
library(dplyr)

library(factoextra)

Woolly.pca <- prcomp(Woolly[,c("Group", "Room")], center = TRUE,scale. = TRUE,tol = 0.1)
summary(Woolly.pca)
Woolly.pca


#this generates the PC scores for each plot
axes_Woolly.pca <- predict(Woolly.pca, newdata = Woolly)
#making sure it worked
head(axes_Woolly.pca, 4)

#creating a new dataframe that adds the the PC scores to the end
df_Woolly.pca <- cbind(Woolly, axes_Woolly.pca)

fviz_eig(Woolly.pca,addlabels = TRUE) #scree plot

eig.val <- get_eigenvalue(Woolly.pca) #getting eighvalue from each pca
eig.val

pca.var <- get_pca_var(Woolly.pca)
pca.var$contrib
pca.var$coord
pca.var$cos2


# % contribution of the variables 
fviz_pca_var(Woolly.pca, axes = c(1, 2), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


library(MASS) ## do to the GLM
RGRGLM <- lm(AverageGR ~ PC1 + PC2,
             data = df_Woolly.pca) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(RGRGLM)

#### AICc for model selection
library(MuMIn)
options(na.action = "na.fail") #Must run this code once to use dredge
model.full <- lm(AverageGR ~ factor(Group) + factor(Room), data = df_Woolly.pca)

# Look for Multicolliniarity
library(car)
car::vif(model.full)

# look at the effects
# Fit a model 
M1 <- lm(AverageGR ~  factor(Group) +  factor(Room), data = Woolly)

summary(M1)

# Type II/III tests (handle unbalanced designs)
library(car)
Anova(M1, type = 3) 

# Estimated marginal means and pairwise comparisons
library(emmeans)
emm <- emmeans(M2, ~ Group | Room)        # treatment effects within each room
pairs(emm, adjust = "tukey")
