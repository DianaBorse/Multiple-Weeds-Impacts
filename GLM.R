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
SurveyData <- read_csv("SurveyData_Clean_WN_removed.csv")

#library(readr)
#PlotData <- read_csv("PlotData_Clean.csv")

library(readr)
PlotData <- read_csv("PlotData_Clean_WN_removed.csv")

# The species that were entered as percent-cover need to be accounted for
# Given that the number of plants will vary greatly, but cover plants were all
# in Tier_1, for each percent cover plant I will give a value of 

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

# Need to subset to only include the environmental weeds
# Subset the data to only include species that are weeds using the Status column
SurveyData_Combined_subset <- SurveyData_Combined[SurveyData_Combined$Status == 1, ]

# Repeat for plot data
# Assigning the Site a unique numeric value
PlotData$Site <- as.numeric(as.factor(PlotData$Site))
# Assigning W and N unique numeric values (weed is now 2, native is now 1)
PlotData$Weed_Native <- as.numeric(as.factor(PlotData$Weed_Native))

# Now combine this into unique numerical plot names
library(tidyr)
PlotData_Combined <- PlotData %>%
  unite(Plot, Site, Plot, sep = "-")

# Unite Plot and Weed vs Native columns so that now the plot id is all in one
# column that includes the site, plot number, and whether it is weed or native
library(tidyr)
PlotData_Combined <- PlotData_Combined %>%
  unite(Plot, Plot, Weed_Native, sep = "-")

# Filter down to only native species
# Remove native species plots
# Remove rows where Group is not one of the three weeds
PlotData_Weeds <- PlotData_Combined %>% 
  filter(CentralSpecies %in% c("Solanum mauritianum", "Ligustrum lucidum", "Paraserianthes lophantha"))

# Now I need to only include the environmental variables that I want to include
# for the GLM

PlotData_Weeds <- PlotData_Weeds %>%
  select(-Date, -CanopyCover_App, -Waypoint, -East_Coordinates, -South_Coordinates, -CoverVascular,
         -CoverNonVascular, -CoverLitter, -CoverBareSoil, -CoverDebris, -CoverGrass,
         -Topography, -ParentMaterial, -Notes)

# Need to simplify the column names 
colnames(PlotData_Weeds)[3:10] <- c("Height", "DBH",
                                  "Slope", "Canopy",
                                  "Erosion", "Disturbance",
                                  "Pests", "Litter") ## Renaming the columns

# Back to the survey data setting up for nMDS

# Need to transform the data square root was given as a good transformation, so I will do that
SurveyData_Combined$Tier_1_sqrt <- sqrt(SurveyData_Combined$Tier_1)
# repeat for weeds only data
SurveyData_Combined_subset$Tier_1_sqrt <- sqrt(SurveyData_Combined_subset$Tier_1)

# Remove rows where Group is "Native"
SurveyData_Combined_Weeds <- SurveyData_Combined %>% 
 filter(CentralSpecies != "Native")

# Repeat for subset data
SurveyData_Combined_subset_weeds <- SurveyData_Combined_subset %>% 
  filter(CentralSpecies != "Native")
 

# This code gives 0 values when species are not present in a plot
Survey_wide <- SurveyData_Combined_Weeds %>%
  pivot_wider(names_from = ScientificName, 
              values_from = Tier_1_sqrt, 
              id_cols = Plot) %>%
  mutate_all(~ replace(., is.na(.), 0))
# repeat for environmental weeds subset
Survey_wide_subset <- SurveyData_Combined_subset_weeds %>%
  pivot_wider(names_from = ScientificName, 
              values_from = Tier_1_sqrt, 
              id_cols = Plot) %>%
  mutate_all(~ replace(., is.na(.), 0))



# instead of being a tibble, I wanted to convert it back to a data frame
Survey_wide = as.data.frame(Survey_wide)
#repeat for environmental weeds subset
Survey_wide_subset = as.data.frame(Survey_wide_subset)

library(dplyr)
Env_Species <- left_join(Survey_wide, PlotData_Weeds, by = "Plot")  # Preserves all rows from df1

# repeat for subset data
Env_Species_subset <- left_join(Survey_wide_subset, PlotData_Weeds, by = "Plot")

# Needs to remove the first column of numbers as row names and make the Scientific 
# names of species into the row names
row.names(Survey_wide) <- Survey_wide$Plot 
# Remove the first column from the data frame 
Survey_wide <- Survey_wide[, -1]

# repeat for subset data
row.names(Survey_wide_subset) <- Survey_wide_subset$Plot 
# Remove the first column from the data frame 
Survey_wide_subset <- Survey_wide_subset[, -1]


# Back to the Survey data
#Quick checks for empty rows or columns...
rowSums(Survey_wide)
colSums(Survey_wide)

# These are the columns that need to be removed using the data with the extra 
# woolly nightshades removed
Survey_wide<-Survey_wide[,-115] # this gets rid of the the column where there is a zero
Survey_wide<-Survey_wide[,-40] # this gets rid of the the column where there is a zero
Survey_wide<-Survey_wide[,-39] # this gets rid of the the column where there is a zero

# Repeat for subset data
#Quick checks for empty rows or columns...
rowSums(Survey_wide_subset)
colSums(Survey_wide_subset)

Survey_wide_subset<-Survey_wide_subset[,-66] # this gets rid of the the column where there is a zero
Survey_wide_subset<-Survey_wide_subset[,-65] # this gets rid of the the column where there is a zero



### NMDS ####

library(vegan)

doubs.dist<-vegdist(Survey_wide)
doubs.dist

# Repeat for subset data
doubs.dist.subset<-vegdist(Survey_wide_subset)
doubs.dist.subset

# Check for Na, NaN,Inf values
any(is.na(doubs.dist))
any(is.infinite(doubs.dist))

# Check for Na, NaN,Inf values in subset data
any(is.na(doubs.dist.subset))
any(is.infinite(doubs.dist.subset))


#Classification
Survey_wide<-hclust(doubs.dist,method='average')
plot(Survey_wide,hang=-1) #The hang=-1 tidies it up so all the end nodes finish at the same level
grp<-cutree(Survey_wide,k=30) #K=4 is saying to identify the dominant 4 groups.
grp
rect.hclust(Survey_wide, k=30)

#Classification for subset data
Survey_wide_subset<-hclust(doubs.dist.subset,method='average')
plot(Survey_wide_subset,hang=-1) #The hang=-1 tidies it up so all the end nodes finish at the same level
grp<-cutree(Survey_wide_subset,k=30) #K=4 is saying to identify the dominant 4 groups.
grp
rect.hclust(Survey_wide_subset, k=30)


#Conduct a PCA on continuous data, commonly used for environmental 
#variable reduction. Say we want to combine variables that are similar in 
#for SEM we may use PCA

# linkage - once you have the two most similar sites together, how do you think about the similarity to the other sites. Best method is UPPGMA (an average weighed method)

# Now doing a PCA on the data
# Can do PCA on vegan or just the base programme of R, they just use different commands 
# (see powerpoint handout for more info)

doubs.pca<-princomp(doubs.dist,cor=TRUE)
summary(doubs.pca) 
biplot(doubs.pca)

# repeat for subset data
doubs.pca.subset<-princomp(doubs.dist.subset,cor=TRUE)
summary(doubs.pca.subset) 
biplot(doubs.pca.subset)

# Create PCA biplot with sites as points

# Load necessary libraries
library(ggplot2)
library(ggfortify)

autoplot(
  doubs.pca,
  data = doubs.dist,
  loadings = TRUE,             # Display loadings vectors
  loadings.label = TRUE,       # Display loadings labels
  loadings.colour = '#AA4499',     # Loadings vector color
  loadings.label.size = 3,     # Loadings label size
  loadings.label.colour = "#AA4499", # Loadings label color
  shape = 16) +                   # Use points for sites
  theme_minimal()              # Apply a minimal theme


autoplot(
  doubs.pca.subset,
  data = doubs.dist.subset,
  loadings = TRUE,             # Display loadings vectors
  loadings.label = TRUE,       # Display loadings labels
  loadings.colour = '#AA4499',     # Loadings vector color
  loadings.label.size = 3,     # Loadings label size
  loadings.label.colour = "#AA4499", # Loadings label color
  shape = 16) +                   # Use points for sites
  theme_minimal()              # Apply a minimal theme



#Moving on now to MDS
#DO MDS with vegan package
z <- metaMDS(comm = doubs.dist,
             autotransform = FALSE,
             distance = "bray",
             engine = "monoMDS",
             k = 5,
             weakties = TRUE,
             model = "global",
             maxit = 300,
             try = 40,
             trymax = 50)

z
#Moving on now to MDS for subset data
#DO MDS with vegan package
z.subset <- metaMDS(comm = doubs.dist.subset,
             autotransform = FALSE,
             distance = "bray",
             engine = "monoMDS",
             k = 3,
             weakties = TRUE,
             model = "global",
             maxit = 300,
             try = 40,
             trymax = 50)

z.subset

# Stress Plot = Sheppard Plot
plot(z$diss, z$dist)

stressplot(object = z,
           p.col = "#88CCEE",
           l.col = "#882255",
           lwd = 1)

# repeat for subset data
plot(z.subset$diss, z.subset$dist)

stressplot(object = z.subset,
           p.col = "#88CCEE",
           l.col = "#882255",
           lwd = 1)

#always report the stress for MDS, never report the Rsquared you get from that plot above
plot(z)

plot(z[["points"]][,2]~z[["points"]][,1],main="Survey Data", xlab="NMDSaxis 1" , ylab= "NMDS axis 2", cex = 0.5 +as.numeric(Survey_wide$nit))

# Using ggplot2 to make a plot with site names
plot(z, type = "t")

# Looking at the stress/goodness of fit
gof <- goodness(object = z)
plot(z, display = "sites", type = "none")
points(z, display = "sites", cex = 2*gof/mean(gof))


# Looking at the stress/goodness of fit for subset data
gof <- goodness(object = z.subset)
plot(z.subset, display = "sites", type = "none")
points(z.subset, display = "sites", cex = 2*gof/mean(gof))


# Make the points into a data frame for ggplot
z$points %>% head()
z.points <- data.frame(z$points)


# Make the points into a data frame for ggplot
z.subset$points %>% head()
z.subset.points <- data.frame(z.subset$points)

# I need to make a new data frame that is just the plots and the Central species
# for that plot
# Load the dplyr package
library(dplyr)

# Create a new data frame with unique Plot values
Plot_Species <- SurveyData_Combined_Weeds %>% distinct(Plot, .keep_all = TRUE)

grp <- Plot_Species$CentralSpecies

# Create a new data frame with unique Plot values
Plot_Species_subset <- SurveyData_Combined_subset_weeds %>% distinct(Plot, .keep_all = TRUE)

grp.subset <- Plot_Species_subset$CentralSpecies

# Create a data frame of NMDS results with group information
z.points$Group <- Plot_Species$CentralSpecies # Replace with your group column

# repeat for subset data
z.subset.points$Group <- Plot_Species_subset$CentralSpecies # Replace with your group column



# Customize plot with ggplot2
nmds_plot <- ggplot(data = z.points, aes(x = MDS1, y = MDS2, shape = Group, color = Group)) +
  geom_point(size = 2) + # Set point size
  scale_shape_manual(values = c(16, 15, 18, 19)) + # Customize shapes
  scale_color_manual(values = c("#EE6677", "#228833", "#44AA99")) + # Customize colors
  theme_minimal() +
  labs(x = "NMDS1", y = "NMDS2") +
  theme_classic()

# Print the plot
print(nmds_plot)

# Customize plot with ggplot2
nmds_plot <- ggplot(data = z.subset.points, aes(x = MDS1, y = MDS2, shape = Group, color = Group)) +
  geom_point(size = 2) + # Set point size
  scale_shape_manual(values = c(16, 15, 18, 19)) + # Customize shapes
  scale_color_manual(values = c("#EE6677", "#228833", "#44AA99")) + # Customize colors
  theme_minimal() +
  labs(x = "NMDS1", y = "NMDS2") +
  theme_classic()

# Print the plot
print(nmds_plot)

# Customize plot with ggplot2 add ellipses
nmds_plot <- ggplot(data = z.points, aes(x = MDS2, y = MDS4, shape = Group, color = Group)) +
  geom_point(size = 2) + # Set point size
  scale_shape_manual(values = c(16, 15, 17)) + # Customize shapes
  scale_color_manual(values = c("#EE6677", "#228833", "#44AA99")) + # Customize colors
  stat_ellipse(aes(group = Group, fill = Group), geom = "polygon", alpha = 0.1) +
  scale_fill_manual(values = c("#EE6677",  "#228833", "#44AA99")) +
  theme_minimal() +
  labs(x = "NMDS2", y = "NMDS4") +
  theme_classic()

# Print the plot
print(nmds_plot)

# Customize plot with ggplot2 add ellipses for subset data
nmds_plot <- ggplot(data = z.subset.points, aes(x = MDS1, y = MDS3, shape = Group, color = Group)) +
  geom_point(size = 2) + # Set point size
  scale_shape_manual(values = c(16, 15, 17)) + # Customize shapes
  scale_color_manual(values = c("#EE6677", "#228833", "#44AA99")) + # Customize colors
  stat_ellipse(aes(group = Group, fill = Group), geom = "polygon", alpha = 0.1) +
  scale_fill_manual(values = c("#EE6677",  "#228833", "#44AA99")) +
  theme_minimal() +
  labs(x = "NMDS1", y = "NMDS3") +
  theme_classic()

# Print the plot
print(nmds_plot)


# Look with centroids
group_centroids <- data.frame(
  Location = c("Solanum mauritianum", "Ligustrum lucidum", "Paraserianthes lophantha"),
  Centroid_X = c(mean(z.points$MDS1[z.points$Group == "Solanum mauritianum"]),
                 mean(z.points$MDS1[z.points$Group == "Ligustrum lucidum"]),
                 mean(z.points$MDS1[z.points$Group == "Paraserianthes lophantha"])),
  Centroid_Y = c(mean(z.points$MDS2[z.points$Group == "Solanum mauritianum"]),
                 mean(z.points$MDS2[z.points$Group == "Ligustrum lucidum"]),
                 mean(z.points$MDS2[z.points$Group == "Paraserianthes lophantha"])),
  Centroid_Z = c(mean(z.points$MDS3[z.points$Group == "Solanum mauritianum"]),
                 mean(z.points$MDS3[z.points$Group == "Ligustrum lucidum"]),
                 mean(z.points$MDS3[z.points$Group == "Paraserianthes lophantha"])),
  Centroid_A = c(mean(z.points$MDS4[z.points$Group == "Solanum mauritianum"]),
                 mean(z.points$MDS4[z.points$Group == "Ligustrum lucidum"]),
                 mean(z.points$MDS4[z.points$Group == "Paraserianthes lophantha"])),
  Centroid_B = c(mean(z.points$MDS5[z.points$Group == "Solanum mauritianum"]),
                 mean(z.points$MDS5[z.points$Group == "Ligustrum lucidum"]),
                 mean(z.points$MDS5[z.points$Group == "Paraserianthes lophantha"]))
)

# Look with centroids for the subset data
group_centroids_subset <- data.frame(
  Location = c("Solanum mauritianum", "Ligustrum lucidum", "Paraserianthes lophantha"),
  Centroid_X = c(mean(z.subset.points$MDS1[z.subset.points$Group == "Solanum mauritianum"]),
                 mean(z.subset.points$MDS1[z.subset.points$Group == "Ligustrum lucidum"]),
                 mean(z.subset.points$MDS1[z.subset.points$Group == "Paraserianthes lophantha"])),
  Centroid_Y = c(mean(z.subset.points$MDS2[z.subset.points$Group == "Solanum mauritianum"]),
                 mean(z.subset.points$MDS2[z.subset.points$Group == "Ligustrum lucidum"]),
                 mean(z.subset.points$MDS2[z.subset.points$Group == "Paraserianthes lophantha"])),
  Centroid_Z = c(mean(z.subset.points$MDS3[z.subset.points$Group == "Solanum mauritianum"]),
                 mean(z.subset.points$MDS3[z.subset.points$Group == "Ligustrum lucidum"]),
                 mean(z.subset.points$MDS3[z.subset.points$Group == "Paraserianthes lophantha"]))
)

# Sort z.points.weeds to match group-centroids order
# Define the custom order for the 'group' column
z.points$Group <- factor(z.points$Group, levels = c('Solanum mauritianum', 'Ligustrum lucidum', 'Paraserianthes lophantha'))

# Sort the data frame based on the custom order
z.points <- z.points[order(z.points$Group), ]

# Repeat for subset data
z.subset.points$Group <- factor(z.subset.points$Group, levels = c('Solanum mauritianum', 'Ligustrum lucidum', 'Paraserianthes lophantha'))

# Sort the data frame based on the custom order
z.subset.points <- z.subset.points[order(z.subset.points$Group), ]


plot_data<-data.frame(
  Location = z.points$Group,
  MDS1=z.points$MDS1,
  MDS2=z.points$MDS2,
  MDS3=z.points$MDS3,
  MDS4=z.points$MDS4,
  MDS5=z.points$MDS5,
  xend=c(rep( group_centroids[1,2],27),rep(group_centroids[2,2],28), rep(group_centroids[3,2],27)),
  yend=c(rep( group_centroids[1,3],27),rep(group_centroids[2,3],28), rep(group_centroids[3,3],27)),
  zend=c(rep( group_centroids[1,4],27),rep(group_centroids[2,4],28), rep(group_centroids[3,4],27)),
  Aend=c(rep( group_centroids[1,5],27),rep(group_centroids[2,5],28), rep(group_centroids[3,5],27)),
  Bend=c(rep(group_centroids[1,6],27),rep( group_centroids[2,6],28), rep(group_centroids[3,6],27)))


plot_data_subset<-data.frame(
  Location = z.subset.points$Group,
  MDS1=z.subset.points$MDS1,
  MDS2=z.subset.points$MDS2,
  MDS3=z.subset.points$MDS3,
  xend=c(rep( group_centroids_subset[1,2],28),rep(group_centroids_subset[2,2],26), rep(group_centroids_subset[3,2],26)),
  yend=c(rep( group_centroids_subset[1,3],28),rep(group_centroids_subset[2,3],26), rep(group_centroids_subset[3,3],26)),
  zend=c(rep( group_centroids_subset[1,4],28),rep(group_centroids_subset[2,4],26), rep(group_centroids_subset[3,4],26)))

# ggplot with centroids
ggplot(plot_data, aes(x = MDS2, y = MDS4, shape = Location, color = Location)) +
  geom_point(size=2) +
  scale_color_manual(values = c("#EE6677", "#228833", "#44AA99")) + 
  scale_shape_manual(values = c(16, 15, 17)) + 
  stat_ellipse(aes(group = Location, fill = Location), geom = "polygon", alpha = 0.1) +
  scale_fill_manual(values = c("#EE6677",  "#228833", "#44AA99")) +
  geom_point(data = group_centroids, aes(x = Centroid_Y, y = Centroid_B, shape = Location, color = Location)) +
  geom_segment(data = plot_data, aes(x = MDS2, y = MDS4, 
                                     xend = yend, yend = Aend, color = Location), alpha = 0.5)+
  
  theme_classic()

# ggplot with centroids for subset data
ggplot(plot_data_subset, aes(x = MDS1, y = MDS3, shape = Location, color = Location)) +
  geom_point(size=2) +
  scale_color_manual(values = c("#EE6677", "#228833", "#44AA99")) + 
  scale_shape_manual(values = c(16, 15, 17)) + 
  stat_ellipse(aes(group = Location, fill = Location), geom = "polygon", alpha = 0.1) +
  scale_fill_manual(values = c("#EE6677",  "#228833", "#44AA99")) +
  geom_point(data = group_centroids_subset, aes(x = Centroid_X, y = Centroid_Z, shape = Location, color = Location)) +
  geom_segment(data = plot_data_subset, aes(x = MDS1, y = MDS3, 
                                     xend = xend, yend = zend, color = Location), alpha = 0.5)+
  
  theme_classic()

# I need to make a data frame that contains the environmental factors with the survey data
# That means I need Plot_species with PlotData_Weeds

Survey_wscores <- cbind(z.points, Env_Species) ## This combines the dataset with the coordinates
en = envfit(z, PlotData_Weeds, permutations = 9999, na.rm = TRUE) ## This creates the arrows
en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en) * 0.5 ##Adjust the last number to change the length of the arrows
en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en) * 0.5 ##Adjust the last number to change the length of the arrows


# Create a custom order for Plot data to get it in the same order as the subset data
PlotData_Weeds$CentralSpecies <- factor(PlotData_Weeds$CentralSpecies, levels = c('Solanum mauritianum', 'Ligustrum lucidum', 'Paraserianthes lophantha'))
# Sort the data frame based on the custom order
PlotData_Weeds <- PlotData_Weeds[order(PlotData_Weeds$CentralSpecies), ]

# Remove the plots that only had native species
PlotData_Weeds_subset<-PlotData_Weeds[-82, ] # this gets rid of the the column where there is a zero
PlotData_Weeds_subset<-PlotData_Weeds_subset[-80, ] # this gets rid of the the column where there is a zero

# I need to make a data frame that contains the environmental factors with the survey data
# That means I need Plot_species with PlotData_Weeds subset

Survey_subset_wscores <- cbind(z.subset.points, Env_Species_subset) ## This combines the dataset with the coordinates
en.subset = envfit(z.subset, PlotData_Weeds_subset, permutations = 9999, na.rm = TRUE) ## This creates the arrows
en_coord_cont.subset = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en) * 0.5 ##Adjust the last number to change the length of the arrows
en_coord_cat.subset = as.data.frame(scores(en, "factors")) * ordiArrowMul(en) * 0.5 ##Adjust the last number to change the length of the arrows



# ggplot with coordinates
ggplot(Survey_wscores, aes(x = MDS1, y = MDS2)) +
  geom_point(data = Survey_wscores, aes(x = MDS1, y = MDS2, shape = Group, color = Group)) +
#  geom_text(data = Survey_wscores, aes(x = NMDS1, y = NMDS2, label = Species), colour = "red")+
  scale_color_manual(values = c("#EE6677", "#228833", "#44AA99")) + 
  scale_shape_manual(values = c(16, 15, 17)) + 
  stat_ellipse(aes(group = Group, fill = Group), geom = "polygon", alpha = 0.25) +
  scale_fill_manual(values = c("#EE6677",  "#228833", "#44AA99")) +
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont, size = 1, alpha = 0.5, colour = "black") +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "black", 
            label = row.names(en_coord_cont)) +
  theme_classic() +
#  labs_pubr() +
  labs(x = "MDS axis 1", y = "MDS axis 2")

# ggplot with coordinates for subset data
ggplot(Survey_subset_wscores, aes(x = MDS1, y = MDS2)) +
  geom_point(data = Survey_subset_wscores, aes(x = MDS1, y = MDS2, shape = Group, color = Group)) +
#  geom_text(data = Survey_wscores, aes(x = NMDS1, y = NMDS2, label = Species), colour = "red")+
  scale_color_manual(values = c("#EE6677", "#228833", "#44AA99")) + 
  scale_shape_manual(values = c(16, 15, 17)) + 
  stat_ellipse(aes(group = Group, fill = Group), geom = "polygon", alpha = 0.25) +
  scale_fill_manual(values = c("#EE6677",  "#228833", "#44AA99")) +
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont.subset, size = 1, alpha = 0.5, colour = "black") +
  geom_text(data = en_coord_cont.subset, aes(x = NMDS1, y = NMDS2), colour = "black", 
            label = row.names(en_coord_cont.subset)) +
  theme_classic() +
#  labs_pubr() +
  labs(x = "MDS axis 1", y = "MDS axis 2")


#### PERMANOVA ####

# Filter down to only native species
# Remove native species plots
# Remove rows where Group is "Native"

SurveyData_Combined_subset_weeds <- SurveyData_Combined_subset_weeds %>% 
  filter(CentralSpecies != "Native")

# This code gives 0 values when species are not present in a plot
Survey_perm <- SurveyData_Combined_subset_weeds %>%
  pivot_wider(names_from = ScientificName, 
              values_from = Tier_1_sqrt, 
              id_cols = Plot) %>%
  mutate_all(~ replace(., is.na(.), 0))

# instead of being a tibble, I wanted to convert it back to a data frame
Survey_perm = as.data.frame(Survey_perm)


# Needs to remove the first column of numbers as row names and make the Scientific 
# names of species into the row names
row.names(Survey_perm) <- Survey_perm$Plot 
# Remove the first column from the data frame 
Survey_perm <- Survey_perm[, -1]

#Quick checks for empty rows or columns...
rowSums(Survey_perm)
colSums(Survey_perm)


Survey_perm<-Survey_perm[,-66] # this gets rid of the the column where there is a zero
Survey_perm<-Survey_perm[,-65] # this gets rid of the the column where there is a zero


# Make a distance matrix
perm_dist<-vegdist(Survey_perm, method='bray')

# Make a distance matrix
perm_dist<-vegdist(Survey_perm, method='bray')


#Assumptions

dispersion<-betadisper(perm_dist, group=plot_data_subset$Location,type = "centroid")

plot(dispersion)

anova(dispersion)


#Test

Perma_result<-adonis2( perm_dist~as.factor(plot_data_subset$Location), data=perm_dist,
                       permutations=9999)

Perma_result






# Change the species names to remove the space

library(dplyr)
Env_Species <- Env_Species %>%
  mutate(CentralSpecies = ifelse(CentralSpecies == "Solanum mauritianum", "Solanum_mauritianum", CentralSpecies))

# GLM results time
library(MASS) ## do to the GLM
Solmau_GLM <- glm.nb(Solanum_mauritanum ~ Height + DBH +
                     Slope + Canopy +
                     Erosion + Disturbance +
                     Pests + Litter,
                     data = Env_Species) ## this is a negative binominal generalised linear model as we are using count data and the data is quite widely dispersed
summary(Solmau_GLM)
