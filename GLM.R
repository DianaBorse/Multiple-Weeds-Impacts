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

library(readr)
SurveyData <- read_csv("SurveyData_Clean.csv")

library(readr)
SurveyData <- read_csv("SurveyData_Clean_WN_removed.csv")

library(readr)
PlotData <- read_csv("PlotData_Clean.csv")

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



# Repeat for plot data
# Assigning the Site a unique numeric value
PlotData$Site <- as.numeric(as.factor(PlotData$Site))
# Assigning W and N unique numeric values (weed is now 2, native is now 1)
PlotData$W_N <- as.numeric(as.factor(PlotData$Weed_Native))

# Now combine this into unique numerical plot names
library(tidyr)
PlotData_Combined <- PlotData %>%
  unite(Plot, Site, Plot, sep = "-")

# Unite Plot and Weed vs Native columns so that now the plot id is all in one
# column that includes the site, plot number, and whether it is weed or native
library(tidyr)
PlotData_Combined <- PlotData_Combined %>%
  unite(Location, Plot, Weed_Native, sep = "-")

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

# Back to the survey data setting up for nMDS

# Need to transform the data square root was given as a good transformation, so I will do that
SurveyData_Combined$Tier_1_sqrt <- sqrt(SurveyData_Combined$Tier_1)

# Remove rows where Group is "Native"
SurveyData_Combined_Weeds <- SurveyData_Combined %>% 
  filter(CentralSpecies != "Native")

# This code gives 0 values when species are not present in a plot
Survey_wide <- SurveyData_Combined_Weeds %>%
  pivot_wider(names_from = ScientificName, 
              values_from = Tier_1_sqrt, 
              id_cols = Plot) %>%
  mutate_all(~ replace(., is.na(.), 0))

# instead of being a tibble, I wanted to convert it back to a data frame
Survey_wide = as.data.frame(Survey_wide)


# Needs to remove the first column of numbers as row names and make the Scientific 
# names of species into the row names
row.names(Survey_wide) <- Survey_wide$Plot 
# Remove the first column from the data frame 
Survey_wide <- Survey_wide[, -1]


# Back to the Survey data
#Quick checks for empty rows or columns...
rowSums(Survey_wide)
colSums(Survey_wide)

# These are the columns that need to be removed using the data with the extra 
# woolly nightshades removed
Survey_wide<-Survey_wide[,-115] # this gets rid of the the column where there is a zero
Survey_wide<-Survey_wide[,-40] # this gets rid of the the column where there is a zero
Survey_wide<-Survey_wide[,-39] # this gets rid of the the column where there is a zero

### NMDS ####

library(vegan)

doubs.dist<-vegdist(Survey_wide)
doubs.dist

# Check for Na, NaN,Inf values
any(is.na(doubs.dist))
any(is.infinite(doubs.dist))


#Classification
Survey_wide<-hclust(doubs.dist,method='average')
plot(Survey_wide,hang=-1) #The hang=-1 tidies it up so all the end nodes finish at the same level
grp<-cutree(Survey_wide,k=30) #K=4 is saying to identify the dominant 4 groups.
grp
rect.hclust(Survey_wide, k=30)


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

# Stress Plot = Sheppard Plot
plot(z$diss, z$dist)

stressplot(object = z,
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


# Make the points into a data frame for ggplot
z$points %>% head()
z.points <- data.frame(z$points)

# I need to make a new data frame that is just the plots and the Central species
# for that plot
# Load the dplyr package
library(dplyr)

# Create a new data frame with unique Plot values
Plot_Species <- SurveyData_Combined_Weeds %>% distinct(Plot, .keep_all = TRUE)

grp <- Plot_Species$CentralSpecies

# I don't think I need these

# data.scores <- as.data.frame(scores(z))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
# data.scores$site <- rownames(z)  # create a column of site names, from the rownames of data.scores
# data.scores$grp <- grp  #  add the grp variable created earlier
# head(data.scores)  #look at the data


# Create a data frame of NMDS results with group information
z.points$Group <- Plot_Species$CentralSpecies # Replace with your group column

# Customize plot with ggplot2
nmds_plot <- ggplot(data = z.points, aes(x = MDS1, y = MDS2, shape = Group, color = Group)) +
  geom_point(size = 2) + # Set point size
  scale_shape_manual(values = c(16, 15, 18, 19)) + # Customize shapes
  scale_color_manual(values = c("#EE6677", "#228833", "#661100", "#44AA99")) + # Customize colors
  theme_minimal() +
  labs(x = "NMDS1", y = "NMDS2")

# Print the plot
print(nmds_plot)

# Customize plot with ggplot2 add ellipses
nmds_plot <- ggplot(data = z.points, aes(x = MDS2, y = MDS4, shape = Group, color = Group)) +
  geom_point(size = 2) + # Set point size
  scale_shape_manual(values = c(16, 15, 17)) + # Customize shapes
  scale_color_manual(values = c("#EE6677", "#661100", "#44AA99")) + # Customize colors
  stat_ellipse(aes(group = Group, fill = Group), geom = "polygon", alpha = 0.1) +
  theme_minimal() +
  labs(x = "NMDS2", y = "NMDS4")

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

# Sort z.points.weeds to match group-centroids order
# Define the custom order for the 'group' column
z.points$Group <- factor(z.points$Group, levels = c('Solanum mauritianum', 'Ligustrum lucidum', 'Paraserianthes lophantha'))

# Sort the data frame based on the custom order
z.points <- z.points[order(z.points$Group), ]


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

# ggplot with centroids
ggplot(plot_data, aes(x = MDS2, y = MDS4, shape = Location, color = Location)) +
  geom_point(size=2) +
  scale_color_manual(values = c("#EE6677", "#661100", "#44AA99")) + 
  scale_shape_manual(values = c(16, 15, 17)) + 
  stat_ellipse(aes(group = Location, fill = Location), geom = "polygon", alpha = 0.1) +
  geom_point(data = group_centroids, aes(x = Centroid_Y, y = Centroid_B, shape = Location, color = Location)) +
  geom_segment(data = plot_data, aes(x = MDS2, y = MDS4, 
                                     xend = yend, yend = Aend, color = Location), alpha = 0.5)+
  
  theme_bw()


en = envfit(z, PlotData_Weeds, permutations = 9999, na.rm = TRUE) ## This creates the arrows
en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en) * 0.5 ##Adjust the last number to change the length of the arrows
en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en) * 0.5 ##Adjust the last number to change the length of the arrows

# ggplot with centroids
ggplot(plot_data, aes(x = MDS2, y = MDS4, shape = Location, color = Location)) +
  geom_point(size=2) +
  scale_color_manual(values = c("#EE6677", "#661100", "#44AA99")) + 
  scale_shape_manual(values = c(16, 15, 17)) + 
  stat_ellipse(aes(group = Location, fill = Location), geom = "polygon", alpha = 0.1) +
  geom_point(data = group_centroids, aes(x = Centroid_Y, y = Centroid_B, shape = Location, color = Location)) +
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont, size = 1, alpha = 0.5, colour = "grey30") +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cont)) +
  
  theme_bw()
