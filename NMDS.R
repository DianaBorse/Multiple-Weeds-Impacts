#### NMDS ####

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


#### Change the data to be a matrix (n sample units x p species) ####

# This code gives NA values when species are not present in a plot
#Survey_wideNA <- SurveyData_Combined %>%
#  pivot_wider(names_from = Plot, 
#              values_from = Tier_1, 
#              id_cols = ScientificName)

# autotransform is not doing anything, so I need to transform my data
# square root was given as a good transformation, so I will do that

SurveyData_Combined$Tier_1_sqrt <- sqrt(SurveyData_Combined$Tier_1)

# This code gives 0 values when species are not present in a plot
Survey_wide <- SurveyData_Combined %>%
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

# Note is that this is not subset, may need to subset to only the 
# most common species or species that occur more than 5 times etc.

#### NMDS ####

library(vegan)

#Quick checks for empty rows or columns...
rowSums(Survey_wide)
colSums(Survey_wide)

#Empty sites can cause problems so let's drop it
Survey_wide<-Survey_wide[-46,] # this gets rid of the the row 8 where there is a zero
Survey_wide<-Survey_wide[-49,] # this gets rid of the the row 8 where there is a zero

doubs.dist<-vegdist(Survey_wide)
doubs.dist

# Check for Na, NaN,Inf values
any(is.na(doubs.dist))
any(is.infinite(doubs.dist))


#Classification
Survey_wide<-hclust(doubs.dist,method='average')
plot(Survey_wide,hang=-1) #The hang=-1 tidies it up so all the end nodes finish at the same level (try dropping it to see what i mean)
grp<-cutree(Survey_wide,k=4) #K=4 is saying to identify the dominant 4 groups.
grp
rect.hclust(Survey_wide, k=4)


#Conduct a PCA on continuous data, commonly used for environmental 
#variable reduction. Say we want to combine variables that are similar in 
#for SEM we may use PCA

# linkage - once you have the two most similar sites together, how do you think about the similarity to the other sites. Best method is UPPGMA (an average weighed method)

# Now doing a PCA on the data
# Can do PCA on vegan or just the base programme of R, they just use different commands 
# (see powerpoint handout for more info)

doubs.pca<-princomp(doubs.dist,cor=FALSE)
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
Plot_Species <- SurveyData_Combined %>% distinct(Plot, .keep_all = TRUE)
Plot_Species<-Plot_Species[-46,] 
Plot_Species<-Plot_Species[-49,] 

grp <- Plot_Species$CentralSpecies

data.scores <- as.data.frame(scores(z))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(z)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- grp  #  add the grp variable created earlier
head(data.scores)  #look at the data


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

# This plot works!!!!!!!!

# Customize plot with ggplot2 add polygons/ellipses
nmds_plot <- ggplot(data = z.points, aes(x = MDS1, y = MDS2, shape = Group, color = Group)) +
  geom_point(size = 2) + # Set point size
  scale_shape_manual(values = c(16, 15, 18, 19)) + # Customize shapes
  scale_color_manual(values = c("#EE6677", "#228833", "#661100", "#44AA99")) + # Customize colors
  geom_polygon(data = z.points, aes(fill = Group, group = Group), alpha = 0.3) +
  theme_minimal() +
  labs(x = "NMDS1", y = "NMDS2")

# Print the plot
print(nmds_plot)

# Customize plot with ggplot2 add ellipses
nmds_plot <- ggplot(data = z.points, aes(x = MDS1, y = MDS2, shape = Group, color = Group)) +
  geom_point(size = 2) + # Set point size
  scale_shape_manual(values = c(16, 15, 18, 19)) + # Customize shapes
  scale_color_manual(values = c("#EE6677", "#228833", "#661100", "#44AA99")) + # Customize colors
  stat_ellipse(aes(group = Group, fill = Group), geom = "polygon", alpha = 0.3) +
  theme_minimal() +
  labs(x = "NMDS1", y = "NMDS2")

# Print the plot
print(nmds_plot)

# Native species plots are kind of all over the place, let's look at it without 
# Native plots

# Remove native species plots
# Remove rows where Group is "Native"
z.points.weeds <- z.points %>% 
  filter(Group != "Native")


# Customize plot with ggplot2 add ellipses
nmds_plot <- ggplot(data = z.points.weeds, aes(x = MDS1, y = MDS2, shape = Group, color = Group)) +
  geom_point(size = 2) + # Set point size
  scale_shape_manual(values = c(16, 15, 17)) + # Customize shapes
  scale_color_manual(values = c("#EE6677", "#661100", "#44AA99")) + # Customize colors
  stat_ellipse(aes(group = Group, fill = Group), geom = "polygon", alpha = 0.3) +
  theme_minimal() +
  labs(x = "NMDS1", y = "NMDS2")

# Print the plot
print(nmds_plot)





# This plot works, but it is just points.
p <- ggplot(data = z.points, aes(x = MDS1, y = MDS2)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

p + geom_point()


#### Subset to the top 50 species occurring across plots ####
# Try again but subset the data
top_50_values <- names(sort(table(SurveyData_Combined$ScientificName), decreasing = TRUE))[1:50] 

print(top_50_values)

# Subset the data frame to include only rows with the top 15 most common values 
subset_SurveyData_Combined <- SurveyData_Combined[SurveyData_Combined$ScientificName %in% top_50_values, ] 
# View the subset data frame 
print(subset_SurveyData_Combined)

# This code gives 0 values when species are not present in a plot
Survey_wide <- subset_SurveyData_Combined %>%
  pivot_wider(names_from = Plot, 
              values_from = Tier_1, 
              id_cols = ScientificName) %>%
  mutate_all(~ replace(., is.na(.), 0))

# instead of being a tibble, I wanted to convert it back to a data frame
Survey_wide = as.data.frame(Survey_wide)


# Needs to remove the first column of numbers as row names and make the Scientific 
# names of species into the row names
row.names(Survey_wide) <- Survey_wide$ScientificName 
# Remove the first column from the data frame 
Survey_wide <- Survey_wide[, -1]

# Need to change the Plots to numeric names

# Note is that this is not subset, may need to subset to only the 
# most common species or species that occur more than 5 times etc.

#### NMDS ####

library(vegan)

#Quick checks for empty rows or columns...
rowSums(Survey_wide)
colSums(Survey_wide)


doubs.dist<-vegdist(Survey_wide)
doubs.dist

# Check for Na, NaN,Inf values
any(is.na(doubs.dist))
any(is.infinite(doubs.dist))


#Classification
Survey_wide<-hclust(doubs.dist,method='average')
plot(Survey_wide,hang=-1) #The hang=-1 tidies it up so all the end nodes finish at the same level (try dropping it to see what i mean)
grp<-cutree(Survey_wide,k=4) #K=4 is saying to identify the dominant 4 groups.
grp
rect.hclust(Survey_wide, k=4)


#Coundct a PCA on continuous data, commonly used for envrionmental 
#variable reduction. Say we want to combine variables that are similar in 
#for SEM we may use PCA

# linkage - once you have the two most smilar sites together, how do you think about the smiliaritiy to the other sistes. Best method is UPPGMA (an avergage weighed method)

# Now doing a PCA on the data
# Can do PCA on vegan or just the base programme of R, they just use different commands 
# (see powerpoint handout for more info)

doubs.pca<-princomp(doubs.dist,cor=TRUE)
summary(doubs.pca) 
biplot(doubs.pca)


doubs.pca2<-rda(doubs.dist,scale=TRUE) #Important to have the scale=TRUE to scale the data, otherwise you end up with perfect horseshooe type data. (try it without the scale to see what I mean)
plot(doubs.pca2)
