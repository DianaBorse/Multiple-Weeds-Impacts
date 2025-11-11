#### Biomass for each plant by treatment group v2####


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

library(readr)
Biomass <- read_csv("Shadehouse/Biomass Experiment 1 Clean.csv")

library(dplyr)

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

#### Calculate LnRR for biomass ####

# Install and load the effectsize package if not already installed
# install.packages("effectsize")
library(effectsize)
library(dplyr)

# ggfortify is a package that works with ggplot2 to make nice plots
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
library("multcomp")
# nlme is used for random effects ANOVA
library("nlme")

#### Have a look at how sapling varies by treatment group ####
Sapling <- Biomass[Biomass$Plant == "ManukaSapling", ]

Sapling$Group <- as.factor(Sapling$Group)

# Load required packages
library(dplyr)
library(metafor)
library(ggplot2)

# Function to compute lnRR using metafor for a given plant and control group
compute_lnRR <- function(Biomass, plant_name = "ManukaSapling", control_group = 1) {
  # Filter for specified plant
  df_filtered <- Biomass %>% filter(Plant == plant_name)
  
  # Split into treatment and control
  control <- df_filtered %>% filter(Group == control_group)
  treatments <- df_filtered %>% filter(Group != control_group)
  
  # Summarise group stats
  group_stats <- treatments %>%
    group_by(Group) %>%
    summarise(
      m1i = mean(Mass),
      sd1i = sd(Mass),
      n1i = n(),
      .groups = "drop"
    ) %>%
    mutate(
      m2i = mean(control$Mass),
      sd2i = sd(control$Mass),
      n2i = nrow(control)
    )
  
  # Compute lnRR and sampling variance
  escalc_results <- escalc(
    measure = "ROM",
    m1i = group_stats$m1i,
    sd1i = group_stats$sd1i,
    n1i = group_stats$n1i,
    m2i = group_stats$m2i,
    sd2i = group_stats$sd2i,
    n2i = group_stats$n2i
  )
  
  # Combine with group info
  results <- bind_cols(group_stats %>% select(Group), escalc_results)
  return(results)
}

# Run the function
lnrr_output <- compute_lnRR(Biomass, plant_name = "ManukaSapling", control_group = 1)
print(lnrr_output)

# Plot lnRR with 95% CI
ggplot(lnrr_output, aes(x = factor(Group), y = yi)) +
  geom_point(shape = 16, size = 3) +
  geom_errorbar(aes(ymin = yi - 1.96 * sqrt(vi), ymax = yi + 1.96 * sqrt(vi)), width = 0.2) +
  labs(
    x = "Treatment Group",
    y = "lnRR_mixed/monoculture L. scoparium biomass (g)"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )

# Seedlings
Seedling <- Biomass[Biomass$Plant == "ManukaSeedling", ]

Seedling<-Seedling[-81, ]

Seedling$Group <- as.factor(Seedling$Group)

# Function to compute lnRR using metafor for a given plant and control group
compute_lnRR <- function(Biomass, plant_name = "ManukaSeedling", control_group = 1) {
  # Filter for specified plant
  df_filtered <- Biomass %>% filter(Plant == plant_name)
  
  # Split into treatment and control
  control <- df_filtered %>% filter(Group == control_group)
  treatments <- df_filtered %>% filter(Group != control_group)
  
  # Summarise group stats
  group_stats <- treatments %>%
    group_by(Group) %>%
    summarise(
      m1i = mean(Mass),
      sd1i = sd(Mass),
      n1i = n(),
      .groups = "drop"
    ) %>%
    mutate(
      m2i = mean(control$Mass),
      sd2i = sd(control$Mass),
      n2i = nrow(control)
    )
  
  # Compute lnRR and sampling variance
  escalc_results <- escalc(
    measure = "ROM",
    m1i = group_stats$m1i,
    sd1i = group_stats$sd1i,
    n1i = group_stats$n1i,
    m2i = group_stats$m2i,
    sd2i = group_stats$sd2i,
    n2i = group_stats$n2i
  )
  
  # Combine with group info
  results <- bind_cols(group_stats %>% select(Group), escalc_results)
  return(results)
}

# Run the function
lnrr_output <- compute_lnRR(Biomass, plant_name = "ManukaSeedling", control_group = 1)
print(lnrr_output)

# Plot lnRR with 95% CI
ggplot(lnrr_output, aes(x = factor(Group), y = yi)) +
  geom_point(shape = 16, size = 3) +
  geom_errorbar(aes(ymin = yi - 1.96 * sqrt(vi), ymax = yi + 1.96 * sqrt(vi)), width = 0.2) +
  labs(
    x = "Treatment Group",
    y = "lnRR_mixed/monoculture L. scoparium seedling biomass (g)"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )

