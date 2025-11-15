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

# ggfortify is a package that works with ggplot2 to make nice plots
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
library("multcomp")
# nlme is used for random effects ANOVA
library("nlme")

#### Response Ratios ####
Sapling <- Biomass[Biomass$Plant == "ManukaSapling", ]

Sapling$Group <- as.factor(Sapling$Group)

# let's have a look at simply doing a response ratio (no log)

# Add a column that is the mass of each sample / mean control mass
group1_mean <- mean(Sapling$Mass[Sapling$Group == 1], na.rm = TRUE)

Sapling$ResponseRatio <- Sapling$Mass / group1_mean

# Let's have a look at that
library(dplyr)
library(ggplot2)

Sapling %>%
  filter(Group %in% 2:8) %>%
  ggplot(aes(x = Group, y = ResponseRatio)) +
  geom_boxplot(fill = "#33B08D", varwidth = TRUE) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  ylab("Sapling Biomass mixture / control (g)") +
  xlab("Treatment Group") +
  theme_classic()

# Check for homogeneous variance
summ_Sapling <- Sapling %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_Sapling$sd_Mass))/(min(summ_Sapling$sd_Mass))
print(ratio)

# Check for homogeneous variance
summ_Sapling <- Sapling %>%
  group_by(Group) %>% 
  summarise(mean_ResponseRatio = mean(ResponseRatio),
            sd_ResponseRatio = sd(ResponseRatio),
            n_ResponseRatio = n())
ratio <-(max(summ_Sapling$sd_ResponseRatio))/(min(summ_Sapling$sd_ResponseRatio))
print(ratio)

# ratio > 3
# Use kruskal-wallis
kruskal.test(Mass ~ Group, data = Sapling)
kruskal.test(ResponseRatio ~ Group, data = Sapling)

# Seedlings
Seedling <- Biomass[Biomass$Plant == "ManukaSeedling", ]

Seedling<-Seedling[-81, ]

Seedling$Group <- as.factor(Seedling$Group)

# Add a column that is the mass of each sample / mean control mass
group1_mean <- mean(Seedling$Mass[Seedling$Group == 1], na.rm = TRUE)

Seedling$ResponseRatio <- Seedling$Mass / group1_mean

Seedling %>%
  filter(Group %in% 2:8) %>%
  ggplot(aes(x = Group, y = ResponseRatio)) +
  geom_boxplot(fill = "#82C782", varwidth = TRUE) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  ylab("Seedling Biomass mixture / control (g)") +
  xlab("Treatment Group") +
  theme_classic()

# Check for homogeneous variance
summ_Seedling <- Seedling %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_Seedling$sd_Mass))/(min(summ_Seedling$sd_Mass))
print(ratio)

# Check for homogeneous variance
summ_Seedling <- Seedling %>%
  group_by(Group) %>% 
  summarise(mean_ResponseRatio = mean(ResponseRatio),
            sd_ResponseRatio = sd(ResponseRatio),
            n_ResponseRatio = n())
ratio <-(max(summ_Seedling$sd_ResponseRatio))/(min(summ_Seedling$sd_ResponseRatio))
print(ratio)

# ratio < 3
# For mass
# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(Seedling$Group) <- contr.sum(8)

model <- lm(Mass ~ Group, data = Seedling)
# Run Type III ANOVA
library(car)
Anova(model, type = 3)

# Set up for ANOVA
model01 <- aov(Mass~Group, data = Seedling)
anova(model01)
summary(model01)

# Tukey-Kramer test (automatically applied for unequal sample sizes)
TukeyHSD(model01)

# For Response Ratio
# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(Seedling$Group) <- contr.sum(8)

model <- lm(ResponseRatio ~ Group, data = Seedling)
# Run Type III ANOVA
library(car)
Anova(model, type = 3)

# Set up for ANOVA
model01 <- aov(ResponseRatio~Group, data = Seedling)
anova(model01)
summary(model01)
# Tukey-Kramer test (automatically applied for unequal sample sizes)
TukeyHSD(model01)

# Wattle
Wattle <- Biomass[Biomass$Plant == "Wattle", ]

Wattle$Group <- as.factor(Wattle$Group)

# Add a column that is the mass of each sample / mean control mass
group7_mean <- mean(Wattle$Mass[Wattle$Group == 7], na.rm = TRUE)

Wattle$ResponseRatio <- Wattle$Mass / group7_mean

Wattle %>%
  filter(Group %in% 2:6) %>%
  ggplot(aes(x = Group, y = ResponseRatio)) +
  geom_boxplot(fill = "#E9A96C", varwidth = TRUE) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  ylab("Wattle Biomass mixture / control (g)") +
  xlab("Treatment Group") +
  theme_classic()

# Check for homogeneous variance
summ_Wattle <- Wattle %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_Wattle$sd_Mass))/(min(summ_Wattle$sd_Mass))
print(ratio)

# Check for homogeneous variance
summ_Wattle <- Wattle %>%
  group_by(Group) %>% 
  summarise(mean_ResponseRatio = mean(ResponseRatio),
            sd_ResponseRatio = sd(ResponseRatio),
            n_ResponseRatio = n())
ratio <-(max(summ_Wattle$sd_ResponseRatio))/(min(summ_Wattle$sd_ResponseRatio))
print(ratio)

# Not normal, need to try a transformation
Wattle <- Wattle %>%
  mutate(logMass = log(Mass))

# Not normal, need to try a transformation
Wattle <- Wattle %>%
  mutate(logResponseRatio = log(ResponseRatio))

# Check for Homogeneous variance
summ_Wattle <- Wattle %>%
  group_by(Group) %>% 
  summarise(mean_logMass = mean(logMass),
            sd_logMass = sd(logMass),
            n_logMass = n())
ratio <-(max(summ_Wattle$sd_logMass))/(min(summ_Wattle$sd_logMass))
print(ratio)

# Check for Homogeneous variance
summ_Wattle <- Wattle %>%
  group_by(Group) %>% 
  summarise(mean_logResponseRatio = mean(logResponseRatio),
            sd_logResponseRatio = sd(logResponseRatio),
            n_logResponseRatio = n())
ratio <-(max(summ_Wattle$sd_logResponseRatio))/(min(summ_Wattle$sd_logResponseRatio))
print(ratio)

# Set up for ANOVA
model01 <- aov(logMass~Group, data = Wattle)
autoplot(model01)
anova(model01)
summary(model01)

# Perform Tukey test
tukey <- TukeyHSD(model01)
summary(tukey)
print(tukey)
plot(tukey, las = 1)

# Set up for ANOVA
model01 <- aov(logResponseRatio~Group, data = Wattle)
autoplot(model01)
anova(model01)
summary(model01)

# Perform Tukey test
tukey <- TukeyHSD(model01)
summary(tukey)
print(tukey)
plot(tukey, las = 1)

# Woolly
Woolly <- Biomass[Biomass$Plant == "Nightshade", ]

Woolly$Group <- as.factor(Woolly$Group)

# Add a column that is the mass of each sample / mean control mass
group6_mean <- mean(Woolly$Mass[Woolly$Group == 6], na.rm = TRUE)

Woolly$ResponseRatio <- Woolly$Mass / group6_mean

Woolly %>%
  filter(Group %in% 2:5) %>%
  ggplot(aes(x = Group, y = ResponseRatio)) +
  geom_boxplot(fill = "#CF597E", varwidth = TRUE) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  ylab("Woolly Biomass mixture / control (g)") +
  xlab("Treatment Group") +
  theme_classic()

# Check for homogeneous variance
summ_Woolly <- Woolly %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_Woolly$sd_Mass))/(min(summ_Woolly$sd_Mass))
print(ratio)

# Check for homogeneous variance
summ_Woolly <- Woolly %>%
  group_by(Group) %>% 
  summarise(mean_ResponseRatio = mean(ResponseRatio),
            sd_ResponseRatio = sd(ResponseRatio),
            n_ResponseRatio = n()) 
ratio <-(max(summ_Woolly$sd_ResponseRatio))/(min(summ_Woolly$sd_ResponseRatio))
print(ratio)

# ratio < 3
# For mass
# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(Woolly$Group) <- contr.sum(8)

model <- lm(Mass ~ Group, data = Woolly)
# Run Type III ANOVA
library(car)
Anova(model, type = 3)

# Set up for ANOVA
model01 <- aov(ResponseRatio~Group, data = Woolly)

autoplot(model01)

anova(model01)

summary(model01)
# Tukey-Kramer test (automatically applied for unequal sample sizes)
TukeyHSD(model01)

# For Response Ratio
# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(Woolly$Group) <- contr.sum(8)

model <- lm(ResponseRatio ~ Group, data = Woolly)
# Run Type III ANOVA
library(car)
Anova(model, type = 3)

# Set up for ANOVA
model01 <- aov(Mass~Group, data = Woolly)
anova(model01)
summary(model01)

# Tukey-Kramer test (automatically applied for unequal sample sizes)
TukeyHSD(model01)

# privet
Privet <- Biomass[Biomass$Plant == "Privet", ]

Privet$Group <- as.factor(Privet$Group)

# Add a column that is the mass of each sample / mean control mass
group8_mean <- mean(Privet$Mass[Privet$Group == 8], na.rm = TRUE)

Privet$ResponseRatio <- Privet$Mass / group8_mean

Privet %>%
#  filter(Group %in% 2:5) %>%
  ggplot(aes(x = Group, y = ResponseRatio)) +
  geom_boxplot(fill = "#E57F6C", varwidth = TRUE) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  ylab("Privet Biomass mixture / control (g)") +
  xlab("Treatment Group") +
  theme_classic()

# Check for homogeneous variance
summ_Privet <- Privet %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_Privet$sd_Mass))/(min(summ_Privet$sd_Mass))
print(ratio)

# Check for homogeneous variance
summ_Privet <- Privet %>%
  group_by(Group) %>% 
  summarise(mean_ResponseRatio = mean(ResponseRatio),
            sd_ResponseRatio = sd(ResponseRatio),
            n_ResponseRatio = n())
ratio <-(max(summ_Privet$sd_ResponseRatio))/(min(summ_Privet$sd_ResponseRatio))
print(ratio)

# ratio < 3
# For mass
# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(Privet$Group) <- contr.sum(8)

model <- lm(Mass ~ Group, data = Privet)
# Run Type III ANOVA
library(car)
Anova(model, type = 3)

# Set up for ANOVA
model01 <- aov(Mass~Group, data = Privet)
anova(model01)
summary(model01)

# Tukey-Kramer test (automatically applied for unequal sample sizes)
TukeyHSD(model01)

# For Response Ratio
# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(Privet$Group) <- contr.sum(8)

model <- lm(ResponseRatio ~ Group, data = Privet)
# Run Type III ANOVA
library(car)
Anova(model, type = 3)

# Set up for ANOVA
model01 <- aov(ResponseRatio~Group, data = Privet)
anova(model01)
summary(model01)
# Tukey-Kramer test (automatically applied for unequal sample sizes)
TukeyHSD(model01)


#### Log Response ratios ####
library(metafor)
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
  results <- bind_cols(group_stats["Group"], escalc_results)
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
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )

# Seedling
#Function to compute lnRR using metafor for a given plant and control group
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
  results <- bind_cols(group_stats["Group"], escalc_results)
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

# Wattle
#Function to compute lnRR using metafor for a given plant and control group
compute_lnRR <- function(Biomass, plant_name = "Wattle", control_group = 7) {
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
  results <- bind_cols(group_stats["Group"], escalc_results)
  return(results)
}

# Run the function
lnrr_output <- compute_lnRR(Biomass, plant_name = "Wattle", control_group = 7)
print(lnrr_output)

# Plot lnRR with 95% CI
ggplot(lnrr_output, aes(x = factor(Group), y = yi)) +
  geom_point(shape = 16, size = 3) +
  geom_errorbar(aes(ymin = yi - 1.96 * sqrt(vi), ymax = yi + 1.96 * sqrt(vi)), width = 0.2) +
  labs(
    x = "Treatment Group",
    y = "lnRR_mixed/monoculture P. lophantha seedling biomass (g)"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )

# Woolly
#Function to compute lnRR using metafor for a given plant and control group
compute_lnRR <- function(Biomass, plant_name = "Nightshade", control_group = 6) {
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
  results <- bind_cols(group_stats["Group"], escalc_results)
  return(results)
}

# Run the function
lnrr_output <- compute_lnRR(Biomass, plant_name = "Nightshade", control_group = 6)
print(lnrr_output)

# Plot lnRR with 95% CI
ggplot(lnrr_output, aes(x = factor(Group), y = yi)) +
  geom_point(shape = 16, size = 3) +
  geom_errorbar(aes(ymin = yi - 1.96 * sqrt(vi), ymax = yi + 1.96 * sqrt(vi)), width = 0.2) +
  labs(
    x = "Treatment Group",
    y = "lnRR_mixed/monoculture S. mauritianum seedling biomass (g)"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )

# Privet
#Function to compute lnRR using metafor for a given plant and control group
compute_lnRR <- function(Biomass, plant_name = "Privet", control_group = 8) {
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
  results <- bind_cols(group_stats["Group"], escalc_results)
  return(results)
}

# Run the function
lnrr_output <- compute_lnRR(Biomass, plant_name = "Privet", control_group = 8)
print(lnrr_output)

# Plot lnRR with 95% CI
ggplot(lnrr_output, aes(x = factor(Group), y = yi)) +
  geom_point(shape = 16, size = 3) +
  geom_errorbar(aes(ymin = yi - 1.96 * sqrt(vi), ymax = yi + 1.96 * sqrt(vi)), width = 0.2) +
  labs(
    x = "Treatment Group",
    y = "lnRR_mixed/monoculture L. lucidum seedling biomass (g)"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )
