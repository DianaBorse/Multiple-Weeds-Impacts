#### Lab 7: Introduction to 1-way ANOVA ####

# Clean up working environment
rm(list = ls())
# Verify working directory
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

#### Question 1 ####

# Load data
Jaffe <- read_csv("datasets/demos/Jaffe.csv")

# Do the necessary transformation for ANOVA
Jaffe <- Jaffe %>%
  mutate(Depth = fct_recode(Depth, Surface = "Surface",
                               Middepth = "Middepth",
                              Bottom = "Bottom"))
# Look at it

head(Jaffe)
summary(Jaffe)

#### Testing assumptions ####

# Normality Aldrin
ggplot(Jaffe) +
  geom_boxplot(aes(x = Depth, y = Aldrin))+
  stat_summary(aes(x = Depth, y = Aldrin), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)
ggplot(Jaffe) +
  geom_histogram(aes(Aldrin), binwidth = 1)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = Aldrin, color = Depth))

# Homogeneous variance

summ_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_Aldrin = mean(Aldrin),
            sd_Aldrin = sd(Aldrin),
            n_Aldrin = n())
ratio <-(max(summ_Aldrin$sd_Aldrin))/(min(summ_Aldrin$sd_Aldrin))

# Plots to check if the assumption of normality is met from HCB concentration
ggplot(Jaffe) +
  geom_boxplot(aes(x = Depth, y = HCB))+
  stat_summary(aes(x = Depth, y = HCB), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)
ggplot(Jaffe) +
  geom_histogram(aes(HCB), binwidth = 1)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = HCB)) +
          facet_wrap(~Depth)

# Checking for homogeneous variance HCB
summ_HCB <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_HCB = mean(HCB),
            sd_HCB = sd(HCB),
            n_HCB = n())
ratio <-(max(summ_HCB$sd_HCB))/(min(summ_HCB$sd_HCB))

# Normailty is met for both
# HCB has homogeneous variance
# Aldrin does not have homogeneous variance

# Must transform Aldrin:
Jaffe <- Jaffe %>%
  mutate(ln_Aldrin = log10(Aldrin))

# Try checking for normality again
ggplot(Jaffe) +
  geom_boxplot(aes(x = Depth, y = ln_Aldrin))+
  stat_summary(aes(x = Depth, y = ln_Aldrin), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)
ggplot(Jaffe) +
  geom_histogram(aes(ln_Aldrin), binwidth = .15)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = ln_Aldrin)) +
  facet_wrap(~Depth)

# Testing for homogeneous variance of Aldrin
summ_ln_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_ln_Aldrin = mean(ln_Aldrin),
            sd_ln_Aldrin = sd(ln_Aldrin),
            n_ln_Aldrin = n())
ratio <-(max(summ_ln_Aldrin$sd_ln_Aldrin))/(min(summ_ln_Aldrin$sd_ln_Aldrin))

#### Performing the ANOVA for Aldrin concentration ####

model01 <- lm(Aldrin~Depth, data = Jaffe)

# Check the assumptions again
summ_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_Aldrin = mean(Aldrin),
            sd_Aldrin = sd(Aldrin),
            n_Aldrin = n())
ratio <-(max(summ_Aldrin$sd_Aldrin))/(min(summ_Aldrin$sd_Aldrin))

autoplot(model01)

anova(model01)

# Now do the same thing for the log-transformed data
# Performing the ANOVA

model02 <- lm(ln_Aldrin~Depth, data = Jaffe)

# Check the assumptions again
summ_ln_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_ln_Aldrin = mean(ln_Aldrin),
            sd_ln_Aldrin = sd(ln_Aldrin),
            n_ln_Aldrin = n())
ratio <-(max(summ_ln_Aldrin$sd_ln_Aldrin))/(min(summ_ln_Aldrin$sd_ln_Aldrin))

autoplot(model02) 

anova(model02)

#### Perform the ANOVA for HCB concentration ####

model03 <- lm(HCB~Depth, data = Jaffe)

# Check the assumptions again
summ_HCB <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_HCB = mean(HCB),
            sd_HCB = sd(HCB),
            n_HCB = n())
ratio <-(max(summ_HCB$sd_HCB))/(min(summ_HCB$sd_HCB))

autoplot(model03)

anova(model03)

#### Perform a Tukey-Kramer Honestly Significant Difference (HSD) #### 
# pairwise comparison for transformed Aldrin

model02 <- lm(ln_Aldrin~Depth, data = Jaffe)

tukey <- glht(model02, linfct = mcp(Depth = "Tukey"))
summary(tukey)
