#### Lab 8: 1-way ANOVA, continued #### 
# For this lab you will use the datasets described in Chapter 15 of your book but you will 
# answer the slightly modified questions that I provide below

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
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

#### Problem 15-26 ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

malaria <-read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", col_types = cols(
  treatmentGroup = col_factor() ))
#Step 1
#normal one way fixed effects ANOVA
#checks for normality
ggplot(malaria) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 1)+
  facet_wrap(~treatmentGroup)
ggplot(malaria)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))
ggplot(malaria) +
  geom_boxplot(aes(x = treatmentGroup, y = logSporozoiteNumbers))


#Step 2

model03 <- lm(logSporozoiteNumbers~treatmentGroup, data = malaria)

#Step 3
summ_sporo <- malaria %>%
  group_by(treatmentGroup) %>% 
  summarise(mean_SporozoiteNum = mean(logSporozoiteNumbers),
            sd_SporozoiteNum = sd(logSporozoiteNumbers),
            n_SporozoiteNum = n())
ratio_sporo <-(max(summ_sporo$sd_SporozoiteNum))/(min(summ_sporo$sd_SporozoiteNum))

autoplot(model03)

#step 4
anova(model03)

#Tukey

tukey <- glht(model03, linfct = mcp(treatmentGroup = "Tukey"))
summary(tukey)


