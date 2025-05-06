### Lab 7 completed ####
# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()
#install.packages("rlang")- to update tidyverse

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

#####Reading in Data#####
river_data <-read_csv("datasets/demos/Jaffe.csv", col_types = cols(
  Depth = col_factor() ))


#1-5
# Look at the data
head(river_data)
summary(river_data)

#####Chech assumptions#####

#boxplots
#aldrin
ggplot(river_data) +
  geom_boxplot(aes(x = Depth, y = Aldrin))

#HCB
ggplot(river_data) +
  geom_boxplot(aes(x = Depth, y = HCB))

#histogram
ggplot(river_data) +
  geom_histogram(aes(Aldrin), binwidth = 1)+
  facet_wrap(~Depth)

ggplot(river_data) +
  geom_histogram(aes(HCB), binwidth = .5)+
  facet_wrap(~Depth)

#qq plot
ggplot(river_data)+
  geom_qq(aes(sample = Aldrin, color = Depth))

ggplot(river_data)+
  geom_qq(aes(sample = HCB, color = Depth))


#####Constructing ANOVA#####
Aldrinmodel <- lm(Aldrin~Depth, data = river_data)

HCBmodel <- lm(HCB~Depth, data= river_data)

#####Recheck assumptions#####

summ_Aldrin <- river_data %>%
  group_by(Depth) %>% 
  summarise(mean_concentration = mean(Aldrin),
            sd_concentration = sd(Aldrin),
            median_concentration= median(Aldrin),
            n_samples = n())

summ_HCB <- river_data %>%
  group_by(Depth) %>% 
  summarise(mean_concentration = mean(HCB),
            sd_concentration = sd(HCB),
            median_concentration= median(HCB),
            n_samples = n())

ratio_Aldrin <-(max(summ_Aldrin$sd_concentration))/(min(summ_Aldrin$sd_concentration))

ratio_HCB <-(max(summ_HCB$sd_concentration))/(min(summ_HCB$sd_concentration))

#####Regular ANOVA for HCB#####

#"Residuals vs. Fitted" graph
autoplot(HCBmodel)

# anova() to answer our research question
anova(HCBmodel)

summary(HCBmodel)

#####ANOVA on non-logged Aldrin#####

anova(Aldrinmodel)

summary(Aldrinmodel)

#####ANOVA on logged Aldrin#####

river_data <- mutate(river_data, log_Aldrin= log10(Aldrin))

AldrinLogModel <- lm(log_Aldrin~Depth, data = river_data)

summ_LogAldrin <- river_data %>%
  group_by(Depth) %>% 
  summarise(mean_concentration = mean(log_Aldrin),
            sd_concentration = sd(log_Aldrin),
            median_concentration= median(log_Aldrin),
            n_samples = n())

ratio_LogAldrin <-(max(summ_LogAldrin$sd_concentration))/(min(summ_LogAldrin$sd_concentration))

autoplot(AldrinLogModel)

anova(AldrinLogModel)

summary(AldrinLogModel)

#####Tukey-Kramer Honestly Significant Difference#####

tukey <- glht(AldrinLogModel, linfct = mcp(Depth = "Tukey"))


summary(tukey)
