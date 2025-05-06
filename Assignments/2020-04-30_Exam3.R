#### EXAM 3 #### 

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

#### Question 5 ####

# Read in data

caffeine <- read_csv("datasets/demos/caffeine.csv", col_types = cols(
  group = col_factor() ))

caffeine <- caffeine %>%
  mutate(group = fct_recode(group, male = "male",
                              high_prog = "high_prog",
                              norm_prog = "norm_prog" ))

#vIEW
head(caffeine)
summary(caffeine)

#### Check for violations of Assumptions ####

# Normality

# Histogram
ggplot(caffeine) +
  geom_histogram(aes(half_life), binwidth = 1)+
  facet_wrap(~group)
# Boxplot
ggplot(caffeine, aes(x = group, y = half_life))+
  geom_boxplot() +
  theme_bw() 
# Q-Q plot
ggplot(caffeine)+
  geom_qq(aes(sample = half_life, color = group)) +
            facet_wrap(~group)

# Homogeneous variance
summ_half_life <- caffeine %>%
  group_by(group) %>% 
  summarise(mean_half_life = mean(half_life),
            sd_half_life = sd(half_life),
            n_half_life = n())
ratio <-(max(summ_half_life$sd_half_life))/(min(summ_half_life$sd_half_life))

# The data meet both the assumption of normality and of homogeneous variance 
# ratio = 1.697 (<3)

#### Performing the ANOVA ####

Model01 <- lm(half_life~group, data = caffeine)

autoplot(Model01)

anova(Model01)

# The ANOVA shows there is significant difference between at least two groups
# p = 0.00006528 (p < 0.05)

#### Planned comparrisons ####

# Between Men and women with normal progesterone levels
planned01 <- glht(Model01, linfct = 
                  mcp(group = c("male - norm_prog = 0")))
confint(planned01)
summary(planned01)

# Results
# Estimate Std. -0.6939
# Error 1.0690
# t -value -0.649
# Pr(>|t|) 0.522

# Between women with high progesterone and women with normal progesterone
planned02 <- glht(Model01, linfct = 
                    mcp(group = c("high_prog - norm_prog = 0")))
confint(planned02)
summary(planned02)

# Results
# Estimate Std. 4.688
# Error 1.162
# t -value 4.034
# Pr(>|t|) 0.000384

#### Summary of Results ####

# There was a significant difference between caffeine metabolism in women with 
# high progesterone and women with low progesterone (Planned Comparison: t= 4.034
# p = 0.000384)

# There was not a significant difference between men and women with normal levels 
# of progesterone (Planned Comparison: t = -0.69, p = 0.522).


#### Plots for question 4 ####
offspring <- read_csv("datasets/demos/offspring.csv")

# Visits
# Histogram
ggplot(offspring) +
  geom_histogram(aes(Visits), binwidth = 1)
# Boxplot
ggplot(offspring, aes(y = Visits)) +
  geom_boxplot() +
  theme_bw() 
# Q-Q plot
ggplot(offspring)+
  geom_qq(aes(sample = Visits))

# Babies
ggplot(offspring) +
  geom_histogram(aes(Babies), binwidth = 1)
# Boxplot
ggplot(offspring, aes(y = Babies)) +
  geom_boxplot() +
  theme_bw() 
# Q-Q plot
ggplot(offspring)+
  geom_qq(aes(sample = Babies))
