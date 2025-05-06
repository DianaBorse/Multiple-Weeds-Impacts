#### Exam II ####

# Clean up the working environment
rm(list = ls())
# Verify Working Directory
getwd()

### Install and load packages ####

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()

#### Question 17 ####

# Read in the data
baker <- read_csv("datasets/demos/baker.csv")

# Calculate difference
baker <- baker %>%
  mutate(diff = Before-After)

# Calculate summary statistics
summary <- baker %>%
  summarise(mean = mean(diff),
            median = median(diff),
            sd = sd(diff),
            n = n(),
            se = sd(diff)/sqrt(n()))

alpha <- 0.05
mean <- summary$mean
se <- summary$se
df <- summary$n -1

# Check if the data violates assumption of normality

# Histogram
ggplot(baker) +
  geom_histogram(aes(diff), binwidth = .5)

# Boxplot
ggplot(baker) +
  geom_boxplot(aes(x = "", y = diff))+
  stat_summary(aes(x = "", y = diff), 
               fun=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Q-Q plot
ggplot(baker)+
  geom_qq(aes(sample = diff))

# It does. Super not normal

# Transformation time! Differences have negative values, so we can't just take 
# Log(diff). 

# mutate so that it is diff^2
baker <- baker %>%
  mutate(diffsqrd = diff^2)

#Then take the log
baker <- baker %>%
  mutate(log_diffsqrd = log(diffsqrd))

# Check for normality after transformation:
summary <- baker %>%
  summarise(mean = mean(log_diffsqrd),
            median = median(log_diffsqrd),
            sd = sd(log_diffsqrd),
            n = n(),
            se = sd(log_diffsqrd)/sqrt(n()))

# close enough to a normal curve

# Histogram
ggplot(baker) +
  geom_histogram(aes(log_diffsqrd), binwidth = .5)

# Boxplot
ggplot(baker) +
  geom_boxplot(aes(x = "", y = log_diffsqrd))+
  stat_summary(aes(x = "", y = log_diffsqrd), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Mean and median are close enough together (had to look at the data itself),
# Whisker lengths are acceptably similar, no outliers, median is almost exactly 
# in the middle of the IQR box.

# Q-Q plot
ggplot(baker)+
  geom_qq(aes(sample = log_diffsqrd))

# Near enough approximation of a line.

# That made it acceptably normal, so I can perform a paired t-test.
t.test(baker$After, baker$Before, 
       alternative = "greater", paired =  TRUE, conf.level = 0.95)

#### Question 18 ####

# load data
install.packages("abd", repos="http://R-Forge.R-project.org")

library("abd")

algae <- AlgaeCO2

# Calculate summary statistics
summary <- algae %>%
  group_by(treatment) %>%
  summarise(mean = mean(growthrate),
            median = median(growthrate),
            sd = sd(growthrate),
            n = n(),
            se = sd(growthrate)/sqrt(n()))

# Test assumptions of a two-sample t-test
# Normality and homogeneous difference

# Normality
# Histogram
ggplot(algae) +
  geom_histogram(aes(growthrate), binwidth = .25)+
  facet_wrap(~treatment)

# Boxplot
ggplot(algae) +
  geom_boxplot(aes(x = treatment, y = growthrate))+
  stat_summary(aes(x = treatment, y = growthrate), 
               fun=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

# Q-Q plot
ggplot(algae)+
  geom_qq(aes(sample = growthrate))+
  facet_wrap(~treatment)

# Assumption of normality is met.

# Homogeneous variance
ratio <- (max(summary$sd))/(min(summary$sd))
# Ratio is 1.1265 which is less than three and is therefore acceptable

# Therefore, we can perform a two-sample two-sided t-test
t.test(growthrate ~ treatment, data = algae, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

### Code runs without breaking 10/10 ####