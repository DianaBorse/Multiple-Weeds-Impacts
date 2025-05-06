### Lab 3. Data manipulation and graphing

# Clean up the working environment
rm(list = ls())

### Install and load packages ####

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()

### Lovett Analysis ####
# Read in the data file and name it lovett
# Note that lovett is a dataset that does not have a grouping variable
lovett <- read_csv("datasets/quinn/chpt4/lovett.csv")

# Using descr() will give everything except variance
lovett %>%
  descr(SO4)

lovett %>%
  descr(SO4MOD)
  
# Or summarise can be used to calculate all summary statistics at once
summary_SO4 <- lovett %>%
  summarise(mean_SO4 = mean(SO4),
            median_SO4 = median(SO4),
            IQR_SO4 = IQR(SO4),
            sd_SO4 = sd(SO4),
            var_SO4 = var(SO4))

summary_SO4MOD <- lovett %>%
  summarise(mean_SO4MOD = mean(SO4MOD),
            median_SO4MOD = median(SO4MOD),
            IQR_SO4MOD = IQR(SO4MOD),
            sd_SO4MOD = sd(SO4MOD),
            var_SO4MOD = var(SO4MOD))

# Plot boxplots of SO4 and Modified SO4 using the code below.  
# I provided this for you.

# The code below modifies the dataset so it only contains SO4 and Modified SO4
# using select{dplyr}, and is oriented in long form using gather{tidyr}
lovett_tidy <- lovett %>%
  select(contains("SO4"))%>%
  gather(key = "type", value = "measurement", SO4, SO4MOD)

# The code below plots the two variables as boxplots, zooming in on the
# 40-75 range where most of the values are found (coord_cartesian).  The red 
# dots indicate the means (stat_summary).


ggplot(data = lovett_tidy)+
  geom_boxplot(aes(x = type, y = measurement), notch = TRUE)+
  coord_cartesian(ylim = c(40, 75))+
  stat_summary(aes(x = type, y = measurement), 
               fun.y=mean, 
               colour="darkred", 
               geom="point", 
               shape=18, 
               size=3)

### Sanchez Analysis ####
sanchez <- read_csv("datasets/demos/sanchez.csv")


sanchez_summary01 <- sanchez %>%
  group_by(COLTYPE) %>%
  summarise(n_BEETLE96 = n(),
            mean_BEETLE96 = mean(BEETLE96),
            median_BEETLE96 = median(BEETLE96),
            sd_BEETLE96 = sd(BEETLE96),
            IQR_BEETLE96 = IQR(BEETLE96),
            var_BEETLE96 = var(BEETLE96),
            se_BEETLE96 = sd(BEETLE96)/sqrt(n()))

# Add a log transformed beetle density.  We need to add 1, because
# log(0) is undefined

sanchez <- sanchez %>%
  mutate(log1beetle = log(BEETLE96 + 1))

# Note that I gave this summary table a different name so it 
# did not overwrite the table I created earlier on!
sanchez_summary02 <- sanchez %>%
  group_by(COLTYPE) %>%
  summarise(n_log1beetle = n(),
            mean_log1beetle = mean(log1beetle),
            median_log1beetle = median(log1beetle),
            sd_log1beetle = sd(log1beetle),
            IQR_log1beetle = IQR(log1beetle),
            var_log1beetle = var(log1beetle),
            se_log1beetle = sd(log1beetle)/sqrt(n()))

# Histogram of untransformed beetle density grouped by colony type
ggplot(sanchez) +
  geom_histogram(aes(BEETLE96), binwidth = 20)+
  facet_wrap(~COLTYPE)


# Boxplots of untransformed beetle density grouped by colony type

# For future reference, x will be the grouping variable (e.g.COLTYPE)
# and y will be the variable you would like to plot, such as BEETLE96

ggplot(data = sanchez)+
  geom_boxplot(aes(x = COLTYPE, y = BEETLE96), notch = TRUE)+
  stat_summary(aes(x = COLTYPE, y = BEETLE96), 
               fun.y=mean, 
               colour="darkred", 
               geom="point", 
               shape=18, 
               size=3)

# Histogram of log + 1 transformed beetle density grouped by colony type
# Note that I need to change the binwidth for it to be reasonable looking

ggplot(sanchez) +
  geom_histogram(aes(log1beetle), binwidth = 1)+
  facet_wrap(~COLTYPE)


# Boxplots of log + 1 transformed beetle density grouped by colony type
ggplot(data = sanchez)+
  geom_boxplot(aes(x = COLTYPE, y = log1beetle), notch = TRUE)+
  stat_summary(aes(x = COLTYPE, y = log1beetle), 
               fun.y=mean, 
               colour="darkred", 
               geom="point", 
               shape=18, 
               size=3)
