### Exam 1 Extra credit ####

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

### Load data ####
chap13e5SagebrushCrickets <- read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")

chap13e5SagebrushCrickets_summary01 <- chap13e5SagebrushCrickets %>%
  group_by(feedingStatus) %>%
  summarise(n_timeToMating = n(),
            mean_timeToMating = mean(timeToMating),
            median_timeToMating = median(timeToMating),
            sd_timeToMating = sd(timeToMating),
            IQR_timeToMating = IQR(timeToMating),
            var_timeToMating = var(timeToMating),
            se_timeToMating = sd(timeToMating)/sqrt(n()))

view(chap13e5SagebrushCrickets_summary01)

### Creating Histograms ####

#First, create a histogram for frequency of time mating for female sagebrush crickets
# who were fed and starved.

#To do this, excecute the following code
ggplot(chap13e5SagebrushCrickets)+ geom_histogram(aes(timeToMating), binwidth = 10)+
  facet_wrap(~feedingStatus)

### Modifying the data ####

#Now you will need to modify the data by transforming the histograms of time to
# mating for starved females and fed females

# To do this, excecute the following code
chap13e5SagebrushCrickets <- chap13e5SagebrushCrickets %>%
  mutate(log1timeToMating = log(timeToMating + 1))
 
# Now you will need to create the new histograms

#To do this, excecute the following code
ggplot(chap13e5SagebrushCrickets)+ geom_histogram(aes(log1timeToMating), binwidth = .25)+
  facet_wrap(~feedingStatus)
