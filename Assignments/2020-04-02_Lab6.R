#### Lab 6 More Practice with t-tests, and friends ####

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


#### Problem 13-20 ####


Craig_Foote <- read_csv("~/Bio 375/Analyses/borse-diana/datasets/demos/Craig_Foote.csv")

# This problem has two independent samples, so I will start by analyzing assumptions
# of a two-sample t-test.

# Calculate summary statistics
summ_Craig_Foote <- Craig_Foote %>%
  group_by(Type) %>%
summarise(mean_Color = mean(Color),
          median_Color = median(Color),
          IQR_Color = IQR(Color),
          sd_Color = sd(Color),
          var_Color = var(Color),
          se_Color = sd(Color)/sqrt(n()),
          n_Color = n()) 

view(summ_Craig_Foote)

# Checking Normality of distributions

ggplot(Craig_Foote) +
  geom_histogram(aes(Color), binwidth = .1)+
  facet_wrap(~Type)

# Testing whether the variances are similar enough

ratio <- (max(summ_Craig_Foote$sd_Color))/(min(summ_Craig_Foote$sd_Color))

view(ratio)

# Trying a transformation

Craig_Foote <- Craig_Foote %>%
  mutate(logColor = log(Color))

summ_Craig_Foote <- Craig_Foote %>%
  group_by(Type) %>%
  summarise(mean_logColor = mean(logColor),
            median_logColor = median(logColor),
            IQR_logColor = IQR(logColor),
            sd_logColor = sd(logColor),
            var_logColor = var(logColor),
            se_logColor = sd(logColor)/sqrt(n()),
            n_logColor = n()) 

#Checking Normality

ggplot(Craig_Foote) +
  geom_histogram(aes(logColor), binwidth = .05)+
  facet_wrap(~Type)

ggplot(Craig_Foote) +
  geom_boxplot(aes(x = Type, y = logColor))


## Better plot?
ggplot(Craig_Foote) +
  geom_boxplot(aes(x = Type, y = logColor))+
  stat_summary(aes(x = Type, y = logColor), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

## Yes, better plot.

# Check if the ratio of sdmax to sdmin is acceptable

ratio <- (max(summ_Craig_Foote$sd_logColor))/(min(summ_Craig_Foote$sd_logColor))

view(ratio)

# Perform t-test (Mann Whitney U / Wilcox)

wilcox.test(Color ~ Type, data = Craig_Foote, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)

# We found that there was not a significant difference between Sockeye Salmon color
# and Kokanee Salmon color. (two sample wilcox-test: w = 303, df = 34, p = 1)


#### Problem 13-25 ####


chap13q25Clearcuts <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")

# This data has one sample, but the data is in terms of difference. Therefore, I 
# will be examining assumptions of a paired t-test.

# Calculate summary statistics
summ_chap13q25Clearcuts <- chap13q25Clearcuts %>%
  summarise(mean_biomassChange = mean(biomassChange),
            median_biomassChange = median(biomassChange),
            IQR_biomassChange = IQR(biomassChange),
            sd_biomassChange = sd(biomassChange),
            var_biomassChange = var(biomassChange),
            se_biomassChange = sd(biomassChange)/sqrt(n()),
            n_biomassChange = n()) 

view(summ_chap13q25Clearcuts)

# Test assumption of normality

ggplot(chap13q25Clearcuts) +
  geom_histogram(aes(biomassChange), binwidth = .65)

ggplot(chap13q25Clearcuts) +
  geom_boxplot(aes(x = "", y = biomassChange))

# Both of these show that the differences are not normally distributed

# Try a transformation

chap13q25Clearcuts <- chap13q25Clearcuts %>%
  mutate(logbiomassChange = log(biomassChange))

# Cannot take log of a negative number... oopse
# squared differences and then took the log of the squared differences

chap13q25Clearcuts <- chap13q25Clearcuts %>%
  mutate(squaredbiomassChange = (biomassChange^2))

chap13q25Clearcuts <- chap13q25Clearcuts %>%
  mutate(logsquaredbiomassChange = log(squaredbiomassChange))

# Test for normality

summ_chap13q25Clearcuts <- chap13q25Clearcuts %>%
  summarise(mean_logsquaredbiomassChange = mean(logsquaredbiomassChange),
            median_logsquaredbiomassChange = median(logsquaredbiomassChange),
            IQR_logsquaredbiomassChange = IQR(logsquaredbiomassChange),
            sd_logsquaredbiomassChange = sd(logsquaredbiomassChange),
            var_logsquaredbiomassChange = var(logsquaredbiomassChange),
            se_logsquaredbiomassChange = sd(logsquaredbiomassChange)/sqrt(n()),
            n_logsquaredbiomassChange = n()) 

ggplot(chap13q25Clearcuts) +
  geom_histogram(aes(logsquaredbiomassChange), binwidth = .5)

ggplot(chap13q25Clearcuts) +
  geom_boxplot(aes(x = "", y = logsquaredbiomassChange))

# This is more normal, but still not entirely normal.

# Sign test.

SignTest(chap13q25Clearcuts$logsquaredbiomassChange, alternative = "two.sided", mu = 0, conf.level = 0.95)

# We found that change in biomass between rainforests before and after next to 
# clearcuts next to them was significantly different
# (one sample sign test: s = 25, df = 34, p < .01)


#### Problem 13-26 ####


# Looks like it should be a one-sided test (prefference for males with redder beaks)
# The question could be evaluated wit a one sample one sided t-test

chap13q26ZebraFinchBeaks <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv")

# Calculate summary statistics

summ_chap13q26ZebraFinchBeaks <- chap13q26ZebraFinchBeaks %>%
  summarise(mean_preference = mean(preference),
            median_preference = median(preference),
            IQR_preference = IQR(preference),
            sd_preference = sd(preference),
            var_preference = var(preference),
            se_preference = sd(preference)/sqrt(n()),
            n_preference = n()) 

# Test for violations of assumptions of normality

ggplot(chap13q26ZebraFinchBeaks) +
  geom_histogram(aes(preference), binwidth = 5)

ggplot(chap13q26ZebraFinchBeaks) +
  geom_boxplot(aes(x = "", y = preference))

ggplot(chap13q26ZebraFinchBeaks) +
  geom_qq(aes(sample = preference))

# Assumption of normality is not violated (that much)

# Try a transformation anyway

chap13q26ZebraFinchBeaks <- chap13q26ZebraFinchBeaks %>%
  mutate(logpreference = log(preference))

# Calculate summary statistics

summ_chap13q26ZebraFinchBeaks <- chap13q26ZebraFinchBeaks %>%
  summarise(mean_logpreference = mean(logpreference),
            median_logpreference = median(logpreference),
            IQR_logpreference = IQR(logpreference),
            sd_logpreference = sd(logpreference),
            var_logpreference = var(logpreference),
            se_logpreference = sd(logpreference)/sqrt(n()),
            n_logpreference = n()) 

# Plots to test for normality

ggplot(chap13q26ZebraFinchBeaks) +
  geom_histogram(aes(logpreference), binwidth = .1)

ggplot(chap13q26ZebraFinchBeaks) +
  geom_boxplot(aes(x = "", y = logpreference))

ggplot(chap13q26ZebraFinchBeaks) +
  geom_qq(aes(sample = logpreference))

# Transformation may be a bit more normal...but not by much. Mean and median are 
# closer together

# Perform t-test on the transformed data

t.test(chap13q26ZebraFinchBeaks$logpreference, alternative = "greater", mu = 0, conf.level = .95)

# We found that there was significantly more preference by female Zebra finches
# for males with redder beaks (one sided t-test: t = 17.625, df = 9, p < .00001).


#### Problem 13-2-16 ####

# This is a two sample, two-sided question

Norton <- read_csv("datasets/demos/Norton.csv")

# Calculate summary statistics

summ_Norton <- Norton %>%
  group_by(Type) %>%
  summarise(mean_Time = mean(Time),
            median_Time = median(Time),
            IQR_Time = IQR(Time),
            sd_Time = sd(Time),
            var_Time = var(Time),
            se_Time = sd)

# difference between the means

# Test assumptions of a two sample t-test (normality and homogeneous variance)

# Normality

ggplot(Norton) +
  geom_histogram(aes(Time), binwidth = 20) + 
  facet_wrap(~Type)

ggplot(Norton) +
  geom_boxplot(aes(x = Type, y = Time))

ggplot(Norton) +
  geom_qq(aes(sample = Time))

# Homogeneous variance

ratio <- (max(summ_Norton$sd_Time))/(min(summ_Norton$sd_Time))

view(ratio)

# variance is acceptable (1.39 < 3.0)

# Not normally distributed, try transformation

Norton <-  Norton %>%
  mutate(logTime = log(Time))

# normal?

ggplot(Norton) +
  geom_histogram(aes(logTime), binwidth = .5) + 
  facet_wrap(~Type)

ggplot(Norton) +
  geom_boxplot(aes(x = Type, y = logTime))

ggplot(Norton) +
  geom_qq(aes(sample = logTime))

# Transformation made it worse.

# So it is not normal, proceed to wilcox test

wilcox.test(Time ~ Type, data = Norton, var.equal = TRUE,exact = FALSE, alternative = "two.sided", mu = 0, conf.level = 0.95)

# We found that there was a significant difference between spd mutants and Wild 
# type zebra fish in the amount of time spent in aggressive activity with their 
# reflection (two sided wilcox test: w = 90, df = 20, p = .01512).
