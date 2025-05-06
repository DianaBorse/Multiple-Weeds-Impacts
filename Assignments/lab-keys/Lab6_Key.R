### Lab 6. Additional practice with t-tests

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, load the package DescTools
library("DescTools")

### Problem 13-20 ######

# a.
# The experiment has two samples (Kokanee and Sockeye), so the appropriate test
# will be some variation on that theme.

salmon <- read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")

# Do the data meet the assumption of normality?  Check with plots.
ggplot(salmon) +
  geom_histogram(aes(skinColor), binwidth = .1)+
  facet_wrap(~species)
ggplot(salmon) +
  geom_boxplot(aes(x = species, y = skinColor))
ggplot(salmon)+
  geom_qq(aes(sample = skinColor, color = species))

# Yes, the data are symmetrical (top & bottom whisker equal for each box) 
# and mostly linear q-q plots, a bit of a wiggle in kokanee, but ok.

# Do the data meet the assumption of homogeneous variance?  Check ratio.
summ_salmon <- salmon %>%
  group_by(species) %>%
  summarise(mean_skin = mean(skinColor),
            sd_skin = sd(skinColor))
ratio <-(max(summ_salmon$sd_skin))/(min(summ_salmon$sd_skin))
# No, the groups do not have equal variance, the ratio is 4.29. 
# The two appropriate methods would be Welch's t-test or t-test on transformed
# values (if the transformation reduces the ration to <3)

# b.
# Begin with a ln transformation

salmon <- salmon %>%
  mutate(ln_skin = log(skinColor))
summ_ln_skin <- salmon %>%
  group_by(species) %>%
  summarise(mean_ln_skin = mean(ln_skin),
            sd_ln_skin = sd(ln_skin))
ratio <-(max(summ_ln_skin$sd_ln_skin))/(min(summ_ln_skin$sd_ln_skin))
# Ratio is now less than 3, can proceed with two sample, two-sided t-test
t.test(ln_skin ~ species, data = salmon, var.equal = TRUE, 
       alternative = "two.sided",
       conf.level = 0.95)

# Kokanee had significantly greater values for skin color, after ln
# transformation (two sample, two sided t-test: t = 12.13, df = 33, p < 0.0001).

### Problem 13-25 ######
# We are testing for a change in biomass after clearcutting, and we are given a
# single sample of biomass change.

forest <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")
ggplot(forest) +
  geom_histogram(aes(biomassChange), binwidth = 1)
ggplot(forest) +
  geom_boxplot(aes(x = "", y = biomassChange))
ggplot(forest)+
  geom_qq(aes(sample = biomassChange))

# Left skew seen in long lower tail in histogram violates the assumption of 
# normality, cannot fix with transformation because dataset includes negative 
# values (both log and sqrt of negative values undefined).  Proceed with sign
# test.  Null hypothesis is no change in biomass, so mu = 0.

SignTest(forest$biomassChange, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)

# There was no significant change in biomass following clearcutting (Sign test: 
# S = 21, n = 36, p = 0.405).

### Problem 13-26 ######

# Getting parsing errors (i.e., trouble reading in data correctly).  Try cleaning
# and reloading packages
finch <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv",
                  col_names = TRUE,
                  col_types = cols(preference = col_number()))
problems(finch)

# My version of the file will not read properly, so I'll just create a datatable.
beaks <- read_csv("preference
                  23
                  27
                  57
                  15
                  15
                  54
                  34
                  37
                  65
                  12",
                  col_names = TRUE)

# A value of 0 for the variable preference indicates equal time spent next to 
# each brother.  So we are basically testing whether the mean is equal to 0.
ggplot(beaks) +
  geom_histogram(aes(preference), binwidth = 10)
ggplot(beaks) +
  geom_boxplot(aes(x = "", y = preference))
ggplot(beaks)+
  geom_qq(aes(sample = preference))

# The lower whisker is shorter than the upper whisker and there is a bit of
# a sigmoid shape to the q-q plot.  Try transforming with arcsin (often used for
# percentages)
beaks <- beaks %>%
  mutate(arcsin_pref = asin(sqrt(preference/100)))
ggplot(beaks) +
  geom_boxplot(aes(x = "", y = arcsin_pref))
ggplot(beaks)+
  geom_qq(aes(sample = arcsin_pref))

# q-q plot is still pretty s-shaped, but the whiskers are a bit more even.
# You could justify doing a parametric one sample t-test, but a sign test is OK too.

SignTest(beaks$preference, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)
# Females preferred to perch next to carotenoid-enhanced males (Sign test: S=10,
# n = 10, p < 0.002).


### Problem Review-16 ######

fish <- read_csv("datasets/abd/chapter03/chap03q22ZebraFishBoldness.csv")
t.test(secondsAggressiveActivity ~ genotype, data = fish,
       var.equal = TRUE,
       alternative = "two.sided",
       conf.level = 0.95)
# a.
# The magnitude of the effect of the mutation is just the estimated difference
# between the means.  The 95% CI of the difference in the means defines so-called
# "appropriate bounds"
# TBH, the easiest way to calculate the 95% CI of the difference is to perform
# a two sample t-test.

summ_zebrafish <- fish %>%
  group_by(genotype) %>%
  summarise(mean = mean(secondsAggressiveActivity),
            sd = sd(secondsAggressiveActivity),
            n = n(),
            se = sd/sqrt(n()))
diff_mean <- 	142.1 - 74.0

# We estimate spd zebrafish spend an average of 68.1 seconds more time in 
# aggressive activity, when compared with wild type fish.  
# 95 % CI of the difference between the means is 25.9 < mu1 -mu2 < 110.3
# The effect of having the spd mutation is about a 25 to 110 second increase.

# b. 
ggplot(fish) +
  geom_histogram(aes(secondsAggressiveActivity), binwidth = 25)+
  facet_wrap(~genotype)
ggplot(fish) +
  geom_boxplot(aes(x = genotype, y = secondsAggressiveActivity))
ggplot(fish)+
  geom_qq(aes(sample = secondsAggressiveActivity, color = genotype))

# Well, with a sample size of 10-11 per group, you cant expect much from the
# histograms.  The boxplot shows  that the lower whisker of spd is shorter
# than the upper whisker, and in the wild type group, the median is toward the 
# third quartile.  It is such a small sample, it is kind of hard to tell if 
# normality has been met.  You could argue either way.

ratio <-(max(summ_zebrafish$sd))/(min(summ_zebrafish$sd))

# ratio is 1.39 so the assumption of equal variance has been met.

t.test(secondsAggressiveActivity ~ genotype, data = fish,
       var.equal = TRUE, 
       alternative = "two.sided", 
       conf.level = 0.95)

# Fish with the spd mutation spent significantly more time in aggressive activity
# (t = 3.38, df = 19,, p = 0.0031).