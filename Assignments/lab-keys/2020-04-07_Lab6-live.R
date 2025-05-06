### Lab 6. live coding

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

if(!require(DescTools)){install.packages("DescTools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()

### Chapter 13, Problem 20 ####
salmon <- read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")

# a.  Two methods that would be appropriate to test for a difference between two
#     groups are two-sample t-test and Welch's t-test (for groups with unequal
#     variance).  

# Look at the summary statistics
summ_skinColor <- salmon %>%
  group_by(species) %>% 
  summarise(mean_skinColor = mean(skinColor),
            sd_skinColor = sd(skinColor),
            n_skinColor = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_skinColor$sd_skinColor))/(min(summ_skinColor$sd_skinColor))

# Look at histograms, box plots, q-q plots
ggplot(salmon) +
  geom_histogram(aes(skinColor), binwidth = .1)+
  facet_wrap(~species)

ggplot(salmon) +
  geom_boxplot(aes(x = species, y = skinColor))+
  stat_summary(aes(x = species, y = skinColor), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(salmon)+
  geom_qq(aes(sample = skinColor, color = species))

# Based on the description, observations are independent and we assume that the
# fish were randomly sampled.
# The assumption of normality is met; although the histogram is difficult to 
# discern patterns in, the box plot shows that the median and mean of each group
# are similar, that the upper and lower whisker lengths of each group are similar,
# and the points in the q-q plots fall more or less on lines.
# The assumption of equal variance is not met because the ratio of smax to smin
# is 4.30, a value greater than 3.

salmon <- salmon %>%
  mutate(ln_color = log(skinColor))

# Look at the summary statistics for transformed ln_color
summ_ln_color <- salmon %>%
  group_by(species) %>% 
  summarise(mean_ln_color = mean(ln_color),
            sd_ln_color = sd(ln_color),
            n_ln_color = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_ln_color$sd_ln_color))/(min(summ_ln_color$sd_ln_color))

# Look at histograms, box plots, q-q plots
ggplot(salmon) +
  geom_histogram(aes(ln_color), binwidth = .1)+
  facet_wrap(~species)

ggplot(salmon) +
  geom_boxplot(aes(x = species, y = ln_color))+
  stat_summary(aes(x = species, y = ln_color), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(salmon)+
  geom_qq(aes(sample = ln_color, color = species))

t.test(ln_color ~ species, data = salmon, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

# b.  Yes, there is a difference in color.  We found that log transformed skin color was significantly higher in 
# Kokanee salmon when compared with Sockeye salmon (two-sample, two-sided t-test: 
# t = 12.133, df = 33, p-value = 1.038e-13).


### Chapter 13, Problem 25 ####

# Note: the variable biomassChange = Biomass after clearcut - Biomass before clearcut
forest <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")

ggplot(data = forest, mapping = aes(biomassChange))+
  geom_histogram(binwidth = 2)
ggplot(data = forest, mapping = aes(x = "", y = biomassChange))+
  geom_boxplot()+
  stat_summary(fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

forest <- forest %>%
  mutate(sqrt_change = sqrt(biomassChange))

# We found that the distribution of biomassChange was left-skewed; there were
# two low value outliers, the mean was lower than the median and the lower tail
# was longer than the upper tail.

SignTest(forest$biomassChange, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)

# There was no change in biomass of rainforest areas following clear-cutting
# (sign test: S = 21, number of differences = 36, p-value = 0.405).

### Chapter 13, Problem 26 ####

# Note that preference that is positive means the female spent more time 
# near the carotenoid-supplemented male.  Preference that is zero means the
# female spent equal time next to each brother.

beaks <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv", 
                  col_types = cols(p = col_integer()))
beaks <- tribble(
  ~preference,
  23,
  27,
  57,
  15,
  15,
  54,
  34,
  37,
  65,
  12
)

ggplot(beaks) +
  geom_histogram(aes(preference), binwidth = 7)

ggplot(beaks) +
  geom_boxplot(aes(x = "", y = preference))+
  stat_summary(aes(x = "", y = preference), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(beaks)+
  geom_qq(aes(sample = preference))

SignTest(beaks$preference, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)
# Females preferred carotenoid-supplemented males over their brothers (sign test:
# S = 10, number of differences = 10, p-value = 0.001953).

### Review 2, Problem 15 ####

return <- read_csv("datasets/abd/review2/rev2q15ReturnTrip.csv")
summary <- return %>%
  summarise(mean_y = mean(returnTripTimeScore),
            var_y = var(returnTripTimeScore),
            sd_y = sd(returnTripTimeScore),
            n_y = n(),
            se_y = sd(returnTripTimeScore)/sqrt(n()))

alpha <- 0.05
mean_y <- summary$mean_y
se_y <- summary$se_y
df_y <- summary$n_y -1
# In words, the confidence interval equation is 
# mean plus or minus the product of the critical value of t, given alpha and df, and the standard error of the mean.
# the mean you can calculate
# the "plus or minus" is accomplished with a short vector c(-1,-2) that you multiply by...
# the critical value of t using the function qt():  qt(1-alpha, df = n-1) which is also multiplied by...
# the standard error of the mean

# NOTE that in the swirl text it refers to the critical value of t as t_(n-1) which I think is bogus, but whatever.

# If you used summarise to calculate the descriptive statistics, then the code is
mean_y + c(-1,1)*qt(1-alpha, df_y )*se_y

# a.  The mean return trip time score is -0.5507.
# b.  The 95% CI for the mean return trip time score is -0.9842 < mu < -0.1172


# c.  I don't think I asked you to do this.  If I did, sorry.

ggplot(return) +
  geom_histogram(aes(returnTripTimeScore), binwidth = 1
                 )

ggplot(return) +
  geom_boxplot(aes(x = "", y = returnTripTimeScore))+
  stat_summary(aes(x = "", y = returnTripTimeScore), 
               fun.y=mean, 
               colour="blue", 
               fill = "blue",
               geom="point", 
               shape=21, 
               size=3)

ggplot(return)+
  geom_qq(aes(sample = returnTripTimeScore))

t.test(return$returnTripTimeScore, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

# People experienced a significantly shorter return trip when
# compared with their experience of the outbound trip (one-sample, two-sided
# t-test: t = -2.1185, df = 68, p-value = 0.03779)
