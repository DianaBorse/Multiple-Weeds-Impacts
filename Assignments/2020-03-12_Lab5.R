#### Lab 5 Confidence Intervals, t-tests, and friends ####

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

#### Confidence Intervals ####

# This is just a general outline for copy-pasting as needed.

# Data needed:
summarise(mean_variable = mean(variable),
          median_variable = median(variable),
          IQR_variable = IQR(variable),
          sd_variable = sd(variable),
          var_variable = var(variable),
          n_variable = n())

alpha <- 0.05
mean <- summary$mean
se <- summary$se
df <- summary$n -1

# mean + c(-1, 1)*qt(1-alpha, df)*se

#### Question 1 #####

library(readr)
Obliquity <- read_csv("datasets/demos/Obliquity.csv")
View(Obliquity)

# Calculate summary statistics

summary_Obliquity <- Obliquity %>%
  summarise(mean_Obliquity = mean(Obliquity),
          median_Obliquity = median(Obliquity),
          IQR_Obliquity = IQR(Obliquity),
          sd_Obliquity = sd(Obliquity),
          var_Obliquity = var(Obliquity),
          se_Obliquity = sd(Obliquity)/sqrt(n()),
          n_Obliquity = n())

view(summary_Obliquity)

alpha <- 0.05
mean <- 23.49878
se <- 	0.008771201
df <- 4
null_mean <- 23.4722

# Checking t-sample

t_sample <- (mean - null_mean)/(se)

view(t_sample)

#Performing the t-test

t.test(Obliquity$Obliquity, alternative = "two.sided", mu = 0, conf.level = 0.95)

#### Question 2 ####

heart <- read_csv("datasets/demos/HeartAttack_short.csv", col_types = cols(group = col_character()))

view(heart)

# Calculate summary statistics

summ_heart <- heart %>%
  group_by(group) %>%
  summarise(mean_cholest = mean(cholest),
            median_cholest = median(cholest),
            IQR_cholest = IQR(cholest),
            sd_cholest = sd(cholest),
            var_cholest = var(cholest),
            se_cholest = sd(cholest)/sqrt(n()),
            n_cholest = n()) 

view(summ_heart)

# Check the ratio for assumptions

ratio <- (max(summ_heart$sd_cholest))/(min(summ_heart$sd_cholest))

view(ratio)

#Test whether assumptions are violated
#Histogram and boxplot

ggplot(heart) +
  geom_histogram(aes(cholest), binwidth = 10)

ggplot(heart) +
  geom_boxplot(aes(x = group, y = cholest))

# Ignore the violated assumptions and perform a t-test anyway
# T-test

t.test(cholest ~ group, data = heart, var.equal = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)

# Try some transformations?

heart <- heart %>%
  mutate(logCHOLEST = log(cholest))

ggplot(heart) +
  geom_boxplot(aes(logCHOLEST))

# Transformation made it a bit more normal...

t.test(logCHOLEST ~ group, data = heart, var.equal = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)

# Results of t-test are similar

#### Question 3 ####

library(readr)
furness <- read_csv("datasets/quinn/chpt3/furness.csv")
View(furness)

# Calculate summary statistics

summary_furness <- furness %>%
  group_by(SEX) %>%
  summarise(mean_METRATE = mean(METRATE),
            median_METRATE = median(METRATE),
            IQR_METRATE = IQR(METRATE),
            sd_METRATE = sd(METRATE),
            var_METRATE = var(METRATE),
            se_METRATE = sd(METRATE)/sqrt(n()),
            n_METRATE = n())

view(summary_furness)

#Check if it violates assumptions

ggplot(furness) +
  geom_boxplot(aes(x = SEX, y = METRATE), varwidth = TRUE)

# perform wilcox test anyway

wilcox.test(METRATE ~ SEX, data = furness, var.equal = TRUE, alternative = "two.sided", mu = 0, conf.level = 0.95)

#### Question 4 ####

library(readr)
elgar <- read_csv("datasets/quinn/chpt3/elgar.csv")
View(elgar)

#Mutate so you have two columns for differences

elgar <- elgar %>%
  mutate(diffH = HORIZDIM - HORIZLIG)

elgar <-  elgar %>%
  mutate(diffV = VERTDIM - VERTLIGH)

# Calculate summary statistics for vertical (this shows that the distribution of
# differences for this dataset is non-normal, but only for vertical data)

summary2_elgar <- elgar %>%
  summarise(mean_diffV = mean(diffV),
            median_diffV = median(diffV),
            IQR_diffV = IQR(diffV),
            sd_diffV = sd(diffV),
            var_diffV = var(diffV),
            se_diffV = sd(diffV)/sqrt(n()),
            n_diffV = n())

view(summary2_elgar)

# Show non-normality with graphs

ggplot(elgar) +
  geom_boxplot(aes(diffV), varwidth = TRUE)

ggplot(elgar) +
  geom_histogram(aes(diffV), binwidth = 15)

ggplot(elgar)+
  geom_qq(aes(sample = diffV))

# Calculate summary statistics for horizontal differences and show that the 
# distribution of differences is approximately normal

summary_elgar <- elgar %>%
  summarise(mean_diffH = mean(diffH),
            median_diffH = median(diffH),
            IQR_diffH = IQR(diffH),
            sd_diffH = sd(diffH),
            var_diffH = var(diffH),
            se_diffH = sd(diffH)/sqrt(n()),
            n_diffH = n())

view(summary_elgar)

# visualize the normal distribution with graphs

ggplot(elgar) +
  geom_boxplot(aes(diffH), varwidth = TRUE)

ggplot(elgar) +
  geom_histogram(aes(diffH), binwidth = 15)

ggplot(elgar)+
  geom_qq(aes(sample = diffH))

# Perform paired t-test

t.test(elgar$HORIZDIM, elgar$HORIZLIG, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

#Show how it is the same when you switch the order.

t.test(elgar$HORIZLIG, elgar$HORIZDIM,
        alternative = "two.sided", paired = TRUE, conf.level = 0.95)
### 10/10 code runs without breaking ####