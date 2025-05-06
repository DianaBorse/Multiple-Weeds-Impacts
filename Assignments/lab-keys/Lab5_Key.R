### Lab 5. confidence intervals, t-tests and friends

# Answers to lab

# Clean up the working environment
rm(list = ls())
library("tidyverse")

# To perform sign tests, install and load the package DescTools
# install.packages("DescTools")
library("DescTools")

### Question 1 #########################
data <- read_csv("source, obliquity
              Regiomontanus, 23.5
              Copernicus, 23.47333
              Waltherus, 23.48778
              Danti, 23.50778
              Tycho, 23.525")
# Option A
null_mean <- 23.4722
y <-data$obliquity
# Calculate summary statistics
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1

# Whether you are given the values for mean/sd/n or calculate them, your next step is calculating t_sample
t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))

# For a two-sided test, the exact probability of obtaining t equal to t_sample or more extreme is calculated
# as:
two_tailed <- 2*(1-pt(abs(t_sample), df))

# Option B
# One-sample t-test can be calculate using t.test. 
# The mu argument gives the value stated in the null hypothesis.

# Two-sided
t.test(y, alternative = "two.sided", mu = null_mean, conf.level = 0.95)

### Question 2 #########################
heart <- read_csv("datasets/demos/HeartAttack_short.csv", col_types = cols(
  group = col_character()))
# Look at the summary statistics
summ_cholest <- heart %>%
  group_by(group) %>% 
  summarise(mean_cholest = mean(cholest),
            sd_cholest = sd(cholest),
            n_cholest = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_cholest$sd_cholest))/(min(summ_cholest$sd_cholest))

# Look at histograms, box plots, q-q plots
ggplot(heart) +
  geom_histogram(aes(cholest), binwidth = 10)+
  facet_wrap(~group)

ggplot(heart) +
  geom_boxplot(aes(x = group, y = cholest))

ggplot(heart)+
  geom_qq(aes(sample = cholest, color = group))

# Looks normal, but sd ratio is a little close to 3

# Two-sided
t.test(cholest ~ group, data = heart, var.equal = TRUE,
       alternative = "two.sided", conf.level = 0.95)

### Question 3 #########################
furness <- read_csv("datasets/quinn/chpt3/furness.csv")
# Two-sided
wilcox.test(METRATE ~ SEX,
            data = furness, alternative = "two.sided", conf.level = 0.95)

### Question 4 #########################
elgar <- read_csv("datasets/quinn/chpt3/elgar.csv")
t.test(elgar$HORIZLIG, elgar$HORIZDIM, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

