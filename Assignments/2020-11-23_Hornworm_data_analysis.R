#### Hornworm data analysis #### 

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

library("ggfortify")
library("broom")
# Load tidyverse
library("tidyverse") 

# Check for updates
tidyverse_update()

### Read in data ####

library(readr)
hornworms_tidy <- read_csv("Assignments/hornworms_tidy.csv")

data <- hornworms_tidy
summary <- hornworms_tidy %>%
  summarise(mean = mean(size),
            sd = sd(size),
            n = n(),
            se = sd(size)/sqrt(n()))

# Test for normality
ggplot(data = hornworms_tidy)+
  geom_boxplot(aes(y= size))
# Normal enough

# graph of size versus max larvae
ggplot(data = hornworms_tidy)+
  geom_smooth(aes(x = size, y= max_larvae))

# Graph of ranked size vs max larvae
ggplot(data = hornworms_tidy)+
  geom_point(aes(x = ranked_size, y= max_larvae))

# Graphs of ranked size vs max wasps
ggplot(data = hornworms_tidy)+
  geom_smooth(aes(x = ranked_size, y= max_wasps))

ggplot(data = hornworms_tidy)+
  geom_point(aes(x = ranked_size, y= max_wasps))


#### Statistical test for size differences between groups ####

# one sided t-test
# HA: clean hornworms are larger than infected hornworms
# H0: clean hornworms are the same size as or smaller than infected hornworms

# One-sided one-sample t-test

summ_hornworms_tidy <- hornworms_tidy %>%
  group_by(group) %>%
  summarise(mean_size = mean(size),
            median_size = median(size),
            IQR_size = IQR(size),
            sd_size = sd(size),
            var_size = var(size),
            se_size = sd(size)/sqrt(n()),
            n_size = n()) 

# test for violations of normality

ggplot(hornworms_tidy) +
  geom_histogram(aes(size), binwidth = 500)+
facet_wrap(~group)

ggplot(hornworms_tidy) +
  geom_boxplot(aes(x = "", y = size))+
  facet_wrap(~group)

ggplot(hornworms_tidy) +
  geom_qq(aes(sample = size))+
  facet_wrap(~group)

# Normal enough!!


# Try a transformation anyway

hornworms_tidy <- hornworms_tidy %>%
  mutate(logsize = log(size))

ggplot(hornworms_tidy) +
  geom_histogram(aes(logsize), binwidth = .1)+
  facet_wrap(~group)

ggplot(hornworms_tidy) +
  geom_boxplot(aes(x = "", y = logsize))

ggplot(hornworms_tidy) +
  geom_qq(aes(sample = logsize))

# transformation made it much worse, so go back to using OG data
#Proceed with one-sided, two-sample t-test

t.test(size ~ group, data = hornworms_tidy, var.equal = TRUE, alternative = "greater", conf.level = 0.95)

#### Larvae survivorship ####

summ_hornworms_tidy <- hornworms_tidy %>%
  group_by(group) %>%
  summarise(mean_max_wasps = mean(max_wasps),
            median_max_wasps = median(max_wasps),
            IQR_max_wasps = IQR(max_wasps),
            sd_max_wasps = sd(max_wasps),
            var_max_wasps = var(max_wasps),
            se_max_wasps = sd(max_wasps)/sqrt(n()),
            n_max_wasps = n()) 

ggplot(data = hornworms_tidy)+
  geom_point(aes(x = max_larvae, y= max_wasps))

### Survival ####

library(readr)
infected_hornworms <- read_csv("Assignments/infected_hornworms.csv")

# Graph of max_larvae vs max_wasps
ggplot(data = infected_hornworms, aes(x = max_larvae, y = max_wasps)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", color= "red", se = FALSE)

# plot of max_larvae and size with line of best fit
ggplot(data = infected_hornworms, aes(x = size, y = max_larvae)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", color= "red", se = FALSE)

# plot of max_wasps and size with line of best fit
ggplot(data = infected_hornworms, aes(x = size, y = max_wasps)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", color= "red", se = FALSE)

# Statistical test for correlation between size and survival within i group

# test for normality - size
ggplot(infected_hornworms) +
  geom_histogram(aes(size), binwidth = 100)

ggplot(infected_hornworms) +
  geom_boxplot(aes(x = "", y = size))

ggplot(infected_hornworms) +
  geom_qq(aes(sample = size))

# test for normality - max_wasps
ggplot(infected_hornworms) +
  geom_histogram(aes(max_wasps), binwidth = 3)

ggplot(infected_hornworms) +
  geom_boxplot(aes(x = "", y = max_wasps))

ggplot(infected_hornworms) +
  geom_qq(aes(sample = max_wasps))

# Correlation test

infectedCor <- cor.test(~ size + max_wasps, data = infected_hornworms,
                        method ="pearson")
infectedCor

# not a strong enough correlation, p>0.05

#### Linear Regression ####

# larvae vs size
model01 <- lm(max_larvae ~ size, data = hornworms_tidy)
autoplot(model01, smooth.colour = NA)

ggplot(data = hornworms_tidy)+
  geom_point(aes(x = size, y= resid(model01)))

summary(model01)

# wasps vs size

model02 <- lm(max_wasps ~ size, data = hornworms_tidy)
autoplot(model02, smooth.colour = NA)

ggplot(data = hornworms_tidy)+
  geom_point(aes(x = size, y= resid(model02)))

summary(model02)
