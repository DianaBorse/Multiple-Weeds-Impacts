### Exam 2 key

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

if(!require(DescTools)){install.packages("DescTools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()


corn <- tribble(
  ~yield,
11062, 
10268, 
10935, 
13221,
10852, 
10553, 
13760, 
14627, 
14927
)

# One-sided, HA that sample mean is less than null mean
t.test(corn$yield, alternative = "less", mu = 12500, conf.level = 0.95)

# Problem 9 ####
baker <- read_csv("datasets/demos/baker.csv")
baker <- baker %>%
  mutate(diff = After - Before)
ggplot(baker) +
  geom_histogram(aes(diff), binwidth = .1)
ggplot(baker) +
  geom_boxplot(aes(x = "", y = diff))
ggplot(baker)+
  geom_qq(aes(sample = diff))

SignTest(baker$diff, 
         alternative = "greater", mu = 0, conf.level = 0.95)

# Problem 10 ####
algae <- AlgaeCO2

summ_growthrate <- algae %>%
  group_by(treatment) %>% 
  summarise(mean_growthrate = mean(growthrate),
            sd_growthrate = sd(growthrate),
            n_growthrate = n())
ratio <-(max(summ_growthrate$sd_growthrate))/(min(summ_growthrate$sd_growthrate))
ggplot(algae) +
  geom_histogram(aes(growthrate), binwidth = .5)+
  facet_wrap(~treatment)
ggplot(algae) +
  geom_boxplot(aes(x = treatment, y = growthrate))
ggplot(algae)+
  geom_qq(aes(sample = growthrate, color = treatment))
t.test(growthrate ~ treatment, data = algae, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

