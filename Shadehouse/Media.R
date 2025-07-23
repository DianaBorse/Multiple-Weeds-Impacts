#### Media Analysis Experiment 1 ####

# Before anything else, verify that your environment is totally clear.
# This is important, because old objects can foul up the works
# Clean up the working environment
rm(list = ls())

# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

#### Cleaning up the data ####

library(readr)
Media <- read_csv("Shadehouse/SoilTests_Time2.csv")

colnames(Media)[3:3] <- c("Group") ## Renaming the columns
Media$Group <- as.factor(Media$Group)

#### Calculating summary stats to look at change from baseline ####

summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_C_NRatio_T2 = mean(C_NRatio_T2),
            sd_C_NRatio_T2 = sd(C_NRatio_T2), mean_TotalNitrogen_T2 = mean(TotalNitrogen_T2),
            sd_TotalNitrogen_T2 = sd(TotalNitrogen_T2), mean_TotalCarbon_T2 = mean(TotalCarbon_T2),
            sd_TotalCarbon_T2 = sd(TotalCarbon_T2), ean_DryMatter_T2 = mean(DryMatter_T2),
            sd_DryMatter_T2 = sd(DryMatter_T2), mean_Sodium_T2 = mean(Sodium_T2),
            sd_Sodium_T2 = sd(Sodium_T2), mean_Magnesium_T2 = mean(Magnesium_T2),
            sd_Magnesium_T2 = sd(Magnesium_T2), mean_Calcium_T2 = mean(Calcium_T2),
            sd_Calcium_T2 = sd(Calcium_T2), mean_Potassium_T2 = mean(Potassium_T2),
            sd_Potassium_T2 = sd(Potassium_T2), mean_Sulphur_T2 = mean(Sulphur_T2),
            sd_Sulphur_T2 = sd(Sulphur_T2), mean_Phosphorus_T2 = mean(Phosphorus_T2),
            sd_Phosphorus_T2 = sd(Phosphorus_T2), mean_Ammonium_T2 = mean(Ammonium_T2),
            sd_Ammonium_T2 = sd(Ammonium_T2), mean_Nitrate_T2 = mean(Nitrate_T2),
            sd_Nitrate_T2 = sd(Nitrate_T2), mean_ElectricalConductivity_T2 = mean(ElectricalConductivity_T2),
            sd_ElectricalConductivity_T2 = sd(ElectricalConductivity_T2), mean_pH_T2 = mean(pH_T2),
            sd_pH_T2 = sd(pH_T2))
            

print(summ_Media)

#### Analyzing the differences for each test between groups ####

ggplot(Media, aes(x = Group, y = C_NRatio_T2, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(Media) +
  geom_histogram(aes(C_NRatio_T2), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Media)+
  geom_qq(aes(sample = C_NRatio_T2, color = Group)) +
  theme_classic()

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_C_NRatio_T2 = mean(C_NRatio_T2),
            sd_C_NRatio_T2 = sd(C_NRatio_T2),
            n_C_NRatio_T2 = n())
ratio <-(max(summ_Media$sd_C_NRatio_T2))/(min(summ_Media$sd_C_NRatio_T2))
print(ratio)

# Ratio is too high, try a transformation
Media <- Media %>%
  mutate(logC_NRatio_T2 = log(C_NRatio_T2))

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_logC_NRatio_T2 = mean(logC_NRatio_T2),
            sd_logC_NRatio_T2 = sd(logC_NRatio_T2),
            n_logC_NRatio_T2 = n())
ratio <-(max(summ_Media$sd_logC_NRatio_T2))/(min(summ_Media$sd_logC_NRatio_T2))
print(ratio)

# Did not help, kruskal wallis then
kruskal.test(C_NRatio_T2 ~ Group, data = Media)

# non-significant between treatment groups

# Total Nitrogen
ggplot(Media, aes(x = Group, y = TotalNitrogen_T2, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(Media) +
  geom_histogram(aes(TotalNitrogen_T2), binwidth = 0.01)+
  facet_wrap(~Group)
ggplot(Media)+
  geom_qq(aes(sample = TotalNitrogen_T2, color = Group)) +
  theme_classic()

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_TotalNitrogen_T2 = mean(TotalNitrogen_T2),
            sd_TotalNitrogen_T2 = sd(TotalNitrogen_T2),
            n_TotalNitrogen_T2 = n())
ratio <-(max(summ_Media$sd_TotalNitrogen_T2))/(min(summ_Media$sd_TotalNitrogen_T2))
print(ratio)

# below 3, can do an ANOVA
library("nlme")
library("multcomp")
model01 <- aov(TotalNitrogen_T2~Group, data = Media)
autoplot(model01)
anova(model01)
summary(model01)

# No difference in total N between groups

# Total Carbon
ggplot(Media, aes(x = Group, y = TotalCarbon_T2, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(Media) +
  geom_histogram(aes(TotalCarbon_T2), binwidth = 0.01)+
  facet_wrap(~Group)
ggplot(Media)+
  geom_qq(aes(sample = TotalCarbon_T2, color = Group)) +
  theme_classic()

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_TotalCarbon_T2 = mean(TotalCarbon_T2),
            sd_TotalCarbon_T2 = sd(TotalCarbon_T2),
            n_TotalCarbon_T2 = n())
ratio <-(max(summ_Media$sd_TotalCarbon_T2))/(min(summ_Media$sd_TotalCarbon_T2))
print(ratio)

# Need to try a transformation
Media <- Media %>%
  mutate(logTotalCarbon_T2 = log(TotalCarbon_T2))
# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_logTotalCarbon_T2 = mean(logTotalCarbon_T2),
            sd_logTotalCarbon_T2 = sd(logTotalCarbon_T2),
            n_logTotalCarbon_T2 = n())
ratio <-(max(summ_Media$sd_logTotalCarbon_T2))/(min(summ_Media$sd_logTotalCarbon_T2))
print(ratio)

# Did not help, kruskal wallis then
kruskal.test(TotalCarbon_T2 ~ Group, data = Media)

# No difference

# Sodium
ggplot(Media, aes(x = Group, y = Sodium_T2, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(Media) +
  geom_histogram(aes(Sodium_T2), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Media)+
  geom_qq(aes(sample = Sodium_T2, color = Group)) +
  theme_classic()

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_Sodium_T2 = mean(Sodium_T2),
            sd_Sodium_T2 = sd(Sodium_T2),
            n_Sodium_T2 = n())
ratio <-(max(summ_Media$sd_Sodium_T2))/(min(summ_Media$sd_Sodium_T2))
print(ratio)

# Need to try a transformation
Media <- Media %>%
  mutate(logSodium_T2 = log(Sodium_T2))
# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_logSodium_T2 = mean(logSodium_T2),
            sd_logSodium_T2 = sd(logSodium_T2),
            n_logSodium_T2 = n())
ratio <-(max(summ_Media$sd_logSodium_T2))/(min(summ_Media$sd_logSodium_T2))
print(ratio)

# Still too high
kruskal.test(Sodium_T2 ~ Group, data = Media)
# No difference

# Magnesium
ggplot(Media, aes(x = Group, y = Magnesium_T2, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(Media) +
  geom_histogram(aes(Magnesium_T2), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Media)+
  geom_qq(aes(sample = Magnesium_T2, color = Group)) +
  theme_classic()

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_Magnesium_T2 = mean(Magnesium_T2),
            sd_Magnesium_T2 = sd(Magnesium_T2),
            n_Magnesium_T2 = n())
ratio <-(max(summ_Media$sd_Magnesium_T2))/(min(summ_Media$sd_Magnesium_T2))
print(ratio)
# infinite (divided by a 0, so will just do kruskal-wallis)

kruskal.test(Magnesium_T2 ~ Group, data = Media)
# Not significant

# Potassium
ggplot(Media, aes(x = Group, y = Potassium_T2, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(Media) +
  geom_histogram(aes(Potassium_T2), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Media)+
  geom_qq(aes(sample = Potassium_T2, color = Group)) +
  theme_classic()

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_Potassium_T2 = mean(Potassium_T2),
            sd_Potassium_T2 = sd(Potassium_T2),
            n_Potassium_T2 = n())
ratio <-(max(summ_Media$sd_Potassium_T2))/(min(summ_Media$sd_Potassium_T2))
print(ratio)

# Try a transformation
Media <- Media %>%
  mutate(logPotassium_T2 = log(Potassium_T2))
# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_logPotassium_T2 = mean(logPotassium_T2),
            sd_logPotassium_T2 = sd(logPotassium_T2),
            n_logPotassium_T2 = n())
ratio <-(max(summ_Media$sd_logPotassium_T2))/(min(summ_Media$sd_logPotassium_T2))
print(ratio)

# Still > 3, need to do kruskal-wallis
kruskal.test(Potassium_T2 ~ Group, data = Media)
# Not significant

# Sulphur
ggplot(Media, aes(x = Group, y = Sulphur_T2, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(Media) +
  geom_histogram(aes(Sulphur_T2), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Media)+
  geom_qq(aes(sample = Sulphur_T2, color = Group)) +
  theme_classic()

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_Sulphur_T2 = mean(Sulphur_T2),
            sd_Sulphur_T2 = sd(Sulphur_T2),
            n_Sulphur_T2 = n())
ratio <-(max(summ_Media$sd_Sulphur_T2))/(min(summ_Media$sd_Sulphur_T2))
print(ratio)

## Try a transformation
Media <- Media %>%
  mutate(logSulphur_T2 = log(Sulphur_T2))
# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_logSulphur_T2 = mean(logSulphur_T2),
            sd_logSulphur_T2 = sd(logSulphur_T2),
            n_logSulphur_T2 = n())
ratio <-(max(summ_Media$sd_logSulphur_T2))/(min(summ_Media$sd_logSulphur_T2))
print(ratio)

# Still > 3, need to do kruskal-wallis
kruskal.test(Sulphur_T2 ~ Group, data = Media)
# Not significant

# Phosphorus
ggplot(Media, aes(x = Group, y = Phosphorus_T2, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(Media) +
  geom_histogram(aes(Phosphorus_T2), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Media)+
  geom_qq(aes(sample = Phosphorus_T2, color = Group)) +
  theme_classic()

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_Phosphorus_T2 = mean(Phosphorus_T2),
            sd_Phosphorus_T2 = sd(Phosphorus_T2),
            n_Phosphorus_T2 = n())
ratio <-(max(summ_Media$sd_Phosphorus_T2))/(min(summ_Media$sd_Phosphorus_T2))
print(ratio)

# Got an inf, so will just do kruskal-wallis
kruskal.test(Phosphorus_T2 ~ Group, data = Media)
# No difference

# Ammonium
ggplot(Media, aes(x = Group, y = Ammonium_T2, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(Media) +
  geom_histogram(aes(Ammonium_T2), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Media)+
  geom_qq(aes(sample = Ammonium_T2, color = Group)) +
  theme_classic()

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_Ammonium_T2 = mean(Ammonium_T2),
            sd_Ammonium_T2 = sd(Ammonium_T2),
            n_Ammonium_T2 = n())
ratio <-(max(summ_Media$sd_Ammonium_T2))/(min(summ_Media$sd_Ammonium_T2))
print(ratio)

# Got an inf, so will just do kruskal-wallis
kruskal.test(Ammonium_T2 ~ Group, data = Media)
# No difference

# Nitrate
ggplot(Media, aes(x = Group, y = Nitrate_T2, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(Media) +
  geom_histogram(aes(Nitrate_T2), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Media)+
  geom_qq(aes(sample = Nitrate_T2, color = Group)) +
  theme_classic()

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_Nitrate_T2 = mean(Nitrate_T2),
            sd_Nitrate_T2 = sd(Nitrate_T2),
            n_Nitrate_T2 = n())
ratio <-(max(summ_Media$sd_Nitrate_T2))/(min(summ_Media$sd_Nitrate_T2))
print(ratio)

# Try a transformation
Media <- Media %>%
  mutate(logNitrate_T2 = log(Nitrate_T2))
# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_logNitrate_T2 = mean(logNitrate_T2),
            sd_logNitrate_T2 = sd(logNitrate_T2),
            n_logNitrate_T2 = n())
ratio <-(max(summ_Media$sd_logNitrate_T2))/(min(summ_Media$sd_logNitrate_T2))
print(ratio)

# <3, do an ANOVA
model01 <- aov(Nitrate_T2~Group, data = Media)
anova(model01)
summary(model01)
# No difference

# ElectricalConductivity
ggplot(Media, aes(x = Group, y = ElectricalConductivity_T2, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(Media) +
  geom_histogram(aes(ElectricalConductivity_T2), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Media)+
  geom_qq(aes(sample = ElectricalConductivity_T2, color = Group)) +
  theme_classic()

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_ElectricalConductivity_T2 = mean(ElectricalConductivity_T2),
            sd_ElectricalConductivity_T2 = sd(ElectricalConductivity_T2),
            n_ElectricalConductivity_T2 = n())
ratio <-(max(summ_Media$sd_ElectricalConductivity_T2))/(min(summ_Media$sd_ElectricalConductivity_T2))
print(ratio)

# Got an inf, so will just do kruskal-wallis
kruskal.test(ElectricalConductivity_T2 ~ Group, data = Media)
# No difference

# pH
ggplot(Media, aes(x = Group, y = pH_T2, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(Media) +
  geom_histogram(aes(pH_T2), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Media)+
  geom_qq(aes(sample = pH_T2, color = Group)) +
  theme_classic()

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_pH_T2 = mean(pH_T2),
            sd_pH_T2 = sd(pH_T2),
            n_pH_T2 = n())
ratio <-(max(summ_Media$sd_pH_T2))/(min(summ_Media$sd_pH_T2))
print(ratio)

# Try a transformation
Media <- Media %>%
  mutate(logpH_T2 = log(pH_T2))
# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_logpH_T2 = mean(logpH_T2),
            sd_logpH_T2 = sd(logpH_T2),
            n_logpH_T2 = n())
ratio <-(max(summ_Media$sd_logpH_T2))/(min(summ_Media$sd_logpH_T2))
print(ratio)

# Still > 3, need to do kruskal-wallis
kruskal.test(pH_T2 ~ Group, data = Media)
# no difference