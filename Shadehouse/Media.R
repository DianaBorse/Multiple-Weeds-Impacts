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
model01 <- lm(TotalNitrogen_T2~Group, data = Media)
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

# Need to try another transformation
Media <- Media %>%
  mutate(sqrTotalCarbon_T2 = sqrt(TotalCarbon_T2))
# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_logTotalCarbon_T2 = mean(logTotalCarbon_T2),
            sd_logTotalCarbon_T2 = sd(logTotalCarbon_T2),
            n_logTotalCarbon_T2 = n())
ratio <-(max(summ_Media$sd_logTotalCarbon_T2))/(min(summ_Media$sd_logTotalCarbon_T2))
print(ratio)

# Check for Homogeneous variance
summ_Media <- Media %>%
  group_by(Group) %>% 
  summarise(mean_sqrTotalCarbon_T2 = mean(sqrTotalCarbon_T2),
            sd_sqrTotalCarbon_T2 = sd(sqrTotalCarbon_T2),
            n_sqrTotalCarbon_T2 = n())
ratio <-(max(summ_Media$sd_sqrTotalCarbon_T2))/(min(summ_Media$sd_sqrTotalCarbon_T2))
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

#### Pairwise Comparisons ####
# wattle comparisons
MediaWattle <- Media %>%
  mutate(Group = case_when(
    Group  %in% c(2, 4, 5, 7) ~ "wattle",
    Group %in% c(1, 3, 6, 8) ~ "no wattle",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

ggplot(MediaWattle, aes(x = Group, y = TotalNitrogen_T2, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(MediaWattle) +
  geom_histogram(aes(TotalNitrogen_T2), binwidth = 1)+
  facet_wrap(~Group)
ggplot(MediaWattle)+
  geom_qq(aes(sample = TotalNitrogen_T2, color = Group)) +
  theme_classic()

wilcox.test(Nitrate_T2 ~ Group, data = MediaWattle, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(Ammonium_T2 ~ Group, data = MediaWattle, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(C_NRatio_T2 ~ Group, data = MediaWattle, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(Phosphorus_T2 ~ Group, data = MediaWattle, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(Potassium_T2 ~ Group, data = MediaWattle, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
t.test(TotalNitrogen_T2 ~ Group, data = MediaWattle, var.equal = FALSE)

# privet tests
MediaPrivet <- Media %>%
  mutate(Group = case_when(
    Group  %in% c(2, 3, 5, 8) ~ "privet",
    Group %in% c(1, 4, 6, 7) ~ "no privet",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

t.test(TotalNitrogen_T2 ~ Group, data = MediaPrivet, var.equal = FALSE)
wilcox.test(Nitrate_T2 ~ Group, data = MediaPrivet, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(Ammonium_T2 ~ Group, data = MediaPrivet, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(C_NRatio_T2 ~ Group, data = MediaPrivet, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(Phosphorus_T2 ~ Group, data = MediaPrivet, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(Potassium_T2 ~ Group, data = MediaPrivet, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)

summary_Privet <- MediaPrivet %>%
  group_by(Group) %>%
  summarise(mean_Phosphorus_T2 = mean(Phosphorus_T2),
            median_Phosphorus_T2 = median(Phosphorus_T2),
            IQR_Phosphorus_T2 = IQR(Phosphorus_T2),
            sd_Phosphorus_T2 = sd(Phosphorus_T2),
            var_Phosphorus_T2 = var(Phosphorus_T2),
            se_Phosphorus_T2 = sd(Phosphorus_T2)/sqrt(n()),
            n_Phosphorus_T2 = n())

print(summary_Privet)

# Woolly tests
MediaWoolly <- Media %>%
  mutate(Group = case_when(
    Group  %in% c(2, 3, 4, 6) ~ "woolly",
    Group %in% c(1, 5, 7, 8) ~ "no woolly",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

t.test(TotalNitrogen_T2 ~ Group, data = MediaWoolly, var.equal = FALSE)
wilcox.test(Nitrate_T2 ~ Group, data = MediaWoolly, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(Ammonium_T2 ~ Group, data = MediaWoolly, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(C_NRatio_T2 ~ Group, data = MediaWoolly, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(Phosphorus_T2 ~ Group, data = MediaWoolly, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(Potassium_T2 ~ Group, data = MediaWoolly, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)


# mixture of weeds vs monoculture
# Woolly tests
MediaMono <- Media %>%
  mutate(Group = case_when(
    Group  %in% c(2, 3, 4, 5) ~ "mixture",
    Group %in% c(6, 7, 8) ~ "monoculture",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

# remove NA
MediaMono <- MediaMono %>%
  filter(!is.na(Group))

t.test(TotalNitrogen_T2 ~ Group, data = MediaMono, var.equal = FALSE)
wilcox.test(Nitrate_T2 ~ Group, data = MediaMono, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(Ammonium_T2 ~ Group, data = MediaMono, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(C_NRatio_T2 ~ Group, data = MediaMono, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(Phosphorus_T2 ~ Group, data = MediaMono, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(Potassium_T2 ~ Group, data = MediaMono, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)

summary_Mono <- MediaMono %>%
  group_by(Group) %>%
  summarise(mean_Nitrate_T2 = mean(Nitrate_T2),
            median_Nitrate_T2 = median(Nitrate_T2),
            IQR_Nitrate_T2 = IQR(Nitrate_T2),
            sd_Nitrate_T2 = sd(Nitrate_T2),
            var_Nitrate_T2 = var(Nitrate_T2),
            se_Nitrate_T2 = sd(Nitrate_T2)/sqrt(n()),
            n_Nitrate_T2 = n())

print(summary_Mono)





#### PCA ####
# Load required package
library(vegan)
library(dplyr)

library(factoextra)

media.pca <- prcomp(Media[,c("logC_NRatio_T2", "TotalNitrogen_T2", "logPotassium_T2", "logNitrate_T2", "Ammonium_T2", "Phosphorus_T2")], center = TRUE,scale. = TRUE,tol = 0.1)
summary(media.pca)
media.pca

#this generates the PC scores for each plot
axes_media.pca <- predict(media.pca, newdata = Media)
#making sure it worked
head(axes_media.pca, 4)

#creating a new dataframe that adds the the PC scores to the end
df_media.pca <- cbind(Media, axes_media.pca)

fviz_eig(media.pca,addlabels = TRUE) #scree plot

eig.val <- get_eigenvalue(media.pca) #getting eighvalue from each pca
eig.val

pca.var <- get_pca_var(media.pca)
pca.var$contrib
pca.var$coord
pca.var$cos2


# % contribution of the variables 
fviz_pca_var(media.pca, axes = c(1, 2), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# try NMDS

#remove non-numeric columns
Media2<-Media[,-2]
Media2<-Media2[,-4]
Media2<-Media2[,-3]
Media2<-Media2[,-2]
Media2<-Media2[,-36]

# Get down to only columns of interest
Media2<-Media2[,-36]
Media2<-Media2[,-35]
Media2<-Media2[,-34]
Media2<-Media2[,-33]
Media2<-Media2[,-32]
Media2<-Media2[,-31]
Media2<-Media2[,-30]
Media2<-Media2[,-29]
Media2<-Media2[,-28]
Media2<-Media2[,-26]
Media2<-Media2[,-24]
Media2<-Media2[,-23]
Media2<-Media2[,-22]
Media2<-Media2[,-21]
Media2<-Media2[,-20]
Media2<-Media2[,-19]
Media2<-Media2[,-18]
Media2<-Media2[,-17]
Media2<-Media2[,-16]
Media2<-Media2[,-14]
Media2<-Media2[,-13]
Media2<-Media2[,-12]
Media2<-Media2[,-10]
Media2<-Media2[,-8]
Media2<-Media2[,-6]
Media2<-Media2[,-5]
Media2<-Media2[,-4]
Media2<-Media2[,-3]
Media2<-Media2[,-2]

Media2 <- as.data.frame(Media2)

row.names(Media2) <- Media2$SampleNumber 
# Remove the first column from the data frame 
Media2 <- Media2[, -1]

library(vegan)
doubs.dist<-vegdist(Media2)
doubs.dist

# Check for Na, NaN,Inf values
any(is.na(doubs.dist))
any(is.infinite(doubs.dist))


#Classification
Media2<-hclust(doubs.dist,method='average')
plot(Media2,hang=-1) #The hang=-1 tidies it up so all the end nodes finish at the same level
grp<-cutree(Media2,k=30) #K=4 is saying to identify the dominant 4 groups.
grp
rect.hclust(Media2, k=30)

doubs.pca<-princomp(doubs.dist,cor=TRUE)
summary(doubs.pca) 
biplot(doubs.pca)

# Create PCA biplot with sites as points

# Load necessary libraries
library(ggplot2)
library(ggfortify)

autoplot(
  doubs.pca,
  data = doubs.dist,
  loadings = TRUE,             # Display loadings vectors
  loadings.label = TRUE,       # Display loadings labels
  loadings.colour = '#AA4499',     # Loadings vector color
  loadings.label.size = 3,     # Loadings label size
  loadings.label.colour = "#AA4499", # Loadings label color
  shape = 1) +                   # Use points for sites
  theme_minimal()              # Apply a minimal theme

#Moving on now to MDS
#DO MDS with vegan package
z <- metaMDS(comm = doubs.dist,
             autotransform = FALSE,
             distance = "bray",
             engine = "monoMDS",
             k = 2,
             weakties = TRUE,
             model = "global",
             maxit = 300,
             try = 40,
             trymax = 50)

z
# Stress Plot = Sheppard Plot
plot(z$diss, z$dist)

stressplot(object = z,
           p.col = "#88CCEE",
           l.col = "#882255",
           lwd = 1)

#always report the stress for MDS, never report the Rsquared you get from that plot above
plot(z)

plot(z[["points"]][,2]~z[["points"]][,1],main="Survey Data", xlab="NMDSaxis 1" , ylab= "NMDS axis 2", cex = 0.5 +as.numeric(Survey_wide$nit))

# Using ggplot2 to make a plot with site names
plot(z, type = "t")

# Looking at the stress/goodness of fit
gof <- goodness(object = z)
plot(z, display = "sites", type = "none")
points(z, display = "sites", cex = 2*gof/mean(gof))


# Make the points into a data frame for ggplot
z$points %>% head()
z.points <- data.frame(z$points)

