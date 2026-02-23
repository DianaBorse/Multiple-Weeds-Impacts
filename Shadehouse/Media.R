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

#### Change in media paired comparisons ####

# make a new column that is the T 2 value minus the T 1 value
# Nitrate
Media <- Media %>%
  mutate(ChangeNitrate = (Nitrate_T2 - Nitrate_T1))
# Ammonium
Media <- Media %>%
  mutate(ChangeAmmonium = (Ammonium_T2 - Ammonium_T1))
# C_N Ratio
Media <- Media %>%
  mutate(ChangeC_NRatio = (C_NRatio_T2 - C_NRatio_T1))
# Phosphorus
Media <- Media %>%
  mutate(ChangePhosphorus = (Phosphorus_T2 - Phosphorus_T1))
# Potassium
Media <- Media %>%
  mutate(ChangePotassium = (Potassium_T2 - Potassium_T1))
# total Nitrogen
Media <- Media %>%
  mutate(ChangeTotalNitrogen = (TotalNitrogen_T2 - TotalNitrogen_T1))

# Show summary stats #
summ_MediaChange <- Media %>%
  group_by(Group) %>% 
  summarise(mean_ChangeC_NRatio = mean(ChangeC_NRatio),
            sd_ChangeC_NRatio = sd(ChangeC_NRatio), se_ChangeC_NRatio = sd(ChangeC_NRatio)/sqrt(n()), mean_ChangeTotalNitrogen = mean(ChangeTotalNitrogen),
            sd_ChangeTotalNitrogen = sd(ChangeTotalNitrogen), se_ChangeTotalNitrogen = sd(ChangeTotalNitrogen)/sqrt(n()),
            mean_ChangePotassium = mean(ChangePotassium), sd_ChangePotassium = sd(ChangePotassium),
            se_ChangePotassium = sd(ChangePotassium)/sqrt(n()), mean_ChangePhosphorus = mean(ChangePhosphorus),
            sd_ChangePhosphorus = sd(ChangePhosphorus), se_ChangePhosphorus = sd(ChangePhosphorus)/sqrt(n()), mean_ChangeAmmonium = mean(ChangeAmmonium),
            sd_ChangeAmmonium = sd(ChangeAmmonium), se_ChangeAmmonium = sd(ChangeAmmonium)/sqrt(n()), mean_ChangeNitrate = mean(ChangeNitrate),
            sd_ChangeNitrate = sd(ChangeNitrate), se_ChangeNitrate = sd(ChangeNitrate)/sqrt(n()))

print(summ_MediaChange)


library(writexl)

write_xlsx(summ_MediaChange, "C:/Users/bella/Documents/summ_MediaChange.xlsx")

# wattle comparisons
MediaWattle <- Media %>%
  mutate(Group = case_when(
    Group  %in% c(2, 4, 5, 7) ~ "wattle",
    Group %in% c(1, 3, 6, 8) ~ "no wattle",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

ggplot(MediaWattle, aes(x = Group, y = ChangeTotalNitrogen, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(MediaWattle) +
  geom_histogram(aes(ChangeTotalNitrogen), binwidth = 1)+
  facet_wrap(~Group)
ggplot(MediaWattle)+
  geom_qq(aes(sample = ChangeTotalNitrogen, color = Group)) +
  theme_classic()

wilcox.test(ChangeNitrate ~ Group, data = MediaWattle, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangeAmmonium ~ Group, data = MediaWattle, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangeC_NRatio ~ Group, data = MediaWattle, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangePhosphorus ~ Group, data = MediaWattle, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangePotassium ~ Group, data = MediaWattle, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
t.test(ChangeTotalNitrogen ~ Group, data = MediaWattle, var.equal = FALSE)

# nitrate
summary_Wattle <- MediaWattle %>%
  group_by(Group) %>%
  summarise(mean_ChangeNitrate = mean(ChangeNitrate),
            median_ChangeNitrate = median(ChangeNitrate),
            IQR_ChangeNitrate = IQR(ChangeNitrate),
            sd_ChangeNitrate = sd(ChangeNitrate),
            var_ChangeNitrate = var(ChangeNitrate),
            se_ChangeNitrate = sd(ChangeNitrate)/sqrt(n()),
            n_ChangeNitrate = n())

print(summary_Wattle)

# ammonium
summary_Wattle <- MediaWattle %>%
  group_by(Group) %>%
  summarise(mean_ChangeAmmonium = mean(ChangeAmmonium),
            median_ChangeAmmonium = median(ChangeAmmonium),
            IQR_ChangeAmmonium = IQR(ChangeAmmonium),
            sd_ChangeAmmonium = sd(ChangeAmmonium),
            var_ChangeAmmonium = var(ChangeAmmonium),
            se_ChangeAmmonium = sd(ChangeAmmonium)/sqrt(n()),
            n_ChangeAmmonium = n())

print(summary_Wattle)

# total nitrogen
summary_Wattle <- MediaWattle %>%
  group_by(Group) %>%
  summarise(mean_ChangeTotalNitrogen = mean(ChangeTotalNitrogen),
            median_ChangeTotalNitrogen = median(ChangeTotalNitrogen),
            IQR_ChangeTotalNitrogen = IQR(ChangeTotalNitrogen),
            sd_ChangeTotalNitrogen = sd(ChangeTotalNitrogen),
            var_ChangeTotalNitrogen = var(ChangeTotalNitrogen),
            se_ChangeTotalNitrogen = sd(ChangeTotalNitrogen)/sqrt(n()),
            n_ChangeTotalNitrogen = n())

print(summary_Wattle)


# privet
MediaPrivet <- Media %>%
  mutate(Group = case_when(
    Group  %in% c(2, 3, 5, 8) ~ "privet",
    Group %in% c(1, 4, 6, 7) ~ "no privet",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

ggplot(MediaPrivet, aes(x = Group, y = ChangeTotalNitrogen, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(MediaPrivet) +
  geom_histogram(aes(ChangeTotalNitrogen), binwidth = 1)+
  facet_wrap(~Group)
ggplot(MediaPrivet)+
  geom_qq(aes(sample = ChangeTotalNitrogen, color = Group)) +
  theme_classic()

wilcox.test(ChangeNitrate ~ Group, data = MediaPrivet, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangeAmmonium ~ Group, data = MediaPrivet, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangeC_NRatio ~ Group, data = MediaPrivet, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangePhosphorus ~ Group, data = MediaPrivet, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangePotassium ~ Group, data = MediaPrivet, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
t.test(ChangeTotalNitrogen ~ Group, data = MediaPrivet, var.equal = FALSE)

summary_Privet <- MediaPrivet %>%
  group_by(Group) %>%
  summarise(mean_ChangePhosphorus = mean(ChangePhosphorus),
            median_ChangePhosphorus = median(ChangePhosphorus),
            IQR_ChangePhosphorus = IQR(ChangePhosphorus),
            sd_ChangePhosphorus = sd(ChangePhosphorus),
            var_ChangePhosphorus = var(ChangePhosphorus),
            se_ChangePhosphorus = sd(ChangePhosphorus)/sqrt(n()),
            n_ChangePhosphorus = n())

print(summary_Privet)

# Woolly
MediaWoolly <- Media %>%
  mutate(Group = case_when(
    Group  %in% c(2, 3, 4, 6) ~ "woolly",
    Group %in% c(1, 5, 7, 8) ~ "no woolly",
    TRUE ~ NA_character_  # Optional: handles other values
  ))

ggplot(MediaWoolly, aes(x = Group, y = ChangeTotalNitrogen, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(MediaWoolly) +
  geom_histogram(aes(ChangeTotalNitrogen), binwidth = 1)+
  facet_wrap(~Group)
ggplot(MediaWoolly)+
  geom_qq(aes(sample = ChangeTotalNitrogen, color = Group)) +
  theme_classic()

wilcox.test(ChangeNitrate ~ Group, data = MediaWoolly, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangeAmmonium ~ Group, data = MediaWoolly, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangeC_NRatio ~ Group, data = MediaWoolly, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangePhosphorus ~ Group, data = MediaWoolly, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangePotassium ~ Group, data = MediaWoolly, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
t.test(ChangeTotalNitrogen ~ Group, data = MediaWoolly, var.equal = FALSE)

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

ggplot(MediaMono, aes(x = Group, y = ChangeTotalNitrogen, group = Group))+
  geom_boxplot() +
  theme_bw() 
ggplot(MediaMono) +
  geom_histogram(aes(ChangeTotalNitrogen), binwidth = 1)+
  facet_wrap(~Group)
ggplot(MediaMono)+
  geom_qq(aes(sample = ChangeTotalNitrogen, color = Group)) +
  theme_classic()

wilcox.test(ChangeNitrate ~ Group, data = MediaMono, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangeAmmonium ~ Group, data = MediaMono, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangeC_NRatio ~ Group, data = MediaMono, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangePhosphorus ~ Group, data = MediaMono, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
wilcox.test(ChangePotassium ~ Group, data = MediaMono, var.equal = TRUE,exact = FALSE, alternative = "less", mu = 0, conf.level = 0.95)
t.test(ChangeTotalNitrogen ~ Group, data = MediaMono, var.equal = FALSE)

summary_Mono <- MediaMono %>%
  group_by(Group) %>%
  summarise(mean_ChangeNitrate = mean(ChangeNitrate),
            median_ChangeNitrate = median(ChangeNitrate),
            IQR_ChangeNitrate = IQR(ChangeNitrate),
            sd_ChangeNitrate = sd(ChangeNitrate),
            var_ChangeNitrate = var(ChangeNitrate),
            se_ChangeNitrate = sd(ChangeNitrate)/sqrt(n()),
            n_ChangeNitrate = n())

print(summary_Mono)
