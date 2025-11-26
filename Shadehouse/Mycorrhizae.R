#### Mycorrhizae Analysis Experiment 1 ####

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
Myc <- read_csv("Shadehouse/MycorrhizaeDataClean.csv")

# Now we need to make a distinct name for each subsample of each pot
library(tidyr)
Myc2 <- Myc %>%
  unite(Sample, Subsample, Sample, sep = "-")


Myc_summary <- Myc2 %>%
  group_by(Sample) %>%
  summarise(
    perc_c = 100 * sum(Intersection[None == "no"], na.rm = TRUE) /
      sum(Intersection, na.rm = TRUE))

# Add Exp1 group to this data frame
Myc3 <- Myc_summary %>%
  left_join(Myc2 %>% dplyr::select(Sample, Exp1Group), by = "Sample")

# Remove if treatment group = NA
Myc3 <- Myc3 %>%
  filter(!is.na(Exp1Group))

# Let's make a boxplot
library(ggplot2)
MycPlot <- ggplot(data = Myc3, 
                  aes(x = factor(Exp1Group), y = perc_c)) +
  geom_boxplot(fill = "lightblue3", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.6, width = 0.2) +
  ylab("Percent mycorrhizal colonisation") +
  xlab("Treatment Group") +
  theme(axis.text.x = element_text(size = 10, color = 'black'),
        axis.text.y = element_text(size = 15, hjust = 1, colour = 'black'), 
        axis.title = element_text(size = 17, face = "bold"),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
MycPlot

# Let's try some analysis
# Check for Homogeneous variance
summ_Myc3 <- Myc3 %>%
  group_by(Exp1Group) %>% 
  summarise(mean_perc_c = mean(perc_c),
            sd_perc_c = sd(perc_c),
            n_perc_c = n())
ratio <-(max(summ_Myc3$sd_perc_c))/(min(summ_Myc3$sd_perc_c))
print(ratio)

# Looks a lot better
# Set up for ANOVA
model01 <- aov(perc_c~Exp1Group, data = Myc3)
autoplot(model01)
anova(model01)
summary(model01)

# The sample sizes are not quite even and therefore we need to use Type 3 analysis
Myc3$Exp1Group <- as.factor(Myc3$Exp1Group)

contrasts(Myc3$Exp1Group) <- contr.sum(9)

model <- lm(perc_c ~ factor(Exp1Group), data = Myc3)

# Run Type III ANOVA
library(car)
Anova(model, type = 3)

# Tukey-Kramer test (automatically applied for unequal sample sizes)
library(emmeans)
emm <- emmeans(model, ~ Exp1Group)        # treatment effects within each room
pairs(emm, adjust = "tukey")
