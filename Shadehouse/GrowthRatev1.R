#### Relative Growth Rate for each plant by treatment group v2####


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

#### Growth Rate ####
library(readr)
Height <- read_csv("Shadehouse/Height Experiment 1 Clean.csv")

# simplifying the experiment 1 treatment group name to Group and Mass_g to Mass
colnames(Height)[4:4] <- c("Group") ## Renaming the columns
Height$Group <- as.factor(Height$Group)

Height$Height1 <- as.numeric(Height$Height1)
Height$Height2 <- as.numeric(Height$Height2)
Height$Height3 <- as.numeric(Height$Height3)
Height$Height4 <- as.numeric(Height$Height4)
Height$Height5 <- as.numeric(Height$Height5)
Height$Height6 <- as.numeric(Height$Height6)
Height$Height7 <- as.numeric(Height$Height7)

# Remove rows with seedlings in treatment group 2
Height<-Height[-610, ]
Height<-Height[-82, ]

# clean up notes
Height$Notes2[Height$Notes2 == "dead, fell out"] <- "dead"
Height$Notes3[Height$Notes3 == "dead, fell out"] <- "dead"
Height$Notes4[Height$Notes4 == "dead, fell out"] <- "dead"
Height$Notes5[Height$Notes5 == "dead, fell out"] <- "dead"
Height$Notes6[Height$Notes6 == "dead, fell out"] <- "dead"
Height$Notes7[Height$Notes7 == "dead, fell out"] <- "dead"

Height$Notes2[Height$Notes2 == "dead, pulled out"] <- "dead"
Height$Notes3[Height$Notes3 == "dead, pulled out"] <- "dead"
Height$Notes4[Height$Notes4 == "dead, pulled out"] <- "dead"
Height$Notes5[Height$Notes5 == "dead, pulled out"] <- "dead"
Height$Notes6[Height$Notes6 == "dead, pulled out"] <- "dead"
Height$Notes7[Height$Notes7 == "dead, pulled out"] <- "dead"

Height$Notes2[Height$Notes2 == "dead, broken"] <- "dead"
Height$Notes3[Height$Notes3 == "dead, broken"] <- "dead"
Height$Notes4[Height$Notes4 == "dead, broken"] <- "dead"
Height$Notes5[Height$Notes5 == "dead, broken"] <- "dead"
Height$Notes6[Height$Notes6 == "dead, broken"] <- "dead"
Height$Notes7[Height$Notes7 == "dead, broken"] <- "dead"

Height$Notes2[Height$Notes2 == "dead and broken"] <- "dead"
Height$Notes3[Height$Notes3 == "dead and broken"] <- "dead"
Height$Notes4[Height$Notes4 == "dead and broken"] <- "dead"
Height$Notes5[Height$Notes5 == "dead and broken"] <- "dead"
Height$Notes6[Height$Notes6 == "dead and broken"] <- "dead"
Height$Notes7[Height$Notes7 == "dead and broken"] <- "dead"


# Remove height measurements if notes = dead for first GR

Height <- Height %>%
  mutate(GR1 = if_else(is.na(Notes2) | Notes2 != "dead", log(Height2) - log(Height1), NA_real_))
Height <- Height %>%
  mutate(GR2 = if_else(is.na(Notes3) | Notes3 != "dead", log(Height3) - log(Height2), NA_real_))
Height <- Height %>%
  mutate(GR3 = if_else(is.na(Notes4) | Notes4 != "dead", log(Height4) - log(Height3), NA_real_))
Height <- Height %>%
  mutate(GR4 = if_else(is.na(Notes5) | Notes5 != "dead", log(Height5) - log(Height4), NA_real_))
Height <- Height %>%
  mutate(GR5 = if_else(is.na(Notes6) | Notes6 != "dead", log(Height6) - log(Height5), NA_real_))
Height <- Height %>%
  mutate(GR6 = if_else(is.na(Notes7) | Notes7 != "dead", log(Height7) - log(Height6), NA_real_))


# Calculate average growth rate for each plant
Height$AverageGR <- rowMeans(Height[, 26:31], na.rm = TRUE)
Height$AverageGR <- Height$AverageGR * 10

# Clean up empty rows
library(dplyr)

Height <- Height %>% 
  filter(!is.na(AverageGR))

Height <- Height %>% 
  filter(!is.na(Group))

view(Height)

#### Let's look at Sapling height ####
SaplingH <- Height[Height$Plant == "ManukaSapling", ]

ggplot(SaplingH,aes(x = Group, y = AverageGR))+
  geom_boxplot(fill = "lightgreen", varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Sapling RGR") + xlab("Treatment Group") +  
  theme_bw() 
ggplot(SaplingH) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~Group)
ggplot(SaplingH)+
  geom_qq(aes(sample = AverageGR, color = Group))

# Check for homogeneous variance
summ_SaplingH <- SaplingH %>%
  group_by(Group) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingH$sd_AverageGR))/(min(summ_SaplingH$sd_AverageGR))
print(ratio)

# ratio > 3
# Use kruskal-wallis
kruskal.test(AverageGR ~ Group, data = SaplingH)
