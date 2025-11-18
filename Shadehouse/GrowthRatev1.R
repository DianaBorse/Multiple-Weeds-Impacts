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

#### Seedling analysis ####

SeedlingH <- Height[Height$Plant == "ManukaSeedling", ]

ggplot(SeedlingH, aes(x = Group, y = AverageGR))+
  geom_boxplot() +
  theme_bw() 
ggplot(SeedlingH) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~Group)
ggplot(SeedlingH)+
  geom_qq(aes(sample = AverageGR, color = Group))

# Check for Homogeneous variance
summ_SeedlingH <- SeedlingH %>%
  group_by(Group) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SeedlingH$sd_AverageGR))/(min(summ_SeedlingH$sd_AverageGR))
print(ratio)

# Normal, do and ANOVA
model01 <- aov(AverageGR~Group, data = SeedlingH)

autoplot(model01)

anova(model01)

summary(model01)

# no difference

# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(SeedlingH$Group) <- contr.sum(8)

model <- lm(AverageGR ~ Group, data = SeedlingH)

# Run Type III ANOVA
Anova(model, type = 3)

# Still not significant

#### Woolly nightshade analysis ####
NightshadeH <- Height[Height$Plant == "Nightshade", ]

ggplot(NightshadeH, aes(x = Group, y = AverageGR))+
  geom_boxplot() +
  theme_bw() 
ggplot(NightshadeH) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~Group)
ggplot(NightshadeH)+
  geom_qq(aes(sample = AverageGR, color = Group))

# Check for Homogeneous variance
summ_NightshadeH <- NightshadeH %>%
  group_by(Group) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_NightshadeH$sd_AverageGR))/(min(summ_NightshadeH$sd_AverageGR))
print(ratio)

# Normal enough, <3
# Set up for ANOVA
model01 <- aov(AverageGR~Group, data = NightshadeH)

autoplot(model01)

anova(model01)

summary(model01)

# Perform planned Tukey test
tukey <- TukeyHSD(model01)
summary(tukey)
print(tukey)
plot(tukey, las = 1)

# calculate summary stats
summ_NightshadeH <- NightshadeH %>%
  group_by(Group) %>%
  summarise(mean_AverageGR = mean(AverageGR),
            median_AverageGR = median(AverageGR),
            IQR_AverageGR = IQR(AverageGR),
            sd_AverageGR = sd(AverageGR),
            var_AverageGR = var(AverageGR),
            se_AverageGR = sd(AverageGR)/sqrt(189))
print(summ_NightshadeH)

# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(NightshadeH$Group) <- contr.sum(8)

model <- lm(AverageGR ~ Group, data = NightshadeH)

# Run Type III ANOVA
Anova(model, type = 3)

#### Privet Analysis ####
PrivetH <- Height[Height$Plant == "Privet", ]

ggplot(PrivetH, aes(x = Group, y = AverageGR))+
  geom_boxplot() +
  theme_bw() 
ggplot(PrivetH) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~Group)
ggplot(PrivetH)+
  geom_qq(aes(sample = AverageGR, color = Group))

# Check for Homogeneous variance
summ_PrivetH <- PrivetH %>%
  group_by(Group) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_PrivetH$sd_AverageGR))/(min(summ_PrivetH$sd_AverageGR))
print(ratio)

# Normal enough, <3
# Set up for ANOVA
model01 <- aov(AverageGR~Group, data = PrivetH)

autoplot(model01)

anova(model01)

summary(model01)

# Perform unplanned Tukey test
tukey <- TukeyHSD(model01)
summary(tukey)
print(tukey)
plot(tukey, las = 1)

# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(PrivetH$Group) <- contr.sum(8)

model <- lm(AverageGR ~ Group, data = PrivetH)

# Run Type III ANOVA
Anova(model, type = 3)

# calculate summary stats
summ_PrivetH <- PrivetH %>%
  group_by(Group) %>%
  summarise(mean_AverageGR = mean(AverageGR),
            median_AverageGR = median(AverageGR),
            IQR_AverageGR = IQR(AverageGR),
            sd_AverageGR = sd(AverageGR),
            var_AverageGR = var(AverageGR),
            se_AverageGR = sd(AverageGR)/sqrt(189))
print(summ_PrivetH)

#### Wattle Analysis ####
WattleH <- Height[Height$Plant == "Wattle", ]

ggplot(WattleH, aes(x = Group, y = AverageGR))+
  geom_boxplot() +
  theme_bw() 
ggplot(WattleH) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~Group)
ggplot(WattleH)+
  geom_qq(aes(sample = AverageGR, color = Group))

# Check for Homogeneous variance
summ_WattleH <- WattleH %>%
  group_by(Group) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_WattleH$sd_AverageGR))/(min(summ_WattleH$sd_AverageGR))
print(ratio)

# Normal enough, <3
# Set up for ANOVA
model01 <- aov(AverageGR~Group, data = WattleH)

autoplot(model01)

anova(model01)

summary(model01)

# Perform unplanned Tukey test
tukey <- TukeyHSD(model01)
summary(tukey)
print(tukey)
plot(tukey, las = 1)

# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(WattleH$Group) <- contr.sum(8)

model <- lm(AverageGR ~ Group, data = WattleH)

# Run Type III ANOVA
Anova(model, type = 3)


library(FSA)

# Dunn's test with Bonferroni correction
dunnTest(AverageGR ~ Group, data = WattleH, method = "bonferroni")


#### Nice Box plots ####

library(ggplot2)
WattlePlot <- ggplot(data = WattleH, 
                     aes(y = AverageGR, ##Change this to variable name
                         x = Group)) + ##Change this to variable name
  geom_boxplot(fill = "#E9A96C", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Relative growth rate ln(mm/month)") + xlab("Treatment Group") +   ##Change axis titles
  theme(axis.text.x=element_text(size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
WattlePlot

NightshadePlot <- ggplot(data = NightshadeH, 
                         aes(y = AverageGR, ##Change this to variable name
                             x = Group)) + ##Change this to variable name
  geom_boxplot(fill = "#CF597E", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Relative growth rate ln(mm/month)h") + xlab("Treatment Group") +   ##Change axis titles
  theme(axis.text.x=element_text(size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
NightshadePlot

PrivetPlot <- ggplot(data = PrivetH, 
                     aes(y = AverageGR, ##Change this to variable name
                         x = Group)) + ##Change this to variable name
  geom_boxplot(fill = "#E57F6C", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Relative growth rate ln(mm/month)") + xlab("Treatment Group") +   ##Change axis titles
  theme(axis.text.x=element_text(size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
PrivetPlot 

SaplingPlot <- ggplot(data = SaplingH, 
                      aes(y = AverageGR, ##Change this to variable name
                          x = Group)) + ##Change this to variable name
  geom_boxplot(fill = "#33B08D", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Relative growth rate ln(mm/month)") + xlab("Treatment Group") +   ##Change axis titles
  theme(axis.text.x=element_text(size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
SaplingPlot 

SeedlingPlot <- ggplot(data = SeedlingH, 
                       aes(y = AverageGR, ##Change this to variable name
                           x = Group)) + ##Change this to variable name
  geom_boxplot(fill = "#82C782", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Relative growth rate ln(mm/month)") + xlab("Treatment Group") +   ##Change axis titles
  theme(axis.text.x=element_text(size=10, color = 'black'), #Change axis text font size and angle and colour etc
        axis.text.y=element_text(size=15, hjust = 1, colour = 'black'), 
        axis.title=element_text(size=17,face="bold"), #Change axis title text font etc
        legend.title = element_blank(), #If you want to remove the legend
        legend.position = "none",
        panel.grid.major = element_blank(),#If you want to remove gridlines
        panel.grid.minor = element_blank(),#If you want to remove gridlines
        panel.background = element_blank(),    #If you want to remove background
        axis.line = element_line(colour = "black"))   ##If you want to add an axis colour
SeedlingPlot 

# Make a boxplot with all values on it
ggplot(Height, aes(x = Group, y = AverageGR, fill = Plant)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Treatment group", y = "Relative growth rate ln(mm/month)", fill = "Species") +
  scale_fill_manual(values = c(
    "ManukaSapling" = "#33B08D",  
    "ManukaSeedling"  = "#82C782", 
    "Nightshade" = "#CF597E",  
    "Privet" = "#E57F6C" , 
    "Wattle" = "#E9A96C"),
    labels = c("ManukaSapling" = "L. scoparium sapling", "ManukaSeedling" = "L. scoparium seedling","Nightshade" = "S. mauritianum",
               "Privet" = "L. lucidum", "Wattle" = "P. lophantha")) +
  theme_classic()

hcl.colors(8, palette = "Temps") # remove # to look at colors below
# "#089392" "#33B08D" "#82C782" "#CBD98E" "#EACF87" "#E9A96C" "#E57F6C" "#CF597E"

# Calculate averages for each group
# calculate summary stats
summ_Height <- Height %>%
  group_by(Plant) %>%
  summarise(mean_AverageGR = mean(AverageGR),
            median_AverageGR = median(AverageGR),
            IQR_AverageGR = IQR(AverageGR),
            sd_AverageGR = sd(AverageGR),
            var_AverageGR = var(AverageGR),
            se_AverageGR = sd(AverageGR)/sqrt(189))
print(summ_Height)