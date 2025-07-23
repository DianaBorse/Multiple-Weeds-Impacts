#### Biomass and Growth Rate for each plant by treatment group ####


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
Biomass <- read_csv("Shadehouse/Biomass Experiment 1 Clean.csv")

library(dplyr)

# Removing rows where the species is blank
Biomass<-Biomass[-592, ] 
Biomass<-Biomass[-591, ] 
Biomass<-Biomass[-590, ] 
Biomass<-Biomass[-961, ] 
Biomass<-Biomass[-960, ] 
Biomass<-Biomass[-959, ] 

# Removing rows where the mass is blank
Biomass <- Biomass[!is.na(Biomass$Mass_g), ]

# Remove seedling from tg2, there should be no seedlings in that tg
Biomass<-Biomass[-63, ]

# simplifying the experiment 1 treatment group name to Group and Mass_g to Mass
colnames(Biomass)[5:5] <- c("Group") ## Renaming the columns
colnames(Biomass)[10:10] <- c("Mass") ## Renaming the columns
Biomass$Group <- as.factor(Biomass$Group)


# ggfortify is a package that works with ggplot2 to make nice plots
# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

#### Have a look at how sapling varies by treatment group ####
Sapling <- Biomass[Biomass$Plant == "ManukaSapling", ]

Sapling$Group <- as.factor(Sapling$Group)

ggplot(Sapling, aes(x = Group, y = Mass))+
  geom_boxplot() +
  theme_bw() 
ggplot(Sapling) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Sapling)+
  geom_qq(aes(sample = Mass, color = Group))

# Check for Homogeneous variance
summ_Sapling <- Sapling %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_Sapling$sd_Mass))/(min(summ_Sapling$sd_Mass))
print(ratio)

# Not normal, need to try a transformation
Sapling <- Sapling %>%
  mutate(logMass = log(Mass))

# Check for Homogeneous variance
summ_Sapling <- Sapling %>%
  group_by(Group) %>% 
  summarise(mean_logMass = mean(logMass),
            sd_logMass = sd(logMass),
            n_logMass = n())
ratio <-(max(summ_Sapling$sd_logMass))/(min(summ_Sapling$sd_logMass))
print(ratio)

# Still not normal... try square root
Sapling <- Sapling %>%
  mutate(sqrtMass = sqrt(Mass))

# Check for Homogeneous variance
summ_Sapling <- Sapling %>%
  group_by(Group) %>% 
  summarise(mean_sqrtMass = mean(sqrtMass),
            sd_sqrtMass = sd(sqrtMass),
            n_sqrtMass = n())
ratio <-(max(summ_Sapling$sd_sqrtMass))/(min(summ_Sapling$sd_sqrtMass))
print(ratio)

# That is worse, will need to use kruskal-wallis
kruskal.test(logMass ~ Group, data = Sapling)

#### Have a look at how seedling biomass varies by treatment group ####
Seedling <- Biomass[Biomass$Plant == "ManukaSeedling", ]

Seedling<-Seedling[-81, ]

Seedling$Group <- as.factor(Seedling$Group)

ggplot(Seedling, aes(x = Group, y = Mass))+
  geom_boxplot() +
  theme_bw() 
ggplot(Seedling) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Seedling)+
  geom_qq(aes(sample = Mass, color = Group))

# Check for Homogeneous variance
summ_Seedling <- Seedling %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_Seedling$sd_Mass))/(min(summ_Seedling$sd_Mass))
print(ratio)

# Looks a lot better
# Set up for ANOVA
model01 <- aov(Mass~Group, data = Seedling)

autoplot(model01)

anova(model01)

summary(model01)

# Perform unplanned Tukey test
tukey <- glht(model01, linfct = mcp(Group = "Tukey"))
summary(tukey)

# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(Seedling$Group) <- contr.sum(8)

model <- lm(Mass ~ Group, data = Seedling)

# Run Type III ANOVA
library(car)
Anova(model, type = 3)

# Tukey-Kramer test (automatically applied for unequal sample sizes)
TukeyHSD(model01)


# Very interesting, the seedling biomass is significantly higher in groups 4 and 5
# which have wattle and woolly and wattle and privet than when growing with conspecifics only

#### woolly nightshade analysis ####
Nightshade <- Biomass[Biomass$Plant == "Nightshade", ]

Nightshade$Group <- as.factor(Nightshade$Group)

ggplot(Nightshade, aes(x = Group, y = Mass))+
  geom_boxplot() +
  theme_bw() 
ggplot(Nightshade) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Nightshade)+
  geom_qq(aes(sample = Mass, color = Group))

# Check for Homogeneous variance
summ_Nightshade <- Nightshade %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_Nightshade$sd_Mass))/(min(summ_Nightshade$sd_Mass))
print(ratio)

# Set up for ANOVA
model01 <- aov(Mass~Group, data = Nightshade)

autoplot(model01)

anova(model01)

summary(model01)

# Perform Tukey test
tukey <- TukeyHSD(model01)
summary(tukey)
print(tukey)
plot(tukey, las = 1)


# Visualize transformed data
ggplot(Nightshade, aes(x = Group, y = Mass))+
  geom_boxplot() +
  theme_classic() 

# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(Nightshade$Group) <- contr.sum(8)

model <- lm(Mass ~ Group, data = Nightshade)

# Run Type III ANOVA
Anova(model, type = 3)

#### privet analysis ####
Privet <- Biomass[Biomass$Plant == "Privet", ]

Privet$Group <- as.factor(Privet$Group)

ggplot(Privet, aes(x = Group, y = Mass))+
  geom_boxplot() +
  theme_bw() 
ggplot(Privet) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Privet)+
  geom_qq(aes(sample = Mass, color = Group))

# Check for Homogeneous variance
summ_Privet <- Privet %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_Privet$sd_Mass))/(min(summ_Privet$sd_Mass))
print(ratio)

# Set up for ANOVA
model01 <- aov(Mass~Group, data = Privet)

autoplot(model01)

anova(model01)

summary(model01)

# Perform Tukey test
tukey <- TukeyHSD(model01)
summary(tukey)
print(tukey)
plot(tukey, las = 1)

# Visualize transformed data
ggplot(Privet, aes(x = Group, y = Mass))+
  geom_boxplot() +
  theme_classic() 

# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(Privet$Group) <- contr.sum(8)

model <- lm(Mass ~ Group, data = Privet)

# Run Type III ANOVA
Anova(model, type = 3)

#### wattle analysis ####
Wattle <- Biomass[Biomass$Plant == "Wattle", ]

Wattle$Group <- as.factor(Wattle$Group)

ggplot(Wattle, aes(x = Group, y = Mass))+
  geom_boxplot() +
  theme_bw() 
ggplot(Wattle) +
  geom_histogram(aes(Mass), binwidth = 1)+
  facet_wrap(~Group)
ggplot(Wattle)+
  geom_qq(aes(sample = Mass, color = Group))

# Check for Homogeneous variance
summ_Wattle <- Wattle %>%
  group_by(Group) %>% 
  summarise(mean_Mass = mean(Mass),
            sd_Mass = sd(Mass),
            n_Mass = n())
ratio <-(max(summ_Wattle$sd_Mass))/(min(summ_Wattle$sd_Mass))
print(ratio)

# Not normal, need to try a transformation
Wattle <- Wattle %>%
  mutate(logMass = log(Mass))

# Check for Homogeneous variance
summ_Wattle <- Wattle %>%
  group_by(Group) %>% 
  summarise(mean_logMass = mean(logMass),
            sd_logMass = sd(logMass),
            n_logMass = n())
ratio <-(max(summ_Wattle$sd_logMass))/(min(summ_Wattle$sd_logMass))
print(ratio)

# Set up for ANOVA
model01 <- aov(logMass~Group, data = Wattle)

autoplot(model01)

anova(model01)

summary(model01)

# Perform Tukey test
tukey <- TukeyHSD(model01)
summary(tukey)
print(tukey)
plot(tukey, las = 1)

# Visualize transformed data
ggplot(Wattle, aes(x = Group, y = logMass))+
  geom_boxplot() +
  theme_classic() 

# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(Wattle$Group) <- contr.sum(8)

model <- lm(Mass ~ Group, data = Wattle)

# Run Type III ANOVA
Anova(model, type = 3)

# Make a new df without saplings because they are too big
BiomassSeedlings <- Biomass %>% filter(Plant != "ManukaSapling")

# Make a boxplot with all values on it
ggplot(BiomassSeedlings, aes(x = Group, y = Mass, fill = Plant)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Treatment group", y = "Dry biomass g", fill = "Species") +
  scale_fill_manual(values = c(
    "ManukaSapling" = "#33B08D",  
    "ManukaSeedling"  = "#82C782", 
    "Nightshade" = "#CF597E",  
    "Privet" = "#E57F6C" , 
    "Wattle" = "#E9A96C"),
    labels = c("ManukaSapling" = "L. scoparium sapling", "ManukaSeedling" = "L. scoparium seedling","Nightshade" = "S. mauritianum",
               "Privet" = "L. lucidum", "Wattle" = "P. lophantha")) +
  theme_classic()



#### Growth Rate Analysis ####
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

# Calculating growth rate
Height <- Height %>%
  mutate(GR1 = (log(Height2) - log(Height1)))
Height <- Height %>%
  mutate(GR2 = (log(Height3) - log(Height2)))
Height <- Height %>%
  mutate(GR3 = (log(Height4) - log(Height3)))
Height <- Height %>%
  mutate(GR4 = (log(Height5) - log(Height4)))
Height <- Height %>%
  mutate(GR5 = (log(Height6) - log(Height5)))
Height <- Height %>%
  mutate(GR6 = (log(Height7) - log(Height6)))

# Calculate average growth rate for each plant
Height$AverageGR <- rowMeans(Height[, 26:31], na.rm = TRUE)
Height$AverageGR <- Height$AverageGR * 10

# Clean up empty rows
library(dplyr)

Height <- Height %>% 
  filter(!is.na(AverageGR))

Height <- Height %>% 
  filter(!is.na(Group))

#### Let's look at Sapling height ####
SaplingH <- Height[Height$Plant == "ManukaSapling", ]

ggplot(SaplingH, aes(x = Group, y = AverageGR))+
  geom_boxplot() +
  theme_bw() 
ggplot(SaplingH) +
  geom_histogram(aes(AverageGR), binwidth = 1)+
  facet_wrap(~Group)
ggplot(SaplingH)+
  geom_qq(aes(sample = AverageGR, color = Group))

# Check for Homogeneous variance
summ_SaplingH <- SaplingH %>%
  group_by(Group) %>% 
  summarise(mean_AverageGR = mean(AverageGR),
            sd_AverageGR = sd(AverageGR),
            n_AverageGR = n())
ratio <-(max(summ_SaplingH$sd_AverageGR))/(min(summ_SaplingH$sd_AverageGR))
print(ratio)

# Normal enough <3

# Set up for ANOVA
model01 <- aov(AverageGR~Group, data = SaplingH)

autoplot(model01)

anova(model01)

summary(model01)

# Perform unplanned Tukey test
tukey <- TukeyHSD(model01)
summary(tukey)
print(tukey)
plot(tukey, las = 1)

# No difference in the average sapling Growth Rate by group

# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(SaplingH$Group) <- contr.sum(8)

model <- lm(AverageGR ~ Group, data = SaplingH)

# Run Type III ANOVA
Anova(model, type = 3)

# Still not significant

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


#### Survival Analysis ####

# Use Kaplan-Meier survival curves

library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)

# I need to calculate survival time, that will be how many months until one of the
# notes columns says 'dead'

# So then if Notes2 = dead, survival time = 33 days, if notes5 = dead, survival time = 130 days etc.
# So then I need to make some code to make a survival time column that follows these rules


# Then I need to make a status column that just gives 0 if the plants never died
# and 1 if the plants died.

# Make a new data frame for survival time in days
library(dplyr)

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

Height <- Height %>%
  mutate(time = case_when(
    Notes2 == "dead" ~ 33,
    Notes3 == "dead" ~ 57,
    Notes4 == "dead" ~ 99,
    Notes5 == "dead" ~ 130,
    Notes6 == "dead" ~ 159,
    Notes7 == "dead" ~ 190,
    TRUE ~ 190
  ))

# Now the time variable should be the number of days a plant survived for. 
Height$status <- ifelse(Height$Notes7 == "dead", 1, 0)
Height$status[is.na(Height$Notes7)] <- 0

# Start by looking at just saplings
SaplingS <- Height[Height$Plant == "ManukaSapling", ]


# Code from co-pilot
# Load required packages
library(survival)
library(survminer)
library(knitr)
library(dplyr)
library(survival)
library(ggplot2)
library(tibble)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)

# Create survival object
surv_obj <- Surv(time = SaplingS$time, event = SaplingS$status)

# Fit Kaplan-Meier curves
fit <- survfit(surv_obj ~ Group, data = SaplingS)

# Plot the curves
ggsurvplot(fit, data = SaplingS, pval = TRUE, risk.table = TRUE, xlim = c(0, 200)) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) 

# logrank test
survdiff(Surv(time, status) ~ Group, data = SaplingS)
?sruvdiff

## Stratified 8-sample test (7 df)
survdiff(Surv(time, status) ~ Group, data=SaplingS)
check <- coxph(Surv(time, status) ~ factor(Group), SaplingS)
round(summary(check)$sctest, 3)

# Can I do an ANOVA for survival? 
# See if time differs by group, need to account for different group sample sizes
# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(SaplingS$Group) <- contr.sum(8)

model <- lm(time ~ Group, data = SaplingS)

# Run Type III ANOVA
Anova(model, type = 3)

# tukey-kramer test
model01 <- aov(time~Group, data = SaplingS)
tukey <- TukeyHSD(model01)
summary(tukey)
print(tukey)

# Not sure how useful this is...

#### Seedling survival analysis ####

SeedlingS <- Height[Height$Plant == "ManukaSeedling", ]

# Create survival object
surv_obj <- Surv(time = SeedlingS$time, event = SeedlingS$status)

# Fit Kaplan-Meier curves
fit <- survfit(surv_obj ~ Group, data = SeedlingS)

# Plot the curves
ggsurvplot(fit, data = SeedlingS, pval = TRUE, risk.table = TRUE, xlim = c(0, 200)) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) 

survdiff(Surv(time, status) ~ Group, data = SeedlingS)

## Stratified 8-sample test (7 df)
survdiff(Surv(time, status) ~ Group, data=SeedlingS)
check <- coxph(Surv(time, status) ~ factor(Group), SeedlingS)
round(summary(check)$sctest, 3)

# Looks like there weren't survival differences between groups for the seedlings

#### Nightshade Survival ####
NightshadeS <- Height[Height$Plant == "Nightshade", ]

# Create survival object
surv_obj <- Surv(time = NightshadeS$time, event = NightshadeS$status)

# Fit Kaplan-Meier curves
fit <- survfit(surv_obj ~ Group, data = NightshadeS)

# Plot the curves
ggsurvplot(fit, data = NightshadeS, pval = TRUE, risk.table = TRUE, xlim = c(0, 200)) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) 

survdiff(Surv(time, status) ~ Group, data = NightshadeS)

## Stratified 8-sample test (7 df)
survdiff(Surv(time, status) ~ Group, data=NightshadeS)
check <- coxph(Surv(time, status) ~ factor(Group), NightshadeS)
round(summary(check)$sctest, 3)

# No difference for Nightshade either

#### Privet Survival ####

PrivetS <- Height[Height$Plant == "Privet", ]

# Create survival object
surv_obj <- Surv(time = PrivetS$time, event = PrivetS$status)

# Fit Kaplan-Meier curves
fit <- survfit(surv_obj ~ Group, data = PrivetS)

# Plot the curves
ggsurvplot(fit, data = PrivetS, pval = TRUE, risk.table = TRUE, xlim = c(0, 200)) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) 

survdiff(Surv(time, status) ~ Group, data = PrivetS)

## Stratified 8-sample test (7 df)
survdiff(Surv(time, status) ~ Group, data=PrivetS)
check <- coxph(Surv(time, status) ~ factor(Group), SeedlingS)
round(summary(check)$sctest, 3)

# Not many privets died

#### Wattle Analysis ####
WattleS <- Height[Height$Plant == "Wattle", ]

# Create survival object
surv_obj <- Surv(time = WattleS$time, event = WattleS$status)

# Fit Kaplan-Meier curves
fit <- survfit(surv_obj ~ Group, data = WattleS)

# Plot the curves
ggsurvplot(fit, data = WattleS, pval = TRUE, risk.table = TRUE, xlim = c(0, 200)) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) 

survdiff(Surv(time, status) ~ Group, data = WattleS)

## Stratified 8-sample test (7 df)
survdiff(Surv(time, status) ~ Group, data=WattleS)
check <- coxph(Surv(time, status) ~ factor(Group), WattleS)
round(summary(check)$sctest, 3)

# Not significant
