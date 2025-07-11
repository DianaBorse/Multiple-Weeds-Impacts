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

# simplifying the experiment 1 treatment group name to Group and Mass_g to Mass
colnames(Biomass)[5:5] <- c("Group") ## Renaming the columns
colnames(Biomass)[10:10] <- c("Mass") ## Renaming the columns


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

# Not normal, need to try a transformation
Seedling <- Seedling %>%
  mutate(logMass = log(Mass))

# Check for Homogeneous variance
summ_Seedling <- Seedling %>%
  group_by(Group) %>% 
  summarise(mean_logMass = mean(logMass),
            sd_logMass = sd(logMass),
            n_logMass = n())
ratio <-(max(summ_Seedling$sd_logMass))/(min(summ_Seedling$sd_logMass))
print(ratio)

# Looks a lot better
# Set up for ANOVA
model01 <- lm(logMass~Group, data = Seedling)

autoplot(model01)

anova(model01)

summary(model01)

# Perform unplanned Tukey test
tukey <- glht(model01, linfct = mcp(Group = "Tukey"))
summary(tukey)

# Visualize transformed data
ggplot(Seedling, aes(x = Group, y = logMass))+
  geom_boxplot() +
  theme_classic() 

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
model01 <- lm(Mass~Group, data = Nightshade)

autoplot(model01)

anova(model01)

summary(model01)

# Perform unplanned Tukey test
tukey <- glht(model01, linfct = mcp(Group = "Tukey"))
summary(tukey)

# Visualize transformed data
ggplot(Nightshade, aes(x = Group, y = Mass))+
  geom_boxplot() +
  theme_classic() 

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
model01 <- lm(Mass~Group, data = Privet)

autoplot(model01)

anova(model01)

summary(model01)

# Perform unplanned Tukey test
tukey <- glht(model01, linfct = mcp(Group = "Tukey"))
summary(tukey)

# Visualize transformed data
ggplot(Privet, aes(x = Group, y = Mass))+
  geom_boxplot() +
  theme_classic() 

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
model01 <- lm(logMass~Group, data = Wattle)

autoplot(model01)

anova(model01)

summary(model01)

# Perform unplanned Tukey test
tukey <- glht(model01, linfct = mcp(Group = "Tukey"))
summary(tukey)


# Visualize transformed data
ggplot(Wattle, aes(x = Group, y = logMass))+
  geom_boxplot() +
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

# Calculating growth rate
Height <- Height %>%
  mutate(GR1 = Height2 - Height1)
Height <- Height %>%
  mutate(GR2 = Height3 - Height2)
Height <- Height %>%
  mutate(GR3 = Height4 - Height3)
Height <- Height %>%
  mutate(GR4 = Height5 - Height4)
Height <- Height %>%
  mutate(GR5 = Height6 - Height5)
Height <- Height %>%
  mutate(GR6 = Height7 - Height6)

# Calculate average growth rate for each plant
Height$AverageGR <- rowMeans(Height[, 26:31], na.rm = TRUE)

# Clean up empty rows
Height<-Height[-592, ] 
Height<-Height[-591, ] 
Height<-Height[-590, ] 
Height<-Height[-589, ]
Height<-Height[-100, ]
Height<-Height[-99, ]
Height<-Height[-98, ]
Height<-Height[-97, ]
Height<-Height[-981, ]
Height<-Height[-957, ]
Height<-Height[-956, ]
Height<-Height[-955, ]
Height<-Height[-954, ]
Height<-Height[-641, ]
Height<-Height[-548, ]


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
model01 <- lm(AverageGR~Group, data = SaplingH)

autoplot(model01)

anova(model01)

summary(model01)

# Perform unplanned Tukey test
tukey <- glht(model01, linfct = mcp(Group = "Tukey"))
summary(tukey)

# No difference in the average sapling Growth Rate by group

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

# Not normal, but too many zeros, try kruskal-wallis
kruskal.test(AverageGR ~ Group, data = SeedlingH)

# no difference

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
model01 <- lm(AverageGR~Group, data = NightshadeH)

autoplot(model01)

anova(model01)

summary(model01)

# There is a significant difference, tukey test
model01 <- lm(AverageGR~Group, data = NightshadeH)

# Perform unplanned Tukey test
tukey <- glht(model01, linfct = mcp(Group = "Tukey"))
summary(tukey)

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
model01 <- lm(AverageGR~Group, data = PrivetH)

autoplot(model01)

anova(model01)

summary(model01)

# There is a significant difference, tukey test
model01 <- lm(AverageGR~Group, data = PrivetH)

# Perform unplanned Tukey test
tukey <- glht(model01, linfct = mcp(Group = "Tukey"))
summary(tukey)

#### Wattle Analysis ####
WattleH <- Height[Height$Plant == "Wattle", ]

WattleH<-WattleH[-20, ]

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
model01 <- lm(AverageGR~Group, data = WattleH)

autoplot(model01)

anova(model01)

summary(model01)

# There is a significant difference, tukey test
model01 <- lm(AverageGR~Group, data = WattleH)

# Perform unplanned Tukey test
tukey <- glht(model01, linfct = mcp(Group = "Tukey"))
summary(tukey)

library(ggplot2)
WattlePlot <- ggplot(data = WattleH, 
                       aes(y = AverageGR, ##Change this to variable name
                           x = Group)) + ##Change this to variable name
  geom_boxplot(fill = "#332288", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Average Growth Rate") + xlab("Treatment Group") +   ##Change axis titles
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
  geom_boxplot(fill = "#882265", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Average Growth Rate") + xlab("Treatment Group") +   ##Change axis titles
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
  geom_boxplot(fill = "#88CCEE", notch = TRUE, varwidth = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ylab("Average Growth Rate") + xlab("Treatment Group") +   ##Change axis titles
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

