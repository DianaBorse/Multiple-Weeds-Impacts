#### Clean Sapling survival ####


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

# Fix up treatment group names
library(dplyr) 
SaplingS$Group <- factor(SaplingS$Group, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), # order  
                         labels = c("m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 

# Fit Kaplan-Meier curves
fit <- survfit(surv_obj ~ Group, data = SaplingS)

# Plot the curves (labels go inside ggsurvplot)
ggsurvplot(
  fit,
  data = SaplingS,
  pval = TRUE,
  risk.table = TRUE,
  xlim = c(0, 200),
  xlab = "Days",
  ylab = "Overall survival probability"
)
plot_obj <- ggsurvplot(fit, data = SaplingS, pval = TRUE, risk.table = TRUE, xlim = c(0, 200))
plot_obj$plot 

# logrank test
survdiff(Surv(time, status) ~ Group, data = SaplingS)

# look for which treatments have different survival curves
PairwiseSapling <- pairwise_survdiff(
  Surv(time, status) ~ Group,
  data = SaplingS,
  p.adjust.method = "BH"   # or "bonferroni", "holm", etc.
)

readable_pw <- PairwiseSapling$p.value %>%
  as.data.frame() %>%
  mutate(Group1 = rownames(.)) %>%
  pivot_longer(
    cols = -Group1,
    names_to = "Group2",
    values_to = "p_value"
  ) %>%
  filter(!is.na(p_value)) %>%
  arrange(Group1, Group2)

## Stratified 8-sample test (7 df)
survdiff(Surv(time, status) ~ Group, data=SaplingS)
check <- coxph(Surv(time, status) ~ factor(Group), SaplingS)
round(summary(check)$sctest, 3)

# Can I do an ANOVA for survival? 

# Add room
library(readr)
RoomPot <- read_csv("Shadehouse/RoomPot.csv")

colnames(RoomPot)[1:1] <- c("Pot") ## Renaming the columns
SaplingS <- SaplingS %>%
  left_join(RoomPot %>% dplyr::select(Pot, Room), by = "Pot")
# See if time differs by group, need to account for different group sample sizes
# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(SaplingS$Group) <- contr.sum(8)

hist(SaplingS$time)

# mixed effects model for survival, just to account for room
library(coxme)

fit <- coxme(Surv(time, status) ~ Group + (1|Room), data = SaplingS)

summary(fit)

# what % of mānuka were alive at the end?
SaplingS <- SaplingS %>% 
  filter(!is.na(status))

percent_status1 <- SaplingS %>%
  summarise(percent = mean(status == "0") * 100) %>%
  pull(percent)

#for each treatment?
summary_stats <- SaplingS %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    n1 = sum(status == "0"),
    prop = n1 / n,
    percent = prop * 100,
    se = sqrt(prop * (1 - prop) / n)
  )

# summary_stats <- as.data.frame(summary_stats)
# library(writexl)
# 
# write_xlsx(summary_stats, "C:/Users/bella/Documents/SaplingSurvivalAverages.xlsx")

# Brush wattle survival
#### Wattle Analysis ####
WattleS <- Height[Height$Plant == "Wattle", ]
WattleS$Group <- factor(WattleS$Group, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), # order  
                        labels = c("m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 

# Create survival object
surv_obj <- Surv(time = WattleS$time, event = WattleS$status)

# Fit Kaplan-Meier curves
fit <- survfit(surv_obj ~ Group, data = WattleS)

# Plot the curves (labels go inside ggsurvplot)
ggsurvplot(
  fit,
  data = SaplingS,
  pval = TRUE,
  risk.table = TRUE,
  xlim = c(0, 200),
  xlab = "Days",
  ylab = "Overall survival probability"
)
plot_obj <- ggsurvplot(fit, data = WattleS, pval = TRUE, risk.table = TRUE, xlim = c(0, 200))
plot_obj$plot 

## Stratified 8-sample test (7 df)
survdiff(Surv(time, status) ~ Group, data=WattleS)
check <- coxph(Surv(time, status) ~ factor(Group), WattleS)
round(summary(check)$sctest, 3)

# logrank test
survdiff(Surv(time, status) ~ Group, data = WattleS)

# look for which treatments have different survival curves
PairwiseWattle <- pairwise_survdiff(
  Surv(time, status) ~ Group,
  data = WattleS,
  p.adjust.method = "BH"   # or "bonferroni", "holm", etc.
)

readable_pw <- PairwiseWattle$p.value %>%
  as.data.frame() %>%
  mutate(Group1 = rownames(.)) %>%
  pivot_longer(
    cols = -Group1,
    names_to = "Group2",
    values_to = "p_value"
  ) %>%
  filter(!is.na(p_value)) %>%
  arrange(Group1, Group2)

## Stratified 8-sample test (7 df)
survdiff(Surv(time, status) ~ Group, data=SaplingS)
check <- coxph(Surv(time, status) ~ factor(Group), SaplingS)
round(summary(check)$sctest, 3)

# Can I do an ANOVA for survival? 

# Add room
library(readr)
RoomPot <- read_csv("Shadehouse/RoomPot.csv")

colnames(RoomPot)[1:1] <- c("Pot") ## Renaming the columns
SaplingS <- SaplingS %>%
  left_join(RoomPot %>% dplyr::select(Pot, Room), by = "Pot")
# See if time differs by group, need to account for different group sample sizes
# The sample sizes are not quite even and therefore we need to use Type 3 analysis
contrasts(SaplingS$Group) <- contr.sum(8)

hist(SaplingS$time)

# mixed effects model for survival, just to account for room
library(coxme)

fit <- coxme(Surv(time, status) ~ Group + (1|Room), data = SaplingS)

summary(fit)

