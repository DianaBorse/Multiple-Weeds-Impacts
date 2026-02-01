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

# Add room
library(readr)
RoomPot <- read_csv("Shadehouse/RoomPot.csv")

colnames(RoomPot)[1:1] <- c("Pot") ## Renaming the columns
colnames(Myc)[3:3] <- c("Pot") ## Renaming the columns
Myc <- Myc %>%
  left_join(RoomPot %>% dplyr::select(Pot, Room), by = "Pot")

# Now we need to make a distinct name for each subsample of each pot
colnames(Myc)[3:3] <- c("Sample") ## Renaming the columns

library(tidyr)
Myc2 <- Myc %>%
  unite(Sample, Subsample, Sample, sep = "-")


Myc_summary1 <- Myc2 %>%
  group_by(Sample) %>%
  summarise(
    perc_c = 100 * sum(Intersection[None == "no"], na.rm = TRUE) /
      sum(Intersection, na.rm = TRUE))

Myc_summary2 <- Myc2 %>%
  group_by(Sample) %>%
  summarise(
    perc_A = 100 * sum(Intersection[Arbuscules == "1"], na.rm = TRUE) /
      sum(Intersection, na.rm = TRUE))

# Add Exp1 group to this data frame
Myc3 <- Myc_summary1 %>%
  left_join(Myc2 %>% dplyr::select(Sample, Exp1Group), by = "Sample")

# add room back
Myc3 <- Myc3 %>%
  left_join(
    Myc2 %>% 
      distinct(Sample, Room),   # keep one row per Sample–Room pair
    by = "Sample"
  )

# Add Exp1 group to this data frame
Myc4 <- Myc_summary2 %>%
  left_join(Myc2 %>% dplyr::select(Sample, Exp1Group), by = "Sample")

# add room back
Myc4 <- Myc4 %>%
  left_join(
    Myc2 %>% 
      distinct(Sample, Room),   # keep one row per Sample–Room pair
    by = "Sample"
  )

# Remove if treatment group = NA
Myc3 <- Myc3 %>%
  filter(!is.na(Exp1Group))

# Remove if treatment group = NA
Myc4 <- Myc4 %>%
  filter(!is.na(Exp1Group))

#### visualisations ####
# Let's make a boxplot for presence of any myc structures
library(ggplot2)

#Myc3$Exp1Group <- factor(Myc3$Exp1Group, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8"), # order  
 #                        labels = c("Baseline", "m", "nwp", "mnp", "mnw", "mwp", "n", "w", "p")) # labels 

MycPlot <- ggplot(data = Myc3, 
                  aes(x = factor(Exp1Group), y = perc_c, fill = Exp1Group)) +
  geom_boxplot(notch = TRUE, varwidth = TRUE) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.6, width = 0.2) +
  ylab("Percent mycorrhizal colonisation") +
  xlab("Treatment Group") +
  scale_fill_manual(values = c( "Baseline" = "#1b9e77", "m" = "#82C782", "nwp" = "#CF597E", 
  "mnp" = "lavender", "mnw" = "lavender", "mwp" = "lavender", "n" = "#E9A96C", "w" = "#E9A96C", "p" = "#E9A96C" )) +
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

# Let's make a boxplot for presence of arbuscules
#Myc4$Exp1Group <- factor(Myc4$Exp1Group, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8"), # order  
#                         labels = c("Baseline", "m", "nwp", "mnp", "mnw", "mwp", "n", "w", "p")) # labels 

library(ggplot2)
MycPlot <- ggplot(data = Myc4, 
                  aes(x = factor(Exp1Group), y = perc_A, fill = Exp1Group)) +
  geom_boxplot(notch = TRUE, varwidth = TRUE) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.6, width = 0.2) +
  ylab("Percent arbuscular colonisation") +
  xlab("Treatment Group") +
  scale_fill_manual(values = c( "Baseline" = "#1b9e77", "m" = "#82C782", "nwp" = "#CF597E", 
                                "mnp" = "lavender", "mnw" = "lavender", "mwp" = "lavender", "n" = "#E9A96C", "w" = "#E9A96C", "p" = "#E9A96C" )) +
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

#### analysis ####
# model
# change group names
Myc3$Exp1Group <- factor(Myc3$Exp1Group, levels = c("0", "1", "2","3","4", "5", "6", "7", "8"), # order  
                         labels = c("baseline", "m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 
# try a mixed effects model
library(lme4)
# Fit a model 
M1 <- lmer(perc_c ~ factor(Exp1Group) + (1 | Room), data = Myc3)

summary(M1)

res <- residuals(M1)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M1, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

# Check for Homogeneous variance for any myc structure presence
summ_Myc3 <- Myc3 %>%
  group_by(Exp1Group) %>% 
  summarise(mean_perc_c = mean(perc_c),
            sd_perc_c = sd(perc_c),
            se_perc_c = sd(perc_c)/sqrt(n()),
            n_perc_c = n())
ratio <-(max(summ_Myc3$sd_perc_c))/(min(summ_Myc3$sd_perc_c))
print(ratio)

# Run Type III ANOVA
library(car)
Anova(M1, type = 3)

# Tukey-Kramer test (automatically applied for unequal sample sizes)
library(emmeans)
emm <- emmeans(M1, ~ Exp1Group)        # treatment effects within each room
MycModel <- pairs(emm, adjust = "tukey")

MycModel <- as.data.frame(MycModel)


# hist(Myc3$perc_c)
# 
# # not normal, check within each group
# ggplot(Myc3) +
#   geom_histogram(aes(perc_c), binwidth = 1)+
#   facet_wrap(~Exp1Group)
# 
# #try a transformation
# Myc3 <- Myc3 %>%
# mutate(logperc_c = log(perc_c + 1))
# 
# #try a transformation
# Myc3 <- Myc3 %>%
#   mutate(sqrtperc_c = sqrt(perc_c))
# 
# hist(Myc3$sqrtperc_c)
# 
# # Define a custom function
# cuberoot <- function(x) {
#   sign(x) * abs(x)^(1/3)
# }
# 
# #try a transformation
# Myc3 <- Myc3 %>%
#   mutate(cubeperc_c = cuberoot(perc_c))
# 
# hist(Myc3$cubeperc_c)
# 
# 
# # not better
# # check for differences in myc with kruskal wallis within each room
# #Kruskal-wallis for each room
# Room546Myc3 <- Myc3 %>% filter(Room == 546)
# 
# kruskal.test(perc_c ~ Exp1Group, data = Room546Myc3)
# 
# # Kruskal-wallis for each room
# Room547Myc3 <- Myc3 %>% filter(Room == 547)
# 
# kruskal.test(perc_c ~ Exp1Group, data = Room547Myc3)
# 
# # Kruskal-wallis for each room
# Room544Myc3 <- Myc3 %>% filter(Room == 544)
# 
# kruskal.test(perc_c ~ Exp1Group, data = Room544Myc3)
# 
# # dunn test
# library(FSA)
# library(dplyr)
# library(gt)
# 
# # Example data
# # response = numeric variable
# # group = factor with 3+ groups
# # dunnTest automatically performs pairwise comparisons
# 
# dt <- dunnTest(perc_c ~ Exp1Group,
#          data = Room546Myc3,
#          method = "bonferroni")
# 
# dunn_tbl <- dt$res %>%
#   mutate(
#     Comparison =
#       dplyr::case_when(
#         # Case 2: FSA output already has a Comparison column
#         "Comparison" %in% names(.) ~ Comparison
#       ),
#     Z = round(Z, 3),
#     P.unadj = round(P.unadj, 4),
#     P.adj = round(P.adj, 4)
#   ) %>%
#   select(Comparison, Z, P.unadj, P.adj)
# 
# gt_tbl <- dunn_tbl %>%
#   gt() %>%
#   tab_header(
#     title = "Pairwise Dunn Test with Bonferroni Correction"
#   ) %>%
#   cols_label(
#     Comparison = "Group Comparison",
#     Z = "Z Statistic",
#     P.unadj = "Unadjusted p",
#     P.adj = "Bonferroni-adjusted p"
#   ) %>%
#   fmt_number(
#     columns = c(Z, P.unadj, P.adj),
#     decimals = 4
#   ) %>%
#   tab_style(
#     style = cell_text(weight = "bold"),
#     locations = cells_column_labels()
#   )
# 
# print(gt_tbl)
# 
# gtsave(gt_tbl, "dunn_table.html")
# 
# library(writexl)
# 
# write_xlsx(dunn_tbl, "C:/Users/bella/Documents/DunnTableRoom546.xlsx")
# 
# # dunnTest automatically performs pairwise comparisons
# 
# dt <- dunnTest(perc_c ~ Exp1Group,
#                data = Room547Myc3,
#                method = "bonferroni")
# 
# dunn_tbl <- dt$res %>%
#   mutate(
#     Comparison =
#       dplyr::case_when(
#         # Case 2: FSA output already has a Comparison column
#         "Comparison" %in% names(.) ~ Comparison
#       ),
#     Z = round(Z, 3),
#     P.unadj = round(P.unadj, 4),
#     P.adj = round(P.adj, 4)
#   ) %>%
#   select(Comparison, Z, P.unadj, P.adj)
# 
# gt_tbl <- dunn_tbl %>%
#   gt() %>%
#   tab_header(
#     title = "Pairwise Dunn Test with Bonferroni Correction"
#   ) %>%
#   cols_label(
#     Comparison = "Group Comparison",
#     Z = "Z Statistic",
#     P.unadj = "Unadjusted p",
#     P.adj = "Bonferroni-adjusted p"
#   ) %>%
#   fmt_number(
#     columns = c(Z, P.unadj, P.adj),
#     decimals = 4
#   ) %>%
#   tab_style(
#     style = cell_text(weight = "bold"),
#     locations = cells_column_labels()
#   )
# 
# print(gt_tbl)
# 
# gtsave(gt_tbl, "dunn_table.html")
# 
# library(writexl)
# 
# write_xlsx(dunn_tbl, "C:/Users/bella/Documents/DunnTableRoom547.xlsx")
# 
# dt <- dunnTest(perc_c ~ Exp1Group,
#                data = Room544Myc3,
#                method = "bonferroni")
# 
# dunn_tbl <- dt$res %>%
#   mutate(
#     Comparison =
#       dplyr::case_when(
#         # Case 2: FSA output already has a Comparison column
#         "Comparison" %in% names(.) ~ Comparison
#       ),
#     Z = round(Z, 3),
#     P.unadj = round(P.unadj, 4),
#     P.adj = round(P.adj, 4)
#   ) %>%
#   select(Comparison, Z, P.unadj, P.adj)
# 
# gt_tbl <- dunn_tbl %>%
#   gt() %>%
#   tab_header(
#     title = "Pairwise Dunn Test with Bonferroni Correction"
#   ) %>%
#   cols_label(
#     Comparison = "Group Comparison",
#     Z = "Z Statistic",
#     P.unadj = "Unadjusted p",
#     P.adj = "Bonferroni-adjusted p"
#   ) %>%
#   fmt_number(
#     columns = c(Z, P.unadj, P.adj),
#     decimals = 4
#   ) %>%
#   tab_style(
#     style = cell_text(weight = "bold"),
#     locations = cells_column_labels()
#   )
# 
# print(gt_tbl)
# 
# gtsave(gt_tbl, "dunn_table.html")
# 
# library(writexl)
# 
# write_xlsx(dunn_tbl, "C:/Users/bella/Documents/DunnTableRoom544.xlsx")

# # The sample sizes are not quite even and therefore we need to use Type 3 analysis
 Myc3$Exp1Group <- as.factor(Myc3$Exp1Group)
# 
contrasts(Myc3$Exp1Group) <- contr.sum(9)

model <- lm(perc_c ~ factor(Exp1Group), data = Myc3)

# Run Type III ANOVA
library(car)
Anova(model, type = 3)

# Tukey-Kramer test (automatically applied for unequal sample sizes)
library(emmeans)
emm <- emmeans(model, ~ Exp1Group)        # treatment effects within each room
MycModel <- pairs(emm, adjust = "tukey")

MycModel <- as.data.frame(MycModel)

# library(writexl)
# 
# write_xlsx(MycModel, "C:/Users/bella/Documents/MycModel.xlsx")

# calculate some summary statistics
print(summ_Myc3)

#### Do the same for Myc4 ####

# Arbuscule only model
# change group names
Myc4$Exp1Group <- factor(Myc4$Exp1Group, levels = c("0", "1", "2","3","4", "5", "6", "7", "8"), # order  
                        labels = c("baseline", "m", "nbp", "np", "nb", "bp", "n", "b", "p")) # labels 



# Check for Homogeneous variance for any myc structure presence
summ_Myc4 <- Myc4 %>%
  group_by(Exp1Group) %>% 
  summarise(mean_perc_A = mean(perc_A),
            sd_perc_A = sd(perc_A),
            se_perc_A = sd(perc_A)/sqrt(n()),
            n_perc_A = n())
ratio <-(max(summ_Myc4$sd_perc_A))/(min(summ_Myc4$sd_perc_A))
print(ratio)

hist(Myc4$perc_A)

# too many infinities

# not better
# # check for differences in myc with kruskal wallis within each room
# #Kruskal-wallis for each room
# Room546Myc4 <- Myc4 %>% filter(Room == 546)
# 
# kruskal.test(perc_A ~ Exp1Group, data = Room546Myc4)
# 
# # Kruskal-wallis for each room
# Room547Myc4 <- Myc4 %>% filter(Room == 547)
# 
# kruskal.test(perc_A ~ Exp1Group, data = Room547Myc4)
# 
# # Kruskal-wallis for each room
# Room544Myc4 <- Myc4 %>% filter(Room == 544)
# 
# kruskal.test(perc_A ~ Exp1Group, data = Room544Myc4)
# 
# # dunn test
# library(FSA)
# library(dplyr)
# library(gt)
# 
# # Example data
# # response = numeric variable
# # group = factor with 3+ groups
# # dunnTest automatically performs pairwise comparisons
# 
# dt <- dunnTest(perc_A ~ Exp1Group,
#                data = Room546Myc4,
#                method = "bonferroni")
# 
# dunn_tbl <- dt$res %>%
#   mutate(
#     Comparison =
#       dplyr::case_when(
#         # Case 2: FSA output already has a Comparison column
#         "Comparison" %in% names(.) ~ Comparison
#       ),
#     Z = round(Z, 3),
#     P.unadj = round(P.unadj, 4),
#     P.adj = round(P.adj, 4)
#   ) %>%
#   select(Comparison, Z, P.unadj, P.adj)
# 
# gt_tbl <- dunn_tbl %>%
#   gt() %>%
#   tab_header(
#     title = "Pairwise Dunn Test with Bonferroni Correction"
#   ) %>%
#   cols_label(
#     Comparison = "Group Comparison",
#     Z = "Z Statistic",
#     P.unadj = "Unadjusted p",
#     P.adj = "Bonferroni-adjusted p"
#   ) %>%
#   fmt_number(
#     columns = c(Z, P.unadj, P.adj),
#     decimals = 4
#   ) %>%
#   tab_style(
#     style = cell_text(weight = "bold"),
#     locations = cells_column_labels()
#   )
# 
# print(gt_tbl)
# 
# gtsave(gt_tbl, "dunn_table.html")
# 
# library(writexl)
# 
# write_xlsx(dunn_tbl, "C:/Users/bella/Documents/DunnTableRoom546AMF.xlsx")
# 
# # dunnTest automatically performs pairwise comparisons
# 
# dt <- dunnTest(perc_A ~ Exp1Group,
#                data = Room547Myc4,
#                method = "bonferroni")
# 
# dunn_tbl <- dt$res %>%
#   mutate(
#     Comparison =
#       dplyr::case_when(
#         # Case 2: FSA output already has a Comparison column
#         "Comparison" %in% names(.) ~ Comparison
#       ),
#     Z = round(Z, 3),
#     P.unadj = round(P.unadj, 4),
#     P.adj = round(P.adj, 4)
#   ) %>%
#   select(Comparison, Z, P.unadj, P.adj)
# 
# gt_tbl <- dunn_tbl %>%
#   gt() %>%
#   tab_header(
#     title = "Pairwise Dunn Test with Bonferroni Correction"
#   ) %>%
#   cols_label(
#     Comparison = "Group Comparison",
#     Z = "Z Statistic",
#     P.unadj = "Unadjusted p",
#     P.adj = "Bonferroni-adjusted p"
#   ) %>%
#   fmt_number(
#     columns = c(Z, P.unadj, P.adj),
#     decimals = 4
#   ) %>%
#   tab_style(
#     style = cell_text(weight = "bold"),
#     locations = cells_column_labels()
#   )
# 
# print(gt_tbl)
# 
# gtsave(gt_tbl, "dunn_table.html")
# 
# library(writexl)
# 
# write_xlsx(dunn_tbl, "C:/Users/bella/Documents/DunnTableRoom547AMF.xlsx")
# 
# dt <- dunnTest(perc_A ~ Exp1Group,
#                data = Room544Myc4,
#                method = "bonferroni")
# 
# dunn_tbl <- dt$res %>%
#   mutate(
#     Comparison =
#       dplyr::case_when(
#         # Case 2: FSA output already has a Comparison column
#         "Comparison" %in% names(.) ~ Comparison
#       ),
#     Z = round(Z, 3),
#     P.unadj = round(P.unadj, 4),
#     P.adj = round(P.adj, 4)
#   ) %>%
#   select(Comparison, Z, P.unadj, P.adj)
# 
# gt_tbl <- dunn_tbl %>%
#   gt() %>%
#   tab_header(
#     title = "Pairwise Dunn Test with Bonferroni Correction"
#   ) %>%
#   cols_label(
#     Comparison = "Group Comparison",
#     Z = "Z Statistic",
#     P.unadj = "Unadjusted p",
#     P.adj = "Bonferroni-adjusted p"
#   ) %>%
#   fmt_number(
#     columns = c(Z, P.unadj, P.adj),
#     decimals = 4
#   ) %>%
#   tab_style(
#     style = cell_text(weight = "bold"),
#     locations = cells_column_labels()
#   )
# 
# print(gt_tbl)
# 
# gtsave(gt_tbl, "dunn_table.html")
# 
# library(writexl)
# 
# write_xlsx(dunn_tbl, "C:/Users/bella/Documents/DunnTableRoom544AMF.xlsx")
# 

# Fit a model
M2 <- lmer(perc_A ~ factor(Exp1Group) + (1 | Room), data = Myc4)

summary(M2)

res <- residuals(M2)

# Method 1: Base R diagnostic plot (select plot 2)
plot(M2, which = 2)

# Method 2: Specific Q-Q plot
qqnorm(res)
qqline(res, col = "red")

# Check for Homogeneous variance for arbuscule only presence
summ_Myc4 <- Myc4 %>%
  group_by(Exp1Group) %>%
  summarise(mean_perc_A = mean(perc_A),
            sd_perc_A = sd(perc_A),
            se_perc_A = sd(perc_A)/sqrt(n()),
            n_perc_A = n())
ratio <-(max(summ_Myc4$sd_perc_A))/(min(summ_Myc4$sd_perc_A))
print(ratio)

# good

# The sample sizes are not quite even and therefore we need to use Type 3 analysis
Myc4$Exp1Group <- as.factor(Myc4$Exp1Group)

contrasts(Myc4$Exp1Group) <- contr.sum(9)

model <- lm(perc_A ~ factor(Exp1Group), data = Myc4)

# Run Type III ANOVA
library(car)
Anova(M2, type = 3)

# Tukey-Kramer test (automatically applied for unequal sample sizes)
library(emmeans)
emm <- emmeans(M2, ~ Exp1Group)        # treatment effects within each room
ArbusculeModel <- pairs(emm, adjust = "tukey")

ArbusculeModel <- as.data.frame(ArbusculeModel)

library(writexl)

write_xlsx(ArbusculeModel, "C:/Users/bella/Documents/ArbusculeModel.xlsx")

# calculate summary stats
print(summ_Myc4)
