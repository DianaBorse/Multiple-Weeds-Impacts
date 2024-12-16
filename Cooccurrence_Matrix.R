#### Co-occurrence Matrix Code ####

# Before anything else, verify that your environment is totally clear.
# This is important, because old objects can foul up the works
# Clean up the working environment
rm(list = ls())

# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# At the beginning of a script, it is useful to make sure you have
# downloaded and installed all necessary packages.

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(tidyverse)){install.packages("Hmisc")}
if(!require(tidyverse)){install.packages("ggfortify")}
if(!require(tidyverse)){install.packages("multcomp")}
if(!require(tidyverse)){install.packages("nlme")}
if(!require(tidyverse)){install.packages("broom")}
if(!require(tidyverse)){install.packages("ggmosaic")}
if(!require(tidyverse)){install.packages("epitools")}
if(!require(tidyverse)){install.packages("swirl")}
if(!require(tidyverse)){install.packages("corrplot")}


# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()


#### Cleaning up the data ####

library(dplyr)

library(readr)
SurveyData <- read_csv("SurveyData_Clean.csv")

# Unite Site and Plot columns

library(tidyr)
SurveyData_united <- SurveyData %>%
  unite(Plot, Site, Plot, sep = " - ")

# Unite Plot and Weed vs Native columns so that now the plot id is all in one
# column that includes the site, plot number, and whether it is weed or native

library(tidyr)
SurveyData_Combined <- SurveyData_united %>%
  unite(Plot, Plot, W_N, sep = " - ")

# Next I need to add a column to give each plot a unique numerical ID



# My data needs to change to presence/absence by plot. Plot needs to be the 
# columns, and all species are rows with presence/absence denoted as a 0 or a 1

# The following are lines of code to make a tibble, but I don't think that's as
# useful as a dataframe will be


# PresenceAbsence <-SurveyData_Combined %>%
# rbind(as.matrix(SurveyData_Combined[, 2]), as.matrix(SurveyData_Combined[, -1])) %>%
# as_tibble() %>% 
# distinct() %>% 
# na.omit()

# This creates a new data frame that is presence/absence

library(tidyr)
library(dplyr)

PresenceAbsence <-SurveyData_Combined %>%
  pivot_wider(id_cols = Plot, names_from=ScientificName, values_from=ScientificName,
              values_fn=function(x) any(unique(x) == x) * 1, values_fill = 0)

# instead of being a tibble, I wanted to convert it back to a data frame
PresenceAbsence_df = as.data.frame(PresenceAbsence)

# I am trying to get rid of the rownames column, I think that this may be the 
# reason that the probalistic code is not running because this appears to be the 
# main difference between my presence/absence matrix and the sample data

rownames(PresenceAbsence_df) <- NULL

# The following line of code makes it into a matrix, but this leads to my 
# species names becoming numbers and that does not seem like it's going to work
# PresenceAbsence_Matrix <- data.matrix(PresenceAbsence_df)


#### Co-occur ####

# install.packages("cooccur")
library(cooccur)

data(PresenceAbsence_df)


cooccur.Survey <- cooccur(PresenceAbsence_df,
                         type = "spp_site",
                          thresh = TRUE,
                             spp_name = TRUE)
class(cooccur.Survey)

data(PresenceAbsence_df)
cooccur(mat, type = "Plot", thresh = TRUE, spp_names = FALSE,
        true_rand_classifier = 0.1, prob = "hyper", 
        site_mask = NULL, only_effects = FALSE,
        eff_standard = TRUE, eff_matrix = FALSE)


# with practice data

data(finches)

cooccur.finches <- cooccur(finches,
          type = "spp_site",
            thresh = TRUE,
            spp_names = TRUE)
class(cooccur.finches)


data("beetles")







#### Calculating Summary Statistics ####

summary(DataFileName)

summ_VariableName <- DataFileName %>%
  summarise(mean_VariableName = mean(VariableName),
            median_VariableName = median(VariableName),
            IQR_VariableName = IQR(VariableName),
            sd_VariableName = sd(VariableName),
            var_VariableName = var(VariableName),
            se_VariableName = sd(VariableName)/sqrt(39))

DataFileName %>%
  descr()

view(summ_VariableName)

#### Visualizing the Data ####

# Plot a histogram

ggplot(lovett)+ geom_histogram(aes(SO4, binwidth = .5))

# Make a boxplot

ggplot(data = lovett_tidy)+
  geom_boxplot(aes(x = type, y = measurement))+
  stat_summary(aes(x = type, y = measurement), fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3)

#### Confidence Intervals ####

# This is just a general outline for copy-pasting as needed.

# Data needed:
summarise(mean_variable = mean(variable),
          median_variable = median(variable),
          IQR_variable = IQR(variable),
          sd_variable = sd(variable),
          var_variable = var(variable),
          n_variable = n())

alpha <- 0.05
mean <- summary$mean
se <- summary$se
df <- summary$n -1

# mean + c(-1, 1)*qt(1-alpha, df)*se
