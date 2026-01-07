#### Buffer Co-occurrence Matrix Code ####

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
library(dplyr)
library(readr)
Bufferdata <- read_csv("Buffer Data/Bufferdata.csv")

# remove rows that do not include surveys
Bufferdata <- Bufferdata %>% filter(EndOutcome != "Q")
Bufferdata <- Bufferdata %>% filter(EndOutcome != "NV")
Bufferdata <- Bufferdata %>% filter(EndOutcome != "NH")
Bufferdata <- Bufferdata %>% filter(EndOutcome != "CB")
Bufferdata <- Bufferdata %>% filter(EndOutcome != "NA")

# Unite coordinate columns
library(tidyr)
Bufferdata <- Bufferdata %>%
  unite(x, y, x, sep = " - ")

# Next I need to give each site a unique numerical ID because the co-occurrence 
# matrix requires this
Bufferdata$x <- as.numeric(as.factor(Bufferdata$x))

# Rename the x column to a more logical name: site
colnames(Bufferdata)[53] <- c("site") ## Renaming the columns

# I need to change the current df so that it is presence/absence for each species
# first, i will reduce to only one column for each species
Bufferdata <- subset(Bufferdata, select = -c(OldestGrowthStageHoneysuckle, Area_m2Honeysuckle, HeightMetresHoneysuckle, OldestGrowthStageMadeiraVine,
 Area_m2MadeiraVine, HeightMetresMadeiraVine, Area_m2Periwinkle, HeightMetresPeriwinkle, OldestGrowthStageEnglishIvy, Area_m2EnglishIvy, 
 HeightMetresEnglishIvy, OldestGrowthStageBlueMorningGlory, Area_m2BlueMorningGlory, HeightMetresBlueMorningGlory, OldestGrowthStageJasmine, 
 Area_m2Jasmine, HeightMetresJasmine, OldestGrowthStageCoastalBanksia, Area_m2CoastalBanksia, HeightMetresCoastalBanksia, OldestGrowthStageRhamnus, 
 Area_m2Rhamnus, HeightMetresRhamnus, OldestGrowthStageWoolly, Area_m2Woolly, HeightMetresWoolly,
 Area_m2BushyAsparagus, HeightMetresBushyAsparagus, OldestGrowthStageClimbingAsparagus, Area_m2ClimbingAsparagus, HeightMetresClimbingAsparagus,
 OldestGrowthStageGinger, Area_m2Ginger, Height_metresGinger, OldestGrowthStageMothPlant, Area_m2MothPlant, HeightMetresMothPlant, CreationDate, EndOutcome))

colnames(Bufferdata)[1:13] <- c("A.sericifera", "H.gardnerianum", "A.scandens", "A.densiflorus", "S.mauritianum", "R.alaternus", 
                                "B.integrifolia", "J.polyanthum", "I.tricolor", "H.helix", "V.major", "A.cordifolia", "L.japonica") ## Renaming the columns

# Now I need any rows that are NA to be 0 and any with text to be 1
Bufferdata[, 1:13] <- lapply(Bufferdata[, 1:13], function(col) {
  ifelse(!is.na(col), "1", NA) })
Bufferdata <- Bufferdata %>%
  mutate_all(~ replace(., is.na(.), 0))

# remove duplicates
Bufferdata <- Bufferdata[!duplicated(Bufferdata$site), ]

Bufferdata = as.data.frame(Bufferdata)
# Needs to remove the first column of numbers as row names and make the Scientific 
# names of species into the row names
Bufferdata <- Bufferdata[, c(14, setdiff(1:ncol(Bufferdata), 14))]

row.names(Bufferdata) <- Bufferdata$site 
# Remove the first column from the data frame 
Bufferdata <- Bufferdata[, -1]

# make the columns numeric
Bufferdata$A.sericifera <- as.numeric(Bufferdata$A.sericifera)
Bufferdata$H.gardnerianum <- as.numeric(Bufferdata$H.gardnerianum)
Bufferdata$A.scandens <- as.numeric(Bufferdata$A.scandens)
Bufferdata$A.densiflorus <- as.numeric(Bufferdata$A.densiflorus)
Bufferdata$S.mauritianum <- as.numeric(Bufferdata$S.mauritianum)
Bufferdata$R.alaternus <- as.numeric(Bufferdata$R.alaternus)
Bufferdata$B.integrifolia <- as.numeric(Bufferdata$B.integrifolia)
Bufferdata$J.polyanthum <- as.numeric(Bufferdata$J.polyanthum)
Bufferdata$I.tricolor <- as.numeric(Bufferdata$I.tricolor)
Bufferdata$H.helix <- as.numeric(Bufferdata$H.helix)
Bufferdata$V.major <- as.numeric(Bufferdata$V.major)
Bufferdata$A.cordifolia <- as.numeric(Bufferdata$A.cordifolia)
Bufferdata$L.japonica <- as.numeric(Bufferdata$L.japonica)

# transpose
BufferdataT <- t(Bufferdata)
BufferdataT = as.data.frame(BufferdataT)
dim(BufferdataT)

BufferdataT[is.na(BufferdataT)] <- 0 
BufferdataT[BufferdataT != 0] <- 1
BufferdataT <- BufferdataT[rowSums(BufferdataT) > 1, ]
#### co-occurrence matrix ####
library(cooccur)

cooccur.Survey <- cooccur(BufferdataT,
                          type = "spp_site",
                          thresh = FALSE,
                          spp_name = TRUE)
class(cooccur.Survey)

summary(cooccur.Survey)
cooccur(mat = BufferdataT, type = "spp_site", thresh = FALSE, spp_names = TRUE)

Prob_table <- prob.table(cooccur.Survey)
str(BufferdataT) 
summary(BufferdataT)
# plot the co-occurrence matrix, need to make the plot into an object so that the legend can be removed

plot <- plot(cooccur.Survey, legend = NULL, plotrand = "TRUE") # add "plotrand = TRUE" to include completely random species

# Remove legend 
plot + theme(legend.position = "none") + ggtitle(NULL) + 
  scale_fill_manual(values = c("#C75DAA","#E5E5E5", "#90CBCD")) #Change axis title text font etc
