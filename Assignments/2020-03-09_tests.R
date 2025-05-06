#### Getting Started ####

# Clean up workspace
rm(list = ls())

### 3.1 Introduction ####

#install.packages("tidyverse")
library("tidyverse")
if(!require(nycflights13)){install.packages("nycflights13")}
if(!require(gapminder)){install.packages("gapminder")}
if(!require(Lahman)){install.packages("Lahman")}
tidyverse_update()

library(readxl)
births <- read_excel("datasets/demos/births_country.csv.xlsx")
View(births)

sum_dif <- births %>%
  summarize (n_Difference = n(),
meand = mean(Difference),
sdd = sd(Difference),
sed = sd(Difference) / sqrt(15))

#Problem two Lizards and Shrikes

data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")

data01 <- data01 %>% slice(-105)

view(data01)

sum_lizards <- data01 %>%
group_by(Survival) %>%
summarize (n_squamosalHornLength = n(),
           meanHornLength = mean(squamosalHornLength),
           varHornLength = var(squamosalHornLength),
           sdHornLength = sd(squamosalHornLength),
           seHornLength = sd(squamosalHornLength) / sqrt(184))

view(sum_lizards)
