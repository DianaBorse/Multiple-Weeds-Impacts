### Lab 2. Brief Introduction to RStudio

# Before anything else, verify that your environment is totally clear.
# This is important, because old objects can foul up the works
# Clean up the working environment
rm(list = ls())

# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# At the beginning of a script, it is useful to make sure you have
# downloaded and installed all necessary packages.

# IRL, I use a tidy bit of code and the p_load function in the package pacman
# to install any packages that have not yet been installed, but you can also
# have a separate line of code and a separate install.packages("PackageName")
# command for each one.  I do the latter here because it is clearer.

install.packages("DescTools")
install.packages("summarytools")
install.packages("Hmisc")
install.packages("ggfortify")
install.packages("multcomp")
install.packages("nlme")
install.packages("broom")
install.packages("ggmosaic")
install.packages("epitools")

install.packages("tidyverse")

# Before using an installed package, you must load it using library().
# For this lab, you only need to load the library for the package tidyverse.
# You will need to load the library every time you start a new R session or 
# run a new script.

library("tidyverse")
# Note that this loads the following packages: ggplot2 purrr 
#   tibble dplyr tidyr stringr readr forcats

# Check for updates
tidyverse_update()

# Create an object called x
x <- 3*4

# View x in the Console
x

# Read in the data file
data <- read_csv("datasets/r4all/competition.csv", col_names = TRUE)


# Swirl lesson
install.packages("swirl")
library(swirl)
