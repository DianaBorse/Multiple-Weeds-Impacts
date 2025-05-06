#### Lab 8: 1-way ANOVA, continued #### 
# For this lab you will use the datasets described in Chapter 15 of your book 
# but you will answer the slightly modified questions that I provide below

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()


#### Problem 15-23 ####
# Complete parts a and c only

#### Problem 15-26 ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

#### Problem 15-30 and/or 15-31 (same data in both problems) ####
# Use the data to perform the correct test.  Please show code for all steps in your process.