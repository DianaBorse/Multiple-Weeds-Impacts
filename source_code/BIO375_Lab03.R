### Lab 3. Data manipulation and graphing

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

### Install and load packages ####
# The following commands will install these packages if they are not already installed, 
# and then load them!

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# I give the general form below in comments.  A < > indicates you will type in
# your own value.  The main thing to watch for this command is the punctuation: ! ""

# if(!require(<package_name>)){install.packages("<package_name>")}

# Check for updates
tidyverse_update()

### Basic Plots ####

# Read in the data file
ward <- read_csv("datasets/quinn/chpt3/ward.csv")

# For the purposes of this exercise, we will start with only the mussel zone
# observations
ward_mussel <- ward %>%
  filter(ZONE == "Mussel")

# Plot number of eggs per egg mass in the mussel zone as a histogram
ggplot(ward_mussel)+
  geom_histogram(aes(EGGS), binwidth = 1)

# Plot two histograms, one for each zone, using facet_wrap()
ggplot(ward) +
  geom_histogram(aes(EGGS), binwidth = 2)+
  facet_wrap(~ZONE)

# Note that you must have the column name EXACTLY correct.  

# Look at the help file for geom_boxpolot
help("geom_boxplot")

# Plot number of eggs per egg mass in the mussel zone as a histogram
ggplot(ward_mussel)+
  geom_boxplot(aes(x = "", y = EGGS), notch = TRUE, varwidth = TRUE)

# Going back to our full dataset, named ward, plot number of eggs per mass as
# boxplots, grouping by ZONE
ggplot(ward)+
  geom_boxplot(aes(x = ZONE, y = EGGS), varwidth = TRUE)

### Descriptive Statistics ####
# Examining the data file ####

# Read in compensation data file
compensation<-read_csv("datasets/r4all/compensation.csv")

# If you are examining an existing dataset for the first time, names(), head()
# dim(), and str() are useful functions

# names() tells you the names assigned to each column, generally variable
names(compensation)

# head() gives you the first six rows of a dataset
head(compensation)

# dim() gives you the dimensions of your dataset
# The first value is the number of rows.  The second is the number of columns.
dim(compensation)

# str() returns the structure of the dataset
str(compensation)

# Using summary and summarise() ####

# In the case of the compensation data, we might want to know the mean and 
# standard deviation of each Grazing group. 

# The function summary gives an output in the console.  It is basic, but quick.
summary(compensation)

# The function summarise() gives an output in a data table, which is really
# useful later on.

# I give the general form below in comments.  A < > indicates you will type in
# your own value.  The find tool (File->Find) can really be your friend here, but be
# careful to always check the "In selection" box!

# <new_object_name> <- <data> %>%
# group_by(<grouping_variable>) %>%
# summarise(
# mean_resp = mean(<response_variable_name>),
# median_resp = median(<response_variable_name>),
# IQR_resp = IQR(<response_variable_name>),
# sd_resp = sd(<response_variable_name>),
# var_resp = var(<response_variable_name>)
# )

summ_root <- compensation %>%
  group_by(Grazing) %>%
  summarise(mean_Root = mean(Root),
            median_Root = median(Root),
            IQR_Root = IQR(Root),
            sd_Root = sd(Root),
            var_Root = var(Root))


# Using descr() ####

# Another alternative is to use the descr() function instead
# of summarise.  I prefer summarise because it creates a new
# data table with the statistics instead of printing the statistics
# in the console, but descr is quick.

compensation %>%
  group_by(Grazing) %>%
  descr()

# Confidence interval of the mean ####

# Traditional method will return the same values that you can calculate
# by hand.  The bootstrapping method is more reliable on strongly skewed data,
# but values cannot be reproduced by hand.

# Let's return to our ward dataset.  If we want to calculate the confidence interval
# of the mean number of eggs/eggmass in each zone, we use the function
# groupwiseMean.  EGGS ~ ZONE argument tells R that EGGS is the y variable and 
# ZONE is the grouping variable

groupwiseMean(EGGS ~ ZONE,
              data = ward,
              conf = 0.95,
              digits = 3)

# If the data are not grouped, like the subset of the ward dataset we named
# ward_mussel, then the CI of the mean can be calculated by using the argument
# by putting a 1 where the grouping variable used to be, on the right side of
# the tilde

groupwiseMean(EGGS ~ 1,
              data = ward_mussel,
              conf = 0.95,
              digits = 3)

# Transforming a variable ####

# mutate() adds new variables while preserving existing ones.  General form:
# <new_object_name> <- mutate(<dataset_name>, <transform_variable_name> =
# <mathematical_function>(<variable_name>))

# For example, the mussel zone data had an outlier, indicating a positive
# skew.  If we take the square root of eggs, and then create plots
# with squareroot data, the outlier goes away.
ward<-mutate(ward, squareroot_eggs = sqrt(EGGS))
ggplot(ward)+
  geom_boxplot(aes(x = ZONE, y = squareroot_eggs), varwidth = TRUE)
ggplot(ward) +
  geom_histogram(aes(squareroot_eggs), binwidth = .2)+
  facet_wrap(~ZONE)

compensation<-mutate(compensation, log(Root))

