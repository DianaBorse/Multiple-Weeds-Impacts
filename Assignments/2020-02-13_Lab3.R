#Assignment from Lab3. 

#Clean up working environment
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

# Check for updates
tidyverse_update()

### Loading Lovett ####

#Load the Lovett file by navigating to the files tab in the lower right hand corner
# of the screen. It can be found at datasets > quinn > chpt2 > lovett.csv
#Click on the name of the file and select import data
#Or execute the following function
lovett <- read_csv("datasets/quinn/chpt2/lovett.csv")

### Calculating Summary Statistics and Visualization ####

#To calculate summary statistics, execute the function below
summary(lovett)

summ_SO4 <- lovett %>%
  summarise(mean_SO4 = mean(SO4),
            median_SO4 = median(SO4),
            IQR_SO4 = IQR(SO4),
            sd_SO4 = sd(SO4),
            var_SO4 = var(SO4),
            se_SO4 = sd(SO4)/sqrt(39))

lovett %>%
  descr()

view(summ_SO4)

#Next you will need to plot a histogram of the data
#To do this, you will need to execute the following function for SO4
ggplot(lovett)+ geom_histogram(aes(SO4, binwidth = .5))

#Do this again for Modified SO4
ggplot(lovett)+ geom_histogram(aes(SO4MOD, binwidth = .5))

#Now you need to generate box plots for So4 and Modified SO4

# The code below modifies the dataset so it only contains SO4 and Modified SO4
# using select{dplyr}, and is oriented in long form using gather{tidyr}
lovett_tidy <- lovett %>%
  select(contains("SO4"))%>%
  gather(key = "type", value = "measurement", SO4, SO4MOD)

#To generate a box plot for SO4, execute the following function

# The code below plots the two variables as boxplots, zooming in on the
# 40-75 range where most of the values are found (coord_cartesian).  The red 
# dots indicate the means (stat_summary).
ggplot(data = lovett_tidy)+
  geom_boxplot(aes(x = type, y = measurement))+
  coord_cartesian(ylim = c(40, 75))+
  stat_summary(aes(x = type, y = measurement), fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3)

### Creating and Opening an Excel File ####

#Create a data table in Excel
#Table should be created with tidy format (3 variables under bird colony,
# integers under beetle density)
#save the file under name "sanchez.csv" in datasets > demos

#Load file
#Go to files in the lower right hand window. Bio 375 > Analyses > lastname-firstname > 
# datasets > demos > sanchez.csv
#click import dataset
# The dataset should populate into the environment, to view the dataset, click the 
# name of the file (sanchez.csv)
#Or execute the following function
sanchez <- read_csv("datasets/quinn/chpt2/sanchez.csv")

### Calculate summary Statistics and Transforming Data ####
#To calculate the summary statistics, execute the function below
summary(sanchez)

summ_beetle96 <- sanchez %>%
  group_by(Colony) %>%
  summarise(mean_beetle96 = mean(beetle96),
            median_beetle96 = median(beetle96),
            IQR_beetle96 = IQR(beetle96),
            sd_beetle96 = sd(beetle96),
            var_beetle96 = var(beetle96),
            se_beetle96 = sd(beetle96)/sqrt(25))

View(summ_beetle96)

sanchez %>%
  group_by(Colony) %>%
  descr()

#Next you will need to generate a histogram for the data.
#To do this, you will execute the following function
ggplot(sanchez)+ geom_histogram(aes(beetle96, binwidth = .5))

#You will also need to generate a box plot for the data
#To do this, execute the following function
ggplot(sanchez)+
  geom_boxplot(aes(x = Colony, y = beetle96), varwidth = TRUE)

#Next, you will need to add a column to the data of log(y+1) for beetle densities
sanchez <- mutate(sanchez, transform_beetle96 = log(beetle96+1))

#Next you will need to generate a new histogram for the transformed data.
#To do this, you will execute the following function
ggplot(sanchez)+ geom_histogram(aes(beetle96, binwidth = .5))

#You will also need to generate a new box plot for the data
#To do this, execute the following function
ggplot(sanchez)+
  geom_boxplot(aes(x = Colony, y = beetle96), varwidth = TRUE)

### Great job annotating your code! 
### GRADE: 10/10 code runs without breaking ####