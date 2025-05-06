# Exam 1
# Script to generate plots and answer questions

# Clean up the working environment
rm(list = ls())

### Install and load packages ####

if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Check for updates
tidyverse_update()

### Salmon mass ####
# Read in salmon mass data
salmon <- read_csv("datasets/abd/chapter02/chap02f2_5SalmonBodySize.csv", 
                   col_types = cols(sex = col_character()))

# Calculate summary statistics for massKg
summary_massKg <- salmon %>%
  summarise(mean_massKg = mean(massKg),
            median_massKg = median(massKg),
            IQR_massKg = IQR(massKg),
            sd_massKg = sd(massKg),
            var_massKg = var(massKg))

# Boxplot of salmon weight in kg
ggplot(data = salmon)+
  geom_boxplot(aes(x = "", y = massKg), notch = TRUE)+
  stat_summary(aes(x = "", y = massKg), 
               fun.y=mean, 
               colour="darkred", 
               geom="point", 
               shape=18, 
               size=3)

ggplot(data = salmon)+
  geom_boxplot(aes(x = "", y = massKg), notch = TRUE)+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

### Inbreeding in Spanish royalty ####

# This creates the dataframe and names it inbreeding
inbreeding <- tribble(
  ~Parents, ~F,~PostnatalSurvival,
  #--/--/--
  "PhilipII-ElizabethValois", 0.01,1.00,
  "PhilipI-JoannaI", 0.04,1.00,
  "Ferdinand-ElizabethCastile", 0.04,1.00,
  "PhilipIV-ElizabethBourbon", 0.05, 0.50,
  "PhilipIII-MargaretAustria", 0.12, 0.63,
  "CharlesI-IsabellaPortugal", 0.12, 0.60,
  "PhillipII-AnnaAustria", 0.22, 0.20,
  "PhilipIV-MarianaAustria", 0.25,0.40
)

# Calculate summary statistics for massKg
summary_F <- inbreeding %>%
  summarise(mean_F = mean(F),
            median_F = median(F),
            IQR_F = IQR(F),
            sd_F = sd(F),
            var_F = var(F))

# Boxplot of inbreeding weight in kg
ggplot(data = inbreeding)+
  geom_boxplot(aes(x = "", y = F), notch = TRUE)+
  stat_summary(aes(x = "", y = F), 
               fun.y=mean, 
               colour="darkred", 
               geom="point", 
               shape=18, 
               size=3)


### Number of cats ####

# Original frequency distribution
cats = c(
  rep("02",1),
  rep("03", 2),
  rep("04", 3),
  rep("05", 4),
  rep("06", 5),
  rep("07", 6),
  rep("08", 5),
  rep("09", 4),
  rep("10", 3),
  rep("11", 2),
  rep("12", 1))
as_factor(cats) 

discrete <- tibble(cats)



ggplot(data = discrete, aes(cats, y = ..prop.., group = 1))+
  geom_bar()+
  coord_cartesian(ylim = c(0,0.25))+
  geom_text(stat = "count",
            aes(label = round(..prop..,2), y = ..prop..+0.02))+
  labs( x = "Number of cats per household", y = "Probability")

### Brineshrimp ploidy ####

# Read in polyploid Artemesia file
polyploid <- read_csv("datasets/demos/polyploid.csv", 
                      col_types = cols(X3 = col_skip(), X4 = col_skip(), 
                                       X5 = col_skip(), X6 = col_skip(), 
                                       X7 = col_skip()))

summ_polyploid <- polyploid %>%
  group_by(ploidy) %>% 
  summarise(n = n(),
            mean = mean(length),
            median = median(length),
            IQR = IQR(length),
            var = var(length),
            sd = sd(length),
            se = sd(length)/sqrt(length(length)))

# Plot histograms
ggplot(polyploid) +
  geom_histogram(aes(length), binwidth = 1)+
  facet_wrap(~ploidy)

# Plot box plots
ggplot(polyploid)+
  geom_boxplot(aes(x = ploidy, y = length), notch = FALSE, varwidth = TRUE)
# Plot diploid separately
diploid_only <- polyploid %>%
  filter(ploidy == "2N")
ggplot(diploid_only)+
  geom_boxplot(aes(x = "", y = length), notch = FALSE, varwidth = TRUE)


### Horned lizards ####

# Read in Horned Lizard file and get rid of NA on line 105
data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
data01 <- data01 %>%
  slice(-105)


# Summary statistics
summ_young <- data01 %>%
  group_by(Survival) %>% 
  summarise(n = n(),
            mean = mean(squamosalHornLength),
            median = median(squamosalHornLength),
            IQR = IQR(squamosalHornLength),
            var = var(squamosalHornLength),
            sd = sd(squamosalHornLength),
            se = sd(squamosalHornLength)/sqrt(length(squamosalHornLength)))


# Sage cricket extra credit problem ###
data02 <- read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")
ggplot(data02) +
  geom_histogram(aes(timeToMating), binwidth = 10)+
  facet_wrap(~feedingStatus)

data02<- mutate(data02, log_time = log(timeToMating))
ggplot(data02) +
  geom_histogram(aes(log_time), binwidth = .5)+
  facet_wrap(~feedingStatus)

