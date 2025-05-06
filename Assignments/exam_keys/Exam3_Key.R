#### Lab 9: Correlation, Linear Regression #### 

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()


library("ggfortify")
library("multcomp")
library("broom")


# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

### Problem 4 ####

prob4 <- tribble(
  ~visits,~offspring,
1,1,
3,5,
4,2,
4,7,
6,4,
7,7
)

# Check for the assumption of bivariate normality using a basic scatter 
# plot

ggplot(data = prob4) +
  geom_point(mapping = aes(x = visits, y = offspring ))

# For a fancier scatter plot using more options see the commands here.
ggplot(data = prob4) +
  geom_point(mapping = aes(x = visits, y = offspring),
             colour = "firebrick", size = 2)+
  theme_bw()+
  labs( x = "Visits to doctor", y = "Number of Offspring")

ggplot(data = prob4)+
  geom_histogram(aes(visits), binwidth = 1)
ggplot(data = prob4)+
  geom_histogram(aes(offspring), binwidth = 1)

ggplot(data = prob4)+
  geom_boxplot(aes("", offspring))
ggplot(data = prob4)+
  geom_qq(aes(sample = offspring))


# Start with a correlation using the function cor.test() and saving it in
# the object prob4Cor. 

# NOTE: since technically in a correlation we are not testing for
# a causal or explanatory relationship, there is no "y" response variable
# per se.  Or at least that is my explanation for the weird formula.
# The formula takes the form ~ x + y or in this example:
# ~ visits + offspring

prob4Cor <- cor.test(~ visits + offspring, data = prob4,
                     method = "pearson")
prob4Cor

# The correlation coefficient, r, is found under sample estimates: cor
# Based on this result, there is a significant positive association 
# between events experienced as a nestling and future behavior.
# If you so desired, you could create an object called r too
r <- prob4Cor$estimate
r

### Scenario 2 ####
caffeine <- read_csv("datasets/demos/caffeine.csv", col_types = cols(
  group = col_factor() ))
ggplot(caffeine) +
  geom_histogram(aes(half_life), binwidth = 2)+
  facet_wrap(~group)
ggplot(caffeine)+
  geom_qq(aes(sample = half_life, color = group))
ggplot(caffeine) +
  geom_boxplot(aes(x = group, y = half_life))

#multiple planned comparisons
model02 <- lm(half_life~group, data = caffeine)

planned <- glht(model02, linfct = 
                  mcp(group = c("male - norm_prog = 0",
                                "norm_prog - high_prog = 0")))
confint(planned)
summary(planned)
