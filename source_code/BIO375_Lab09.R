#### Lab 9: Correlation, Linear Regression #### 

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()


library("ggfortify")

library("broom")


# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

### Correlation ####

# The parametric (i.e., assuming bivariate normality) version of a
# correlation is named after some statistician named Pearson.  

# This example is described on pg.506 ABD
# Estimate a linear correlation between the number of non-parent adult
# visits experienced by boobies as chicks and the number of similar 
# behaviors performed by the same birds when adult. 
# Read and inspect the data

booby <- read_csv("datasets/abd/chapter16/chap16e1FlippingBird.csv")
head(booby)

# Check for the assumption of bivariate normality using a basic scatter 
# plot

ggplot(data = booby) +
  geom_point(mapping = aes(x = nVisitsNestling, y = futureBehavior ))

# For a fancier scatter plot using more options see the commands here.
ggplot(data = booby) +
  geom_point(mapping = aes(x = nVisitsNestling, y = futureBehavior),
             colour = "firebrick", size = 2)+
  theme_bw()+
  labs( x = "Events experienced as a nestling", y = "Future behavior")

# The scatter plot shows no evidence of non-linearity and the cloud of
# points is elliptical in shape.  Last way to check for bivariate normality 
# is to see whether x and y separately are normally distributed.  The
# sample size here is 24, so start with a histogram.
ggplot(data = booby)+
  geom_histogram(aes(nVisitsNestling), binwidth = 5)
ggplot(data = booby)+
  geom_histogram(aes(futureBehavior), binwidth = .2)

# nVisitsNestling looks good, doublecheck futureBehavior with a boxplot
# and Q-Q plot
ggplot(data = booby)+
  geom_boxplot(aes("", futureBehavior))
ggplot(data = booby)+
  geom_qq(aes(sample = futureBehavior))

# OK, so futureBehavior is not terrible, but not terrific, in terms of
# normality.  The median is located in the center of the box, but the lower
# whisker is a bit longer than the upper whisker indicating left skew. 
# The Q-Q plot looks a bit asymptotic.  A correlation is probably fine,
# but you could also justify a Spearman's rank correlation.

# Start with a correlation using the function cor.test() and saving it in
# the object boobyCor. 

# NOTE: since technically in a correlation we are not testing for
# a causal or explanatory relationship, there is no "y" response variable
# per se.  Or at least that is my explanation for the weird formula.
# The formula takes the form ~ x + y or in this example:
# ~ nVisitsNestling + futureBehavior

boobyCor <- cor.test(~ nVisitsNestling + futureBehavior, data = booby,
                     method = "pearson")
boobyCor

# The correlation coefficient, r, is found under sample estimates: cor
# Based on this result, there is a significant positive association 
# between events experienced as a nestling and future behavior.
# If you so desired, you could create an object called r too
r <- boobyCor$estimate
r

### Spearman's rank correlation ####

# We will now perform a non-parametric Spearman's rank correlation on the
# booby data.

# Test of zero Spearman rank correlation. In this example, the variable
# "nVisitsNestling" has lots of tied observations.
# Because of the ties, R will warn you that the P-value in the output is 
# not exact.  That is fine, you can still interpret the results!

boobySpear <-cor.test(~ nVisitsNestling + futureBehavior, data = booby,
                      method = "spearman")
boobySpear
# Note that instead of r, the coefficient is rho.  
# While the p-value of the non-parametric test is not quite as low, we
# still draw the same conclusion: there is a significant positive 
# association between events experienced as a nestling and future behavior.

### Linear Regression ####
# # Based on an example on pg.541 of ABD, researchers were interested in
# the relationship between lion age and proportion of black on the lion 
# nose. 

lion <- read_csv("datasets/abd/chapter17/chap17e1LionNoses.csv")

# The assumptions for a linear regression are difficult to test directly,
# so we mostly diagnose departures using residuals plots.  In order
# to plot residuals, however, we first need to fit a model!

model01 <- lm(proportionBlack ~ ageInYears, data = lion)

# Autoplot gives you a residual by predicted plot in the upper left panel
autoplot(model01, smooth.colour = NA)

# You will get the warning message:
# Removed 32 rows containing missing values (geom_path)
# That is because of the smooth.colour = NA argument, which I use because
# otherwise there is a distracting line

# I personally like to see residual by x plot.  To do that, we need
# to add columns to the original data, lion, for the residuals, etc.  To
# do this, we have multiple options.

# Option 1. Use the function augment.
    
lion_plus <- augment(model01)
ggplot(data = lion_plus)+
  geom_point(aes(x = ageInYears, y= .resid))

# Option 2.  Use the function resid() right in the plotting command
ggplot(data = lion)+
  geom_point(aes(x = ageInYears, y = resid(model01)))

# Option 3.  Use mutate() to add a residuals column to the original data
lion <- lion %>%
  mutate(black_resid = resid(model01))
ggplot(data = lion) +
  geom_point(aes(x = ageInYears, y = black_resid))

# So taking all the plots together, the normal Q-Q of the residuals looks
# like the residuals are normal, which is good
# Residuals vs. fitted (fitted is the same thing as predicted),
# Some fan shape, except for the highest fitted value.  Residuals vs.
# x?  Not good at all.  

# For the sake of learning, let's just see what happens if we proceed
# with the linear regression

# So what are the actual statistical results???
summary(model01)

# The p-value of interest is found in the row "ageInYears", the intercept
# and slope are found under the column header "Estimate".

# Older lions have significantly higher proportion of black on their noses
# (Linear regression: proportionBlack = 0.0697 + 0.0586(ageInYears);
# df = 1, 30, F=49.75, P<0.0001), and lion age explained more than 60%
# of the variabiity in nose blackness (R2 = 0.6238).

# For a linear regression, we usually want to add a regression line to 
# our plot and often we also want to give an idea about how confident we
# are in our estimate of that regression line.  To do this, we generate
# what are known as confidence bands.  The narrower the band, the more
# confident we are in our estimate of the line.  I

# We create confidence bands by adding in a layer 
# geom_smooth(method = "lm", level=0.95).

ggplot(data = lion, aes(x = proportionBlack, y = ageInYears)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "Proportion black", y = "Age (years)")

# What do you think will happen to the confidence band if we set level
# to 0.99?
ggplot(data = lion, aes(x = proportionBlack, y = ageInYears)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.99) +
  theme_bw()+
  labs( x = "Proportion black", y = "Age (years)")

### Linear Regression with transformation ####
# # Based on an example on pg.562 of ABD, researchers were interested in
# the relationship floral tube length of an iris and the number of
# pollen grains deposited by their pollinators.

iris <- read_csv("datasets/abd/chapter17/chap17f6_3IrisPollination.csv")

# Examine the data, including the residuals
model02 <- lm(grainsDeposited ~ tubeLengthMm, data = iris)
autoplot(model02, smooth.colour = NA)

ggplot(data = iris)+
  geom_point(aes(x = tubeLengthMm, y= resid(model02)))

# So all the diagnostic plots are terrible--the residual by predicted is
# fan shaped, the normal Q-Q for residuals is definitely not linear,
# and the residual by x plot is very fan shaped.  Let's try transforming
# the y variable, grainsDeposited

iris <- iris %>%
  mutate(sqrt_grains = sqrt(grainsDeposited))
model03<-lm(sqrt_grains ~ tubeLengthMm, data = iris)
ggplot(data = iris)+
  geom_point(aes(x = tubeLengthMm, y= resid(model03)))

# That is much better.  Now we can look at the statistical results!
summary(model03)

# Irises with longer tubes have significantly greater number of pollen 
# grains deposited by long-proboscid flies(Linear regression: 
# sqrt(grainsDeposited) = 2.083 + 0.1450(tubeLengthMm);
# df = 1, 72, F=26.88, P<0.0001), and flower tube length explained 
# more than 25% of the variation in square root transformed grain number
# (R2 = 0.2719).





