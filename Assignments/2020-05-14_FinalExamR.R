#### Final Exam: Linear Regression #### 

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

### Linear Regression problem 11####

insulation <- read_csv("datasets/demos/insulation.csv")

# Examine the data, including the residuals
model01 <- lm(heat_loss ~ leanness, data = insulation)
autoplot(model01, smooth.colour = NA)

ggplot(data = insulation)+
  geom_point(aes(x = leanness, y= resid(model01)))

# The normal Q-Q plot is appropriately linear. The residuals versus predicted is 
# somewhat fan shaped and the residual by x plot is also somewhat fan shaped

# Try a transformation
insulation <- insulation %>%
  mutate(sqrt_heat_loss = sqrt(heat_loss))
model02<-lm(sqrt_heat_loss ~ leanness, data = insulation)
ggplot(data = insulation)+
  geom_point(aes(x = leanness, y= resid(model02)))
autoplot(model02)
 
# The transformation made the Q-Q plot slightly less linear, but not to an
# unacceptable degree. It also made both the residuals vs fitted and the 
# residual by x plots less fan shaped. Therefore the transformation is warranted.

summary(model02)

# Boys with higher body leanness values had significantly higher heat loss per 
# minute in the water (Linear Regression: sqrt(heat_loss) = 0.12389 + .046761
# (leanness); df = 1, 10, F = 44.58, p-value: p < 0.0001), and leanness explained
# 44.58% of the variation in square root transformed heat loss (R2 = 0.8168).
