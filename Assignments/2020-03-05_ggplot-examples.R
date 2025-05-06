#### LAB 5 ####

#Clean up the workspace
rm(list = ls())

### load packages ####

#install.packages("tidyverse")
library("tidyverse")
if(!require(nycflights13)){install.packages("nycflights13")}
if(!require(gapminder)){install.packages("gapminder")}
if(!require(Lahman)){install.packages("Lahman")}
tidyverse_update()

mpg %>%
summary(mpg)%>%
view(mpg)

#### Different Kinds of Plots ####

# boxplot: geom_boxplot
# Histogram: geom_histogram
# Area chart: geom_area
# bar chart: geom_bar

# point is very simple geom_point is a dot plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# smooth geom_smooth is a line
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))
# Depending on what you want, you can use geom_line for a jagged line like a time series, geom_abline
# for a straight line specified by slope and intercept, geom_path to trace follow from timepoint 1 to 
# timepoint 2, to 3 etc.

# Or both, be sure to map at the top so it applies to both unless you want them 
# to be different
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

#### Aesthetic Mappings ####

# color = color
# size = size
# transparency = alpha
# shape = shape
# Outline thickness = stroke

#### Facets ####

# Can help pick out differences

#wrap facet_wrap
#grid facet_grid

#### Examples of Interesting Plots ####

# Position Adjustment with position_dodge2 and fill
# dodge arranged the variables horizontally while keeping the vertical position
# fill created the gradient based on the variable clarity
demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551)

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge2")

# Bar graph with stacking
ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut, fill = color))

# coord_polar is for pie charts aka stacked bar charts in polar coordinates
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut, fill = cut))+
  coord_polar()

# For a line graph without the added shading from smooth, you have to calculate 
# slope and intercept
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point(mapping = aes(x = wt, y = mpg), color = "PURPLE")
# Calculate slope and intercept of line of best fit
coef(lm(mpg ~ wt, data = mtcars))
p + geom_abline(mapping = aes(intercept = 37, slope = -5), color = "TURQUOISE")

#Overlayed with shape and color mapped to make a super confusing graph
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(shape = class, color = drv)) + 
  geom_smooth()

# Boxplot
ggplot(data = mpg, mapping = aes(x = drv, y = hwy)) +
  geom_boxplot(aes(color=class))
# Changing the aesthetic changes what data you can visualize with the plot
ggplot(data = mpg, mapping = aes(x = drv, y = hwy)) +
  geom_boxplot(aes(color=drv))
ggplot(data = mpg, mapping = aes(x = drv, y = hwy)) +
  geom_boxplot(aes(color=fl))
ggplot(data = mpg, mapping = aes(x = drv, y = hwy)) +
  geom_boxplot(aes(color=model))
# This last one has WAY too many groups

# Maps
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "BLACK", colour = "PURPLE") +
  coord_quickmap()
