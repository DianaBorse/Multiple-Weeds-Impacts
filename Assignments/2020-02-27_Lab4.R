#Assignment for Lab 4 

#Clean up working environment
rm(list = ls())

# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

library(tidyverse)

# Check for updates
tidyverse_update()

mpg

summary(mpg)

view(mpg)

#### Plotting mg ####

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

#### 3.2.4 Exercises ####

# 1. Run
ggplot(data = mpg)
#I see nothing when I run this.

# 2. 
# There are 234 rows and 11 columns

# 3.
# drv stands for drive as in front wheel, 
# four wheel, and rear wheel.

# 4. 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = hwy, y = cyl))

# 5. 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv))
# This is not useful because both variables are discrete and neither relies
# on the other.

#### Aesthetic Mappings ####

#Aesthetic change - color
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

#Aesthetic change - size
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

#Aesthetic change - transparency

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

#Aesthetic change - shape

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

#Aesthetic change - color blue
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

#### 3.3.1 Exercises ####

# 1. Aesthetic change - color blue problem
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
#problem was that color blue was with parenthesis

# 2. 
?mpg
# Categorical: model, trans, drv, fl, class
# Numerical continuous: displ, cty, hwy
mpg
#This function shows the Categorical variables and the numerical
# continuous in the console

# 3. Map a continuous variable to color, size, and shape

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty))

# The color becomes a gradient.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = cty))

# Size varies based on the continuous variable, but is still divided into discrete 
# numerical groups.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = cty))

#An error occured because a continuous variable cannot be mapped to shape

# 4. Mapping the same variable to multiple aesthetics
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = class, shape = class)
#An error message appears

# 5. 
ggplot(data = mpg) + 
geom_point(mapping = aes(x = displ, y = hwy), stroke = .1)
#Stroke changes the size of the dots on the plot
#geom_point(mapping = NULL, data = NULL, stat = "identity",
# position = "identity", ..., na.rm = FALSE, show.legend = NA,
# inherit.aes = TRUE)

# 6. 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), colour = displ < 5)
# There is an error because displ is not an object. Error in layer(data = data, 
# mapping = mapping, stat = stat, geom = GeomPoint,  : 
# object 'displ' not found

#### Facets ####

#The command to facet plot by a single variable is facet_wrap()
# variable should be discrete
# To facet your plot on the combination of two variables, add facet_grid() to your plot call.

#### 3.5.1 Exercises ####

# 1.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = cty)) + 
  facet_wrap(~ hwy, nrow = 2)
#It makes a plot of the data divided by number categories.

# 2. 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl)) +
  facet_grid(drv ~ cyl)
# The empty cells are where the cyllinders and the drive functions do not correlate

# 3. 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

# The dot makes it so that the data is displyed in rows

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

# The dot in this code makes it so that the data is displayed in columns

# 4.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
# The facet feature makes it easier to see the distinctions in the datasets, but it
# also makes it harder to compare the data in separate boxes. 
# In order to balance this change with a sufficiently large dataset you could use both 
# aesthetics

# 5. 
?facet_wrap
# nrow is the number of rows and ncol is the number of columns
# as.table allows you to lay out the table either with highest values at the bottom 
# right or highest values at the top-right
# The switch function changes where the labels are displayed
# dir changes the direction 
facet_grid()
# This does not need to have nrow and ncol becuase this function uses the number of 
# rows and columns in the dataset to determine these aesthetics of the plot

# 6. 
# Putting the variable with more unique levels in columns makes the data more readable
# because it will be easier to see how they are arranged along the x-axis of fewer variables

#### Geometric Objects ####

# a geom is the geometrical object that a plot uses to represent data
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# change from point to smooth line
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))
# Using different line types to separate data
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

# Displaying data in groups with lines of different colors

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE)

# Overlaying different geoms
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# Avoiding errors by passing a set of mappings to ggplot()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# If you place the mappings in a geom function, they will be treated as local
# mappings for that layer
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

# In a similar way, you can specify data for each layer with the filter function
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

#### 3.6.1 Exercises ####

# 1. 
# Line chart: geom_smooth
# boxplot: geom_boxplot
# Histogram: geom_histogram
# Area chart: geom_area

# 2. 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)
# There are three color groups because it is grouped by drv
# There are smooth lines geom_smooth and points from geom_point
# hwy is the y axis and displ is the x axis

# 3.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)
  show.legend = FALSE
# It populates in the environment under mpg
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
    geom_point() + 
    geom_smooth(se = FALSE)
# Removing it once it has already been run has no effect

# 4. 
# Display confidence interval around smooth (TRUE by default, see level to control.)

# 5. 
# These graphs will look the same, the only difference is the added room for error 
# in the second one due to a lack of mapping
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))
# 6.
#  1
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()
#  2
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
#  3
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +  
  geom_point() +
  geom_smooth()
#  4
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) + 
  geom_smooth()
#  5
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(mapping = aes(linetype = drv))
#  6
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = drv))

### Statistical Transformations ####

# Basic Bar Chart
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

geom_bar()

?geom_bar

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

# Overriding default stat
demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

# Overriding the default mapping from transformed variables to aesthetics
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

# Drawing greater attention to the transformation
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median)

#### 3.7.1 Exercises ####

# 1. 
stat_summary()
# The default geom is geom_pointrange
# This can be changed by typing geom_bar or other geom in front of the mapping

# 2.
?geom_col()
# geom_col makes the heights of the bars represent values in the data whereas 
# geom_bar makes the heights of the bara proportional to the number of cases in each
# group

# 3.
# geom_bar and stat(prop), stat = "identity"
# geom_smooth() and stat_smooth()

# 4.
?stat_smooth()
# Helps in visualizing data which is prone to overplotting

# 5.
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop..))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop..))
?group
# The problem with these graphs is that they both show the data as all having the 
# same prop because they are not discrete variables.

#### Position Adjustments ####

# Color aesthetic or fill to make colored bar charts

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

# Mapping to another variable creates stacked bar graph
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))
# This is because of position adjustment, to avoid stacking use identity, dodge 
# or fill

#identity (more useful for 2D geoms)
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

#fill (makes each set of stacked bars the same height)
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

#dodge(overlapping objects side-by-side)
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

#Overplotting makes it hard to see where mass of the data is.
# to avoid this; position = jitter (adds random noise at each point)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
# less accurate at small scales, more revealing at large scales
# shortcut > geom_jitter()

# learn more about position adjustment:
?position_dodge
?position_fill
?position_identity
?position_jitter
?position_stack.

#### 3.8.1 Exercises ####

# 1. 
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()
# The problem is that many of the points appear to overlap
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter()
# Now we can see where the massing occurs in the data

# 2. 
?geom_jitter()
# The width is default at 40% of the resolution of the data

# 3.
?geom_count()
# geom_jitter() is more about visualizing difference in the data
# geom_count() gives you information about what overplotting covers up
?geom_count()

