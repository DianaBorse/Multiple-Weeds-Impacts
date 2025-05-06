### Lab 4. Data manipulation and graphing ####

# Answers to lab

# R for Data Science, Chapter 3 ####
# https://r4ds.had.co.nz/data-visualisation.html
# Enter your code here

# Tidyverse book Ch.3 (online version, actually Ch.1 in text version)
rm(list = ls())

### 3.1 Introduction ####

#install.packages("tidyverse")
library("tidyverse")
if(!require(nycflights13)){install.packages("nycflights13")}
if(!require(gapminder)){install.packages("gapminder")}
if(!require(Lahman)){install.packages("Lahman")}
tidyverse_update()

### 3.2 First steps ####
mpg
?mpg
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
?geom_point
# ggplot template
# ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

# 1.  You see the dimensions of the tibble, 234 rows by 11 columns, and the first 10 rows of data
# 2.  234 rows and 11 columns and the type of variable: chr, dbl, int
# 3.  Whether the vehicle in question is f = front-wheel drive, r = rear wheel drive, 4 = 4wd

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy))

# 4.  Since you are plotting two categorical variables, the plot is not informative.  You simply have a dot
#     at the intersection of each combination in which cars exist (e.g., there are 4wd suvs but not front 
#     wheel drive suvs)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = class))


### 3.3. Aesthetic mappings ####
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# 1.  The points are not blue because the end parentheses should be after y = hwy not after color = "blue"
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

# 2.  When I map a continuous variable to color, I get a legend that shows a gradient with light blues for
#     larger numbers and dark blues for lower numbers.  When I map a continuous varialbe to size, smaller
#     values have smaller size points.  When I map a continuous variable to shape, R throws up an error
#     "Error: A continuous variable can not be mapped to shape"
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = cty))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = cty))

# 3.  If you look at the ?mpg help file, you can tell which are continuous by the units offered, but that
#     still a bit of a guessing game.  It is easier to look at the words in < > under the variable names
#     chr indicates a categorical variable, dbl indicates numerical continuous, and int for integer
#     indicates numerical discrete.  Since there are more than 8 years, that could be treated as continuous.
#     Other integer variables like cyl are basically categorical, where the numbers 4, 5, 6, and 8 are 
#     different groups.

# 4.  Below I mapped cty to color and size.  It doesn't break the graph, but neither is it necessary!  You
#     also get two legends, which looks dorky.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty, size = cty))

# 5.  Stroke is thickness of the line outlining a shape.  As the vignette ggplot2-specs states, "Note that 
#     shapes 21-24 have both stroke colour and a fill. The size of the filled part is controlled by size, 
#     the size of the stroke is controlled by stroke. Each is measured in mm, and the total size of the 
#     point is the sum of the two
?geom_point
# Looking up specs in the package ggplot2
??ggplot2::specs
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class, stroke = cyl))
# R hates this because only 21-24 can do stroke.  If I instead made class different colors and specify
# that all points are shape = 21, a circle, R is happier, although the plot is HIDEOUS.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class, stroke = cyl), shape = 21)

# 6.  Because displ < 5 is a logical statement, it has two possible outcomes: FALSE and TRUE.  The color
#     blue is assigned to TRUE and the color red is assigned to FALSE
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = displ < 5))

### 3.4. Common Problems ####
ggplot(data = mpg) 
+ geom_point(mapping = aes(x = displ, y = hwy))


### 3.5.  Facets ####
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

# 1.  If I facet on an integer that behaves like a continous variable like cty I get a different plot for
#     every blessed value of cty.  If I facet on a truly continuous variable like displ it does the same
#     thing for every value.  In this dataset it ends up looking not insane, but if you had a continuous
#     variable in which every observation was unique, then you would have one point per plot.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ cty)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cty, y = hwy)) + 
  facet_wrap(~ displ, nrow = 2)

# 2.  Empty cells mean that there are no cars with, for example 4wd and 5 cylinders.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))

# 3.  If the facet grid formula is drv ~ . that means organize drive vertically.  If the facet grid formula
#     is . ~ cyl, that means organize cyl types horizontally. 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

# 4.  Faceting is not very useful when some of the categories that you are faceting by (in this case 2seater)
#     don't include very many data points.  Color might be more useful because then the overall pattern
#     between hwy and displ would not be lost.  If there were more observations, then faceting can help
#     pick out differences in the hwy ~ displ relationship among car types that would be harder to see 
#     in a single graph
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# 5.  nrow and ncol define how many rows of plots and columns of plots will be shown.  You can also use the
#     facets argument if you want, which looks like either ~cyl or vars(cyl).  When using facet_grid, the 
#     number of rows is defined by a variable like cyl, which is why you don't give an actual number like 2.

?facet_wrap
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ cyl)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(vars(cyl))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))+
  facet_wrap(vars(cyl,drv))


# 6.  You put the variable with more levels in columns so you don't end up with a tall skinny group of plots.
#     Basically, it is a better use of space.

### 3.6.  Geometric objects ####

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

?geom_smooth
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

# 1.  Depending on what you want, you can use geom_line for a jagged line like a time series, geom_abline
#     for a straight line specified by slope and intercept, geom_path to trace follow from timepoint 1 to 
#     timepoint 2, to 3 etc.

p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
# Calculate slope and intercept of line of best fit
coef(lm(mpg ~ wt, data = mtcars))
p + geom_abline(intercept = 37, slope = -5)
# But this is easier to do with geom_smooth:
p + geom_smooth(method = "lm", se = FALSE)

# geom_line() is suitable for time series
ggplot(economics, aes(date, unemploy)) + geom_line()
ggplot(economics_long, aes(date, value01, colour = variable)) +
  geom_line()
# geom_path lets you explore how two variables are related over time,
# e.g. unemployment and personal savings rate
m <- ggplot(economics, aes(unemploy/pop, psavert))
m + geom_path()
m + geom_path(aes(colour = as.numeric(date)))

# 2.  Honestly, this happened in your head.

# 3.  show.legend = FALSE removes the legend.  It was used earlier in the chapter because three graphs
#     were presented together and they did not use the same legend.

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

# 4.  According to the handy dandy help file, se argument will "Display confidence interval around smooth? 
#     (TRUE by default, see level to control.)
?geom_smooth

# 5.  They do not look different because both use the same mappings, the same x variable and the same y
#     variable.  The only difference is the first puts that info in the first line, and the second has
#     to specify that information in both the second and third line.  The first is more efficient.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# 6. 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(size = 5)+
  geom_smooth(se = FALSE, size = 2)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(size = 5)+
  geom_smooth(mapping = aes(group = drv), se = FALSE, size = 2)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv), size = 5)+
  geom_smooth(mapping = aes(group = drv), se = FALSE, size = 2)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv), size = 5)+
  geom_smooth(se = FALSE, size = 2)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv), size = 5)+
  geom_smooth(mapping = aes(group = drv, linetype = drv), se = FALSE, size = 2)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point( size = 5, stroke = 2, color = "white")+
  geom_point( mapping = aes(color = drv), size = 3)


### 3.7. Statistical transformations ####
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

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

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# 1.  The default geom seems to be pointrange
?stat_summary
?geom_pointrange

# For the life of me, I cannot do this without resorting to creating a new table

diamonds_summary <- diamonds %>%
  group_by(cut) %>%
  summarise(min_depth = min(depth),
            max_depth = max(depth),
            med_depth = median(depth))
ggplot(data = diamonds_summary, mapping = aes(x = cut, y = med_depth)) +
  geom_pointrange(mapping = aes(ymin = min_depth,
                                ymax = max_depth) )

# 2.  geom_bar() makes the height proportional to the number of cases in each group.  In other words,
#     geom_bar() shows the count of each group in the dataset.  So if you had a dataset with groups
#     a, b, c, and d and each there were 10 observations in each group, each bar would be 10 high.
#     geom_bar() only requires a single categorical variable
#     geom_col() requires a categorical variable and a continuous variable and shows group sums.

ggplot(data = mpg, aes(x = fl))+
  geom_bar()
ggplot(data = mpg, aes(x = fl, y = hwy))+
  geom_col()

summ_hwy <- mpg %>%
  group_by(fl) %>%
  summarise(sum = sum(hwy),
            mean = mean(hwy))

# 3.  So I'll do this for the ones we have used in class so far:
#     geom_histogram, stat = "bin"
#     geom_bar, stat = "bar"
#     geom_col stat = "count"
#     geom_point stat = "identity"
#     geom_pointrange stat = "identity"
#     In terms of what they have in common, it is what R is calculating behind your back and then displaying


# 4.  stat_smooth() is calculating a predicted value for y and then a confidence interval around that
#     point by point.  The predicted value of y comes from a fit equation.  The default fitting method is
#     method = "loess" which according to Wikipedia is "LOESS curve fitted to a population sampled from a 
#     sine wave with uniform noise added." Personally, I find it a little vile and rarely what I want.

#   INSTEAD, for a linear regression, we use method = "lm" which stands for linear model. 
#     the equation of the best fit line allows us to plug in each value of x and then get the predicted y.
#     So for example, in the graph below, I'm plotting a simple dataset looking at the relationship
#     between rainfall and biomass.  I write the equation in gthe form y ~ x, or biomass ~ rainfall.
environment <- read_csv("datasets/r4all/environment.csv")
plot01 <-ggplot(data = environment, mapping = aes(x = rainfall.m, y = biomass.g.per.m2))+
  geom_point()
plot01
# When I fit the linear model, biomass.g.per.m2 ~ rainfall.m, part of the output is the slope and intercept
model01 <- lm(biomass.g.per.m2 ~ rainfall.m, environment)
model01
# I can create a new column in the datatable called predicted OR, lazy way, just plot the fitted values
plot02 <- plot01+
  geom_point(mapping = aes(x = environment$rainfall.m, y=model01$fitted.values), color = "red")
plot02

# Each of those fitted values has its own 95% confidence interval!  Then R connects the C.I.s and makes
# a nice smooth curve, so long as I specify the smoothing method as lm.

plot03 <- plot02+
  geom_smooth(method = "lm")
plot03

# Of course, the default method is this messy looking loess fit instead of a pretty linear fit.  
plot04 <- plot02 +
  geom_smooth()
plot04

# 5.  What is happening is your bars are showing groupwise proportions on the y axis, which is not at all
#     useful because the number of fair cut diamonds/ total number of fair cut diamonds is 1.00.
#     Similarly with geom_bar, each bar shows the number of color D diamonds of certain cut/ total number
#     D diamonds of a certain cut and is just not at all useful.  It is a stack of 1.00 on top of each other.
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop..))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = color))


### 3.8.  Position adjustments ####
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))



ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
?position_dodge
?position_fill
?position_identity
?position_jitter
?position_stack

#1.  This plot has too many overlapping data points and could be improved with jitter
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cty, y = hwy), position = "jitter")
  

#2.  width and height control jitter in the geom_jitter()
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter(width=0.2, height = 0)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter(width = 0, height=0.2)
?geom_jitter
?geom_count

#3.  geom_jitter makes overlapping points more visible by jittering them slightly.
#    geom_count  makes the point size scale with the number of points that overlap.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_jitter()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_count()

#4.  The boxplots are ordered alphabetically by category and use the position = "dodge"
ggplot(data = mpg, mapping = aes(x = drv, y = hwy)) +
  geom_boxplot(aes(color=class))

### 3.9.  Coordinate systems ####

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()
install.packages("maps")
library("maps")
nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

#1.  
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut, fill = cut))+
  coord_flip()
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut, fill = cut))+
  coord_polar()

#2. labs() allows you to specify axis labels, titles, subtitles, etc.
?labs

#3.   coord_map is a projection that preserves straight lines and is computationally intensive, whereas
#     coord_quickmap is, well, quicker, but is going to be very distorted near the poles.

#4.  The plot shows that across the board, all cars had higher highway miles per gallon than city miles 
#    per gallon.  The line is a 1:1 line and the coord_fixed() keeps the x and y axis scales the same so
#    the line is at a perfect 45 degree angle.


### 3.10.  The layerd grammar of graphics ####

# Template
# ggplot(data = <DATA>) + 
# <GEOM_FUNCTION>(
#    mapping = aes(<MAPPINGS>),
#    stat = <STAT>, 
#    position = <POSITION>
#  ) +
#  <COORDINATE_FUNCTION> +
#  <FACET_FUNCTION>




