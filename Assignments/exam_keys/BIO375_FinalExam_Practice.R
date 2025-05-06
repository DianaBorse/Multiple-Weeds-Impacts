### Final Exam Practice ###
rm(list = ls())

library("DescTools")
library("ggfortify")
library("multcomp")
library("nlme")
library("broom")
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()

### 33a: Chest Pain ####
# (i)   among people admitted to emergency rooms with heart attacks, frequency of death is independent
#       of having chest pain.

# (ii)  predictor: presence of chest pain (categorical, 2 levels: chest pain, no chest pain)
#       response: survival (categorical, 2 levels: died, survived)

# (iii) chi-squared test of independence

# (iv)  assumption is that <=20% of cells have expected frequency less than 5 is met, smallest
#       expected frequency is 88

tab01 <- matrix(c(822, 229, 18296, 1534), 2, 2, byrow=TRUE)
# Add row names, then column names with the function dimnames()
dimnames(tab01) <- list("Survival" = c("died", "survived"),
                        "Chest Pain" = c("chest pain", "no chest pain"))
as.matrix(tab01)
model_01 <- chisq.test(tab01, correct = FALSE)
model_01$expected

# (v)
model_01

# (vi)
# We found that people admitted to emergency rooms with heart attacks were significantly
# more likely to die if they did not have chest pain (Chi-sq test of Independence: X-sq = 254.99,
# df = 1, p < 0.0001).


### 33b: Cichlids ####
# (i)   the difference in mean GnRH between territorial and non-territorial fish is zero
#       muT - muNT = 0 or muT = muNT

# (ii)  predictor: territorialStatus (categorical, 2 levels: NT, T)
#       response: GnRHmRNALevel (continuous)

# (iii) two-sample t-test, two-sided

# (iv)  Not clear whether fish were randomly sampled from population
#       Non-normality: right skew indicated by outlier and long upper tail in boxplot,
#       histograms hard to interpret with small n, but in same direction, q-q plot looks
#       curvilinear.
#       With respect to equal variance, the ratio of sds is 3.10, so it does not meet the
#       assumption of equal variance.


Cichlid <- read_csv("datasets/FinalPractice/Cichlid.csv")

ggplot(data = Cichlid)+
  geom_boxplot(mapping = aes(x=territorialStatus, y=GnRHmRNALevel))
ggplot(data = Cichlid)+
  geom_histogram(mapping = aes(GnRHmRNALevel), binwidth = 0.2)+
  facet_wrap(~territorialStatus)
ggplot(data = Cichlid)+
  geom_qq(aes(sample = GnRHmRNALevel, color = territorialStatus) )

summ_cichlid01 <- Cichlid %>%
  group_by(territorialStatus) %>%
  summarise(mean = mean(GnRHmRNALevel),
            sd  =  sd(GnRHmRNALevel))
ratio01 <- max(summ_cichlid01$sd)/min(summ_cichlid01$sd)

Cichlid <- Cichlid %>%
  mutate(logGnRH = log(GnRHmRNALevel))

ggplot(data = Cichlid)+
  geom_boxplot(mapping = aes(x=territorialStatus, y=logGnRH))
ggplot(data = Cichlid)+
  geom_histogram(mapping = aes(logGnRH), binwidth = 0.2)+
  facet_wrap(~territorialStatus)
ggplot(data = Cichlid)+
  geom_qq(aes(sample = logGnRH, color = territorialStatus) )

summ_cichlid02 <- Cichlid %>%
  group_by(territorialStatus) %>%
  summarise(mean = mean(logGnRH),
            sd  =  sd(logGnRH))
ratio02 <- max(summ_cichlid02$sd)/min(summ_cichlid02$sd)

# (v)
# Log transforming the data improves normality (makes the whiskers more symmetrical on the box
# plot) and makes the variances more equal (ratio < 3 on transformed data), so we will proceed
# with a two-sample, two-sided t-test 

t.test(logGnRH ~ territorialStatus, data = Cichlid, var.equal = TRUE,
       alternative = "two.sided", conf.level = 0.95)

# (vi)
# Territorial males have higher levels of mRNA for gonadotropin-releasing hormone than non-
# territorial fish(2-sample, 2-sided t-test on ln transformed data: t = -2.9032, df =9, p<0.05).

### 33c: No Smoking Day ####
# (i)   Mean difference in injuries on No Smoking Day and the previous Wednesday greater than
#       or equal to 0 where difference = injuriesBeforeNSD - injuriesOnNSD
#       mud > = 0

# (ii)  This can be answered 2 ways.  
#       Predictor = Day (categorical variable, 2 levels: injuriesBeforeNSD and injuriesOnNSD) and
#       Response = injuries (continuous)
#       OR
#       Response = difference in injuries on No Smoking Day and the previous Wednesday

# (iii) Paired t-test, one-sided

# (iv)  Not clear whether or not sample of years is random or independent
#       Differences appear to be normally distributed based on the median line being central in
#       boxplot, although upper whisker is a little longer than the lower whisker. The 
#       histogram has one peak in the middle and the q-q plot is mostly linear.  Assumption of 
#       normally distributed differences has been met.

nsd <- read_csv("datasets/FinalPractice/NoSmokingDay.csv")
nsd <- nsd %>%
  mutate(diff = injuriesBeforeNSD - injuriesOnNSD)

ggplot(nsd) +
  geom_histogram(aes(diff), binwidth = 10)
ggplot(nsd) +
  geom_boxplot(aes(x = "", y = diff))
ggplot(nsd)+
  geom_qq(aes(sample = diff))

# (v)
t.test(nsd$injuriesBeforeNSD, nsd$injuriesOnNSD, 
       alternative = "less", paired =  TRUE, conf.level = 0.95)

# (vi)
# There were significantly more injuries on No Smoking Day than on the previous Wednesday
# (paired, one-sided t-test: t = -2.447, df = 9, p < 0.05).

### 33d: Worms ####
# (i)   rho = 0

# (ii)  No real predictor because a correlation, response = lifespan (continuous), response =
#       relativeOffspringNumber (continuous)

# (iii) Correlation

# (iv)  Not clear whether the 14 mutations were randomly selected or independent
#       With respect to linear relationship, the dot plot shows a linear enough relationship
#       Bivariate normality: the cloud of points looks elliptical; however, based on the 
#       histograms and  boxplot lifespan is a bit right-skewed. Log transformation made the 
#       whiskers a bit more even.  Could justify correlation using log-transformed data or 
#       Spearman's rank correlation.
worm <- read_csv("datasets/FinalPractice/worm.csv")
ggplot(data = worm) +
  geom_point(mapping = aes(x = lifespan, y = relativeOffspringNumber))
ggplot(data = worm)+
  geom_histogram(aes(lifespan), binwidth = 10)
ggplot(data = worm)+
  geom_histogram(aes(relativeOffspringNumber), binwidth = 10)
ggplot(data = worm)+
  geom_boxplot(aes("", lifespan))

worm <- worm %>%
  mutate(log_life = log(lifespan))
ggplot(data = worm) +
  geom_point(mapping = aes(x = log_life, y = relativeOffspringNumber))
ggplot(data = worm)+
  geom_histogram(aes(log_life), binwidth = 0.2)
ggplot(data = worm)+
  geom_boxplot(aes("", log_life))

# (v)
wormSpear <-cor.test(~ lifespan + relativeOffspringNumber, data = worm,
                      method = "spearman")
wormSpear

# (vi)
# Among worms with a mutation at the daf-2 gene, there was a negative correlation between 
# lifespan and relative offspring number (Spearman's rank correlation: S=726.6, p<0.05, r=-0.60).


### 33e: Mouse oxygen ####
# (i)     Not really a null hypothesis, per se, because it is a random-effects ANOVA

# (ii)    Predictor: mouse (categorical, random)
#         Response: VO2max (continuous)

# (iii)   Random-effects ANOVA

# (iv)    Mice were randomly sampled
#         Basically impossible to see if randomly distributed VO2max within each mouse b/c
#         only two measurements per mouse.  Similarly, can't calculate sd per mouse, so can't
#         calculate ratio of sdmax/sdmin.  Group means are kind of normally distributed,
#         could argue that the VO2max have a left skew.  Could transform, or not.

oxygen <- read_csv("datasets/FinalPractice/oxygen.csv")
summ_O2 <- oxygen %>%
  group_by(mouse) %>%
  summarise(mean_O2 = mean(VO2max),
            n_O2 = n())

ggplot(summ_O2) +
  geom_histogram(aes(mean_O2), binwidth = 0.2)
ggplot(summ_O2) +
  geom_boxplot(aes(x = "", y = mean_O2))
ggplot(summ_O2)+
  geom_qq(aes(sample = mean_O2))

# (v)
model_02 <- lme(fixed = VO2max ~ 1,
               random = ~1|mouse, data = oxygen)
model_02_varcomp <- VarCorr(model_02)
model_02_varcomp
varAmong  <- as.numeric( model_02_varcomp[1,1] )
varWithin <- as.numeric( model_02_varcomp[2,1] )
repeatability <- varAmong / (varAmong + varWithin)
repeatability

# (vi)
# 43% of the variability in the maximal rate of oxygen consumption is due to mouse identity,
# which suggests that the remaining 57% of the variation is due to differences in 
# measurements on the same mouse, 68 days apart.  In other words, the mice vary quite a bit 
# from time point to time point.


### 33f: Mendel's peas ####
# (i)     Proportion of YS:YW:GS:GW is 9/16:3/16:3/16:1/16

# (ii)    Response: Pea type (categorical, 4 levels: YS, YW, GS, GW)

# (iii)   Chi-square goodness of fit

# (iv)    Primary assumption is that no more than 20% of cells have expected frequency < 5
#         To do this, have to calculate by hand or do the analysis.  Smallested expected 
#         frequency is 34 so assumption has been met.  Not clear whether the peas were randomly
#         sampled from all the peas produced by the cross.

peas <- read_csv("datasets/FinalPractice/peas.csv")
# These data are most unfortunately entered. I will actually create my own table using the 
# function tribble().

# tribble() is customised for data entry in code: column headings are defined by formulas (i.e. 
# they start with ~), and entries are separated by commas.
peas_short <- tribble(
  ~phenotype, ~obs_freq, ~exp_prop,
  #--|--|----
  "Yellow smooth", 315, 9/16,
  "Yellow wrinkled", 101, 3/16,
  "Green smooth", 108, 3/16,
  "Green wrinkled", 32, 1/16
)

peas_short
model_03 <-chisq.test(x = peas_short$obs_freq, p = peas_short$exp_prop)
model_03
model_03$expected

# (v) shown above

# (vi) Mendel's pea data do not differ from the expected proportions (Chi-sq goodness of fit:
#      X-sq = 0.47, df = 3, p = 0.9254).

