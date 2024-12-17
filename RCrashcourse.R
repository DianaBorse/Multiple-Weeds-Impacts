install.packages("tidyverse")

install.packages("devtools")

10 + 2 #add

10 - 2 # subtract

10 * 2 # multiply

10 / 2 # divide

10 ^ 2 # exponent

1 %% 2 # moduLo

10 %% 2 # moduLo

c(1,2,3,4) # concatenate

abs(-10) # absolute value

sqrt(10-1) # square root (with subtraction)

log(10) # natural log

log10(10) # log base 10

exp(1) # power of e

sin(pi/2) # sine function

asin(1) # inverse sine

cos(pi) # cosine

acos(-1) # inverse cosine

tan(0) #tangent

atan(0) # inverse tangent

pi

round(pi, digits=3) # standard rounding to 3 digits

floor(pi) # round down to the closest whole number

ceiling(pi) # round up to the closest whole number

signif(pi, digits=2) # round to keep 2 significant digits

1 > 2 # greater than

1 < 2 # less than

1<= 2 # less than or equal to

1 == 1 # equal to

1 == 2 | 1 == 1 # "|" means or

1 == 2 & 1 == 1 # & means and

1 != 1 # not equal to

?round

1 %in% c(1,2,3,4,5,6,7,8,9,10) # the %in% stands in for | with concatenated objects

runif(n=10, min=0, max=1) # random number generation (uniform)

rnorm(10, mean=0, sd=1) # gaussian distribution, the 10 is the n value, you don't

rpois(10, lambda = 10) # Poisson distribution for discrete values, lambda is

rbinom(10, size = 1, prob = 0.5) # binomial distribution for discrete variables

rbinom(10, size = 10, prob = 0.5) # binomial distribution for discrete variables

rep(c("Treatment", "Control"), 3)

rep(mean(rnorm(1000)),3) # sampling from a normal distribution and calculating

rep(mean(rnorm(1000)),3) # sampling from a normal distribution and calculating

replicate(3,mean(rnorm(1000)))

replicate(3,mean(rnorm(1000))) # this one actually replicates the function

sample(c(1:10),10,replace=T) # sample from a vector with replacement

sample(c(1:15),10,replace=F) # sample from a vector without replacement

runif(5) # without seed

runif(5)

set.seed(3)

runif(5)

set.seed(172834782)

runif(5)

c(1,2,5)

c(c(1,2,5),c(48,49,50))

c(c(1,2,5),c(48,49,50)) # can nest concatenates

c(1:10)

c(1:100)

c(100:90)

c(-1:1)

c((-1:1), digits = 2)

c(round(-1:1), digits = 2)

round(c(-1:1), digits = 2)

round(c(-1:1), digits = 3)

signif(c(-1:1), digits = 3)

1 %in% c(-1:1)

runif(10,c(-1:1))

runif(10,c(-1:1))

10 %% 2 # moduLo

c((-1:1), digits = 3)

c((-1:1), digits = 3)

c((-1:1), value= 3)

c((-1:1), n = 3)

c((-1:1),10)

sample(c(-1:1), 10)

sample(c(-1:1), 10, replace = false)

sample(c(-1:1), 10, replace = F)

c((-1:1), sd = 10)

c((-1:1), size = 10)

c((-1:1), n = 10)

c(n = 10(-1:1))

seq(-1, 1, by = 0.1)

seq(-1, 1, length=7)

cbind(1:10,10:1)

rbind(1:10,10:1)

x<-"string"

print(x)

paste(x)

x

(x<-"string")

Xvec[1]

c(1,2,3,1.23)

Xvec[1]

Xvec<-c(1.1829378, X, 1:10, "E", "Computational Biology", 100:90)

x<-"string"

Xvec<-c(1.1829378, X, 1:10, "E", "Computational Biology", 100:90)

Xvec<-c(1.1829378, x, 1:10, "E", "Computational Biology", 100:90)

Xvec[1]

Xvec{13}

Xvec[13]

xvec[1:3]

Xvec[1:3]

Xmat<-matrix(Xvec,nrow=5)

print(Xmat)

10 + 2 # add

10 - 2 # subtract

10 * 2 # multiply

10 / 2 # divide

10 ^ 2 # exponent

10 %% 2 # moduLo

c(1,2,3,4) # concatenate

abs(-10) # absolute value

sqrt(10-1) # square root (with subtraction)

log(10) # natural log

log10(10) # log base 10

exp(1) # power of e

sin(pi/2) # sine function

asin(1) # inverse sine

cos(pi) # cosine

acos(-1) # inverse cosine

tan(0) # tangent

atan(0) # inverse tangent

round(pi, digits=3) # standard rounding to 3 digits

floor(pi) # round down to the closest whole number

ceiling(pi) # round up to the closest whole number

signif(pi, digits=2) # round to keep 2 significant digits

1 > 2 # greater than

1 < 2 # less than

1<= 2 # less than or equal to

1 == 1 # equal to

1 == 2 | 1 == 1 # "|" means or

1 == 2 & 1 == 1 # & means and

1 != 1 # not equal to

?round

1 %in% c(1,2,3,4,5,6,7,8,9,10) # the %in% stands in for | with concatenated objects

runif(n=10, min=0, max=1) # random number generation (uniform) n is number of

rnorm(10, mean=0, sd=1) # gaussian distribution, the 10 is the n value, you don't

rpois(10, lambda = 10) # Poisson distribution for discrete values, lambda is

rbinom(10, size = 10, prob = 0.5) # binomial distribution for discrete variables

rep(c("Treatment", "Control"), 3) # repeat function repeats values

rep(mean(rnorm(1000)),3) # sampling from a normal distribution and calculating

replicate(3,mean(rnorm(1000))) # this one actually replicates the function

sample(c(1:10),10,replace=T) # sample from a vector with replacement

sample(c(1:15),10,replace=F) # sample from a vector without replacement

runif(5) # without seed

set.seed(3) # seed adds reproducibility

runif(5)

set.seed(172834782)

runif(5)

c(1,2,5)

c(c(1,2,5),c(48,49,50)) # can nest concatenates

c(1:10) # can concatenate a range with a :

c(100:90)

seq(-1, 1, by = 0.1) # more complicated combination

seq(-1, 1, length=7)

cbind(1:10,10:1) # makes columns

rbind(1:10,10:1) # makes rows

x<-"string"

print(x) # super basic way to look at bits of complex scripts

paste(x) # converts objects to a string

x

(x<-"string") # both assigns the value to x and prints it

c(1,2,3,1.23)

Xvec<-c(1.1829378, x, 1:10, "E", "Computational Biology", 100:90)

Xvec[1] # subsetting a vector, call out the cell you want from the vector

Xvec[13]

Xvec[1:3]

Xmat<-matrix(Xvec,nrow=5) # converted the vector to a matrix. When this happens
# it simply kept the same order (moving from top to bottom) and spread the
# values out over 5 columns

print(Xmat)

Xmat[1,3] # call for the third value in the first row, should be 9

Xmat[1,] # call for the whole first row

Xmat[,3] # call for the whole third column

Xarray<-array(0, dim=c(3,3,2)) # making an array with 3 dimensions

Xarray

Xarray[1:2,1:2,1,1,1,1] # making an array with 6 dimensions

Xarray<-array(rnorm(64), dim=c(2,2,2,2,2,2)) # making an array with 6 dimensions

Xarray

Xarray[2,2,1,1,1,1]

Xarray[2,1,1,2,2,1]

Xarray[1:2,1:2,1,1,1,1]

x<-c(1:10)

x

y<-c((1:10)*0.5)

y

x*y

x+y # add each value of x to each value of y

x/y # divide each value of x by each value of y

x^y # raise each value of x to the power of each value of y

log(x)

log(x) # natural log of x

exp(y)

getwd()

setwd("C:/Users/bella/Documents/Thesis") # sets working directory on an

setwd("./Analysis")

getwd()

setwd("..")

getwd()

setwd("./Analysis")

getwd()

#### Working with a data set ####

library(readr)
FallopiaData <- read.csv("Data/FallopiaData.csv",header=T)

names(FallopiaData) # gives the column names

head(FallopiaData) # show the first 6 rows of data

tail(FallopiaData) # show the last 6 rows of data

dim(FallopiaData) # show the dimension (number of rows and columns)

nrow(FallopiaData) # show the number of rows

ncol(FallopiaData) # show the number of columns

str(FallopiaData) # look at the structure of the data, i.e. the types of data 
# in each column

## can subset dataframes as well
FallopiaData[1,] # returns the first row of data

FallopiaData[1,1] # returns the first value of the data fram

FallopiaData[1:4, "PotNum"] # can show a specific column, this is also subset by
#the first 4 values in the column

FallopiaData$PotNum[1:4] # shortcut of the previous way to subset

subset(FallopiaData,Scenario=="low" & Total > 60) # subset by the values in the 
# columns

FallopiaData$NewTotal<-FallopiaData$Symphytum + FallopiaData$Silene + FallopiaData$Urtica
# this created a new column that was made up of the sum of the values from 
# symphytu, silene, and Urtica

names(FallopiaData)

print(FallopiaData$NewTotal[1:10]) # print the data from the new column

unique(FallopiaData$Nutrients) # find all of the unique values in a vector

duplicated(FallopiaData$Nutrients) # returns true if duplicated and a false if unique

aggregate(FallopiaData$NewTotal, list(FallopiaData$Nutrients), mean) # calculate
# the means of one column of data for each value of another column within groups

aggregate(NewTotal ~ Nutrients, data=FallopiaData, mean) # another way to run 
# aggregate, the ~ means by. This way of writing it out preserves the column names

aggregate(NewTotal ~ Nutrients:Taxon:Scenario, data=FallopiaData, mean) # calculate
# means across different combinations of tow or more grouping columns

aggregate(NewTotal ~ Nutrients, data=FallopiaData, sd)

tapply(FallopiaData$NewTotal, list(FallopiaData$Nutrients), mean) # similar but 
# uses list function and the group names become columns

lapply(FallopiaData, class)[1:3] # shows the class of the first three columns as
# a list

sapply(FallopiaData, class)[1:3] # shows the class of the first three columns as 
# a vector

class(FallopiaData)

class(FallopiaData$Taxon)

## Calculate Means
NutrientMeans<-tapply(FallopiaData$NewTotal,list(FallopiaData$Nutrients), mean)
## Save means as a csv file
write.csv(NutrientMeans, "FallopiaData_Nutrient_Means.csv", row.names =F)
write.csv(NutrientMeans, "./Data/FallopiaData_Nutrient_Means.csv", row.names=F)

?cor

## Installing the package from the book
install.packages('baRcodeR')

# Loading packages
library(baRcodeR)
make_labels()

baRcodeR::make_labels # Call the make_labels function from the baRcodeR package


