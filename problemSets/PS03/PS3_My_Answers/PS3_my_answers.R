#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

install.packages("stargazer")
library(stargazer)
install.packages("brant")
library(brant)



# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)
typeof(gdp_data)

################
#Q1 part 1
################


# Initialize GDP_cat as a character vector with "NA" as placeholder
gdp_data$GDP_cat <- rep("NA", nrow(gdp_data)) 

# Create category 
for (i in 1:nrow(gdp_data)){
  if (gdp_data$GDPWdiff[i] > 0) {
    gdp_data$GDP_cat[i] = "positive"
  }
  else if (gdp_data$GDPWdiff[i] < 0){
    gdp_data$GDP_cat[i] = "negative" 
  }
  else gdp_data$GDP_cat[i] = "no change"
}
# Set the ref category to no change
gdp_data$GDP_cat <- relevel(as.factor(gdp_data$GDP_cat), ref = "no change")

#Run regression
mult.log <- multinom(GDP_cat ~ REG + OIL, data = gdp_data)
summary(mult.log)

stargazer(mult.log, type = "latex")

# First I create new variable for categories of "no change" "negative" and "positive"
#   make it a factor, re order it, and set "no change as ref category. 

# Intercept negative: When the regime is not a democracy and the fuel export ratio is 
#  below 50% the log odds of changing from "no change" to "negative" is approximately 3.80
#   This value is not statically significant.

# Intercept positive: When fuel export is below 50% and regime is non democracy the 
#   estimated log odds of moving from "no change" to "positive" is approximately 4.53.
#   This value is not statically significant.

# Holding OIL constant, a change from not democracy to democracy is associated with an
#   estimated log odds of moving from "no change" to "negative" of approximately 1.38. 
#   This value is not statically significant.

# Holding OIL constant, a change from not democracy to democracy is associated with an
#   estimated log odds of moving from "no change" to "positive" of approximately 1.78. 
#   This value is not statically significant.

# Holding REG constant, a change from fuel export ratio from below 50% to above
#   50% is associated with an estimated log odds of moving from "no change" to "negative"
#   of approximately 4.73.
#   This value is not statically significant.

# Holding REG constant, a change from fuel export ratio from below 50% to above
#   50% is associated with an estimated log odds of moving from "no change" to "positive"
#   by approximately 4.58.
#   This value is not statically significant.


################
#Q1 part 2
################

# Let me check if order makes sense to use an ordered model
# Set the ref category to no change
gdp_data$GDP_cat <- relevel(as.factor(gdp_data$GDP_cat), ref = "negative")
#Run regression
mult.log <- multinom(GDP_cat ~ REG + OIL, data = gdp_data)
summary(mult.log)

# IGNORE ALL THIS STUFF
# "no change" to "positive" should be about the same as a move from "negative" to "no change" in this
# new model where negative is the ref category. However the coefficient for oil does not remain 
# consistent. In the second model a shift from in OIL from 0 to 1 is associated with a shift in
# log odds of moving from "negative" to "no change" by "-7.9240683". In the fist model a change 
# from OIL 0 to 1 is associated with a log odds shift of moving from "no change" to "positive" 
# by 4.576321 This means the effect of OIL is not consistent between categories and an ordered
# model should not be used.


# I try the ordered model anyway
## Ordered Logistic Regression (change the lowest category to reference)
gdp_data$GDP_cat <- relevel(as.factor(gdp_data$GDP_cat), ref = "negative")
ord.log <- polr(GDP_cat ~ REG + OIL, data = gdp_data, Hess = TRUE)
summary(ord.log)

#Holding OIL constant, a change from not democracy to democracy is associated with an
#   estimated log odds of moving from up one category (negative -> no change -> positive) 
#   by approximately 0.40.

#Holding REG constant, a change from fuel export ratio from below 50% to above
#   50% is associated with an estimated log odds of moving one category (negative -> no change -> positive)
#   by approximately -0.1987.

# Our intercepts are the "cutoff points" where we change from one category to another
# negative|no change: -0.7312 is the estimated cut point between negative and no change when all coefficients equal zero.
# It is the log odds of being in "negative" compared to "no change" or positive" when all coefficients are at zero.
# no change|positive: -0.7105 is the estimated cut point between negative and no change when all coefficients equal zero
# It is the log odds of being in "negative" or "no change" compared to being in positive" when all coefficients are at zero.

# Ill take it a step further and test parallel lines regression assumption. We need to ensure that 
# the relationship between our predictors is the same between each category. I can use this function.
install.packages("brant")
library(brant)
brant(ord.log)

####################################################################################
# Problem 2
####################################################################################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")


#Plot
# Assuming x and y are the names of your variables

# Make a plot excluding the one out liar of 35 visits
plot(jitter(as.numeric(mexico_elections$competitive.district)), jitter(mexico_elections$PAN.visits.06), 
     ylim = c(0, 5), main = "Jitter Plot",
     xlab = "Competitive", ylab = "Visits", pch = 20, col = "blue")



################
#P 1
################
mex_mod <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
               data = mexico_elections, family = poisson())
summary(mex_mod)
stargazer(mex_mod, type = "latex")
# We do not find statistically significant evidence that the to support the alternate hypothesis  
#   that candidates visit swing districts more than than not (we fail to reject the NULL). The p-value for
#   the "competitive.district" variable is 0.6336 (p>.05) and the test statistic is -0.477.

################
#P 2
################
# interpreting outputs Get coefficeints
cfs <- coef(mex_mod)
exp(cfs)

exp(-2.08014)
# marginality.06: Holding all other variables constant and a one unit increase in
#   marginality.06 is associated with and estimated decrease the expected number of 
#   visits by a multiplicative factor of exp(-2.08014) = 0.1249127. This value
#   has high statistical significance.

# A one unit increase in marginality is associated with a % decrease in in the probability
#   of a visit to the district.

exp(-0.31158)
#PAN.governor.06: Holding all other variables constant a change from PAN governor from 0 to 1
#   is associated with and estimated decrease the expected number of 
#   visits by a multiplicative factor of exp(-0.31158) = 0.732289.
#   This value is not statistically significant.


################
#P 3
################

# Estimated mean number of visits. 
exp(cfs[1]*1 + cfs[2]*1 + cfs[3]*0 + cfs[4]*1) # 0.01494818
# Above is a linear equation which includes the coefficients at their given values including the intercept.
# It is estimated that he will make 0.01494818 visits under the given circumstances
#   so he probably won't visit :(


#BONUS 
# An assumption of poisson regression is the mean is the same (or close to) variance lets take a quick look
# See if mean = variance in the response variable
with(mexico_elections,
     list(mean(PAN.visits.06), var(PAN.visits.06))) # do we meet assumptions for Poisson?

# mean 0.09181554

#variance 0.6436861

# We need to do further test to make sure but it doesn't like the mean and variance are close enough.
# Also, when I check how many zeros there are it doesn't look good!
table(mexico_elections$PAN.visits.06)




























