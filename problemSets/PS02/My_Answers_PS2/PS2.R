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
install.packages("dplyr")
library(dplyr)
install.packages("stargazer")
library(stargazer)


lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##########################################
# Problem 1
##########################################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
ls()
data = climateSupport
View(data)

# Re order the levels and change reference category 
data$countries <- as.character(data$countries)
data$countries <-  c("20 of 192", "160 of 192", "80 of 192"))
data$countries <- factor(data$countries)
data$countries <- relevel(data$countries, ref = "20 of 192")

# Do it again for sanctions 
data$sanctions <- as.character(data$sanctions)
data$sanctions <-  c("None", "5%", "15%", "20%")
data$sanctions <- factor(data$sanctions)
data$sanctions <- relevel(data$sanctions, ref = "None")


add_mod <- glm(choice ~ countries + sanctions, family = binomial(link = "logit"), data = data)
summary(add_mod)
stargazer(add_mod, type = "latex")



# Ho: There is no statistically significant association between either of the two independent variables (number of countries who participate and sanctions).


#(Intercept) -0.26597:  When sanctions are "None" and 20 countries participate, the legislation 
#   the estimated expected odds of the person supporting the legislature is exp(-0.005665) = 0.7664621 (baseline odds ratio)
#   This value has high statistical significance with a p value of 6.08e-07.
exp(-0.26597)

#Countries160 of 192  0.64568: When sanctions are "None" and there is a change from 
#   20 to 160 countries participate, the log odds of a person supporting the
#   legislation increase by 0.64568. 
#   This value has high statistical significance with a p value of < 2e-16 (almost zero).

#Countries80 of 192  0.32106: When sanctions are "None" and there is a change from 
#   20 to 80 countries participate, the legislation the log odds of a person supporting the
#   legislation increase by 0.32106. 
#   This value has high statistical significance with a p value of 2.03e-09.


#sanctions20%  -0.12203: When 20 countries participate, the legislation and sanctions are move from 0% to 20% the 
#     log odds of a person supporting the legislation decreases by -0.12203. This value is statistically significant.
#     This value is statistically significant with a p value of 0.0488.


#sanctions15%  -0.02222: When 20 countries participate, the legislation and sanctions are move from 0% to 20% the 
#     log odds of a person supporting the legislation decreases by -0.02222. This value is not statistically significant.
#     This value is not statistically significant with a p value of 0.7197.


#sanctions5%  -0.10383: When 20 countries participate, the legislation and sanctions are move from 0% to 5% the 
#     log odds of a person supporting the legislation decreases by --0.10383. 
#     This value is not statistically significant in most cases with a p value of 0.0936.



##########################################
# Problem 2
##########################################

######
#2A
######
# Change the reference category 
data$countries <- relevel(data$countries, ref = "160 of 192")
# Do it again for sanctions 
data$sanctions <- relevel(data$sanctions, ref = "5%")
# Run the model again
add_mod <- glm(choice ~ countries + sanctions, family = binomial(link = "logit"), data = data)
summary(add_mod)

#When the number of countries who participate is 160 and the sanctions
#   change from 5% to 15% the estimated change in log odds that the person will support the 
#   policy is 0.08161. This equates to an odds ratio of exp(0.08161) = 1.085033. This means that 
#   a change from 5% to 15% in sanctions is estimated to increase the odds that a person will support 
#   the policy by approximately 8.5%. 1.085033 - 1 = .085 and 0.085*100 = 8.5%
#   However, this value is not statistically significant with a p value of 0.1874.
exp(0.08161)

######
#2B
######

# Change the reference category 
data$countries <- relevel(data$countries, ref = "80 of 192")
# Do it again for sanctions 
data$sanctions <- relevel(data$sanctions, ref = "None")
# Run the model again
add_mod <- glm(choice ~ countries + sanctions, family = binomial(link = "logit"), data = data)
summary(add_mod)
stargazer(add_mod, type = "latex")


# When 80 countries participate and there are no sanctions the estimated expected odds of the person 
#   supporting the legislature is exp(0.05509) = 1.056636 (baseline odds ratio). To find probability 
#   I use the formula 1.056636/(1+1.056636). The estimated probability is approximately 51%.
#   This value is not statistically significance with a p value of 0.3069.
exp(0.05509)
1.056636/(1+1.056636)

######
#2C
######
int_mod <- glm(choice ~ countries * sanctions, family = binomial(link = "logit"), data = data)
summary(int_mod)
stargazer(int_mod, type = "latex")


compare <- anova(add_mod, int_mod , test = "LRT")
print(compare)
stargazer(compare, type = "latex")

#Since the p value of 0.9307 is not statistically significant 

