install.packages("eha")
library(eha)
library(survival)
install.packages("stargazer")
library(stargazer)

setwd("/Users/riccimason99/GitHub/StatsII_Spring2024/problemSets/PS04/PS4_MY_ANSWERS")

## Look at Data
data(child)
View(child)
head(child)
names(child)
print(head(child$m.age))
print(head(child$sex))

class(child$sex)
head(child$sex)
as.integer(head(child$sex))
# Create Model
##mortality_cox <- coxph(child_surv ~ sex + m.age, data = child)

cox_model <- coxph(Surv(enter, exit, event) ~ sex + m.age, data = child)
summary(child_mortality_cox)
exp(-0.082215) - 1 
exp(0.007617) - 1




stargazer(cox_model, type = "latex")

# This was used in lecture slides but I am not sure how to interpret it...
LRT <- drop1(cox_model, test = "Chisq")
stargazer(LRT, type = "latex")
