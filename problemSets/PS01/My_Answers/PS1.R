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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

set.seed(123)

# Create both datasets
cauchyData <- rcauchy(1000, location = 0, scale = 1)
ECDF <- ecdf(cauchyData)
empiricalCDF <- ECDF(cauchyData)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(cauchyData)))


ks_tst <- function(data) {
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  # generate test statistic
  D <- max(abs(empiricalCDF - pnorm(data)))
  #####
  #
  n <- length(data)
  sum1 <- 0 
  for (i in 1:n) {   # loop through each value
  # get the summation
  sum1 <- sum1 + exp(-(2*i-1)^2 * pi^2 / (8*D^2))
  }
  # solve the first part of the equation
  pval <- (sqrt(2*pi)/D) * sum1 
  # Return the t-stat and p value
  answer <- list(D = D, P_Value = pval)
  return(answer)
}

# Test my function 
cauchyData <- rcauchy(1000, location = 0, scale = 1)
ks_tst(cauchyData)

# Test run the built in funciton to compare
ks.test(cauchyData, "pnorm")



#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)
View(data)

### Test with lm()###
mod1 <- lm(y~x, data = data)
summary(mod1)


### Make link function###
lin_link <- function(theta, y, x){
  n <- nrow(x)
  k <- ncol(x)
  beta <- theta[1:k] # our predictors
  sigma2 <- theta[k+1]^2 # calculate the variance of error 
  e <- y-x%*%beta # compute the residuals
  # log likelihood equation
  logl <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ((t(e) %*% 
      e)/ (2*sigma2))
  return(-logl)
}


linear_MLE <- optim(
  fn = lin_link,
  par = c(1, 1, 1), # initial parameters
  hessian = TRUE,
  y = data$y,
  x = cbind(1, data$x),
  method = "BFGS"
)

linear_MLE$par

?optim
