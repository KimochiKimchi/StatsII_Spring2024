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

# lapply(c()),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#####################
# Problem 1
#####################

#Setting the seed for reproducibility
set.seed(100)
# create empirical distribution of observed data
empirical <- rcauchy(1000, location = 0, scale = 1)

#Writing a function to conduct the K-S Test in R
KS_function <- function (data){
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
# generate test statistic
  D <- max(abs(empiricalCDF - pnorm(data)))
# Calculating the p-value
  i_values <- 1:1000
  sum1 <- sum(exp(-((2 * i_values - 1)^2 * pi^2) / ((8 * D)^2)))
  p_value <- sqrt(2 * pi) / D * sum1
# Return result
  result <- list(
  Test_statistic = D,
  P_value = p_value
)
  return(result)
}
# Performing the K-S test with our data
KS_function(empirical)

#On the basis of this, we get the test statistic = 0.13997 and p-value is 0.00683

# Checking the result with the in-built function in R
ks.test(empirical,"pnorm")
# With this, we get the test statistic = 0.14097 and p-value is 2.2e-16


#####################
# Problem 2
#####################

# Set seed for reproducibility and using the provided code to create the data
set.seed(191)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75 * data$x + rnorm(200, 0, 1.5)

# Log-likelihood function for OLS
log_likelihood <- function(beta, x, y) {
  y_hat <- beta[1] + beta[2] * x
  residuals <- y - y_hat
  log_likelihood1 <- 0.5 * sum((residuals / 1.5)^2) + 0.5 * length(residuals) * log(2 * pi * 1.5^2)
  return(log_likelihood1)
}

# Initial guess for coefficients for iteration
initial_guess <- c(0, 0)

# Optimize using BFGS
?optim
result_bfgs <- optim(par = initial_guess,fn = log_likelihood, 
                     x = data$x, y = data$y, method = "BFGS")

# Extract coefficients from BFGS result
coefficients_bfgs <- result_bfgs$par
print(coefficients_bfgs)

#On the basis of this, the intercept is 0.15734 and beta1 is 2.76752

# OLS Regression using lm
ols_lm <- lm(y ~ x, data = data)

# Extract coefficients from lm result
print(coef(ols_lm))
#On the basis of this, the intercept is 0.15208 and beta1 is 2.76760

