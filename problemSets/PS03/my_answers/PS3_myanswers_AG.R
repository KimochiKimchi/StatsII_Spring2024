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

lapply(c("nnet", "MASS","stargazer","texreg"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

head(gdp_data)
# Construct and interpret an unordered multinomial logit 
# with GDPWdiff as the output and ”no change” as the 
#reference category, including the estimated cutoff points and coefficients.

# First, I convert the GDP to a factor variable
gdp_data[gdp_data$GDPWdiff==0, "factor_GDP"] <- "no change"
gdp_data[gdp_data$GDPWdiff>0, "factor_GDP"] <- "positive"
gdp_data[gdp_data$GDPWdiff<0, "factor_GDP"] <- "negative"
gdp_data$factor_GDP <- relevel(as.factor(gdp_data$factor_GDP), ref="no change")

# I run an unordered multinomial logit model
unorderedLogit <- multinom(factor_GDP ~ REG + OIL, data=gdp_data)
summary(unorderedLogit)

#Using texreg to make a table in LaTex
texreg(unorderedLogit)

#1b) Construct and interpret an ordered multinomial logit with 
# GDPWdiff as the outcome variable, 
# including the estimated cutoff points and coefficients.

#For this, I relevel the variable again to create an ordering
gdp_data$factor_GDP1 <- relevel(gdp_data$factor_GDP, ref="negative")

# I run an ordered multinom logit model
OrderedModel <- polr(factor_GDP1 ~ REG + OIL, data=gdp_data)
summary(OrderedModel)

#Using texreg to make a table in LaTex
texreg(OrderedModel)

#Calculating a p-value
ctable <- coef(summary(OrderedModel))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))


#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")


#2a) Run a Poisson regression because the outcome is a count variable. 
# Is there evidence that PAN presidential candidates visit swing districts more? 
# Provide a test statistic and p-value.

# I run a Poisson regression model
poissonModel <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, family=poisson)
summary(poissonModel)

# Using texreg to make a table in LaTex
texreg(poissonModel)

# Interpreting outputs
cfs <- coef(poissonModel)
cfs

#2c) I use the predict function and put the corresponding values for each input variable
predict(poissonModel, newdata=data.frame(competitive.district=1, 
                                    marginality.06 = 0, 
                                    PAN.governor.06=1), type="response")
