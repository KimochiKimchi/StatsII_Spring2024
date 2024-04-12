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

lapply(c("eha", "survival", "survminer","stargazer"),  pkgTest)
#Setting the working directory
setwd("C:/Users/Lenovo/Documents/GitHub/StatsII_Spring2024/problemSets/PS04/my_answers")

#####################
# Problem 1
#####################

# load data
data("child")
head(child)
summary(child)

#Running a Cox proportional hazard model with mother's age and infant sex as predictors
child_surv <- with(child, Surv(enter, exit, event))

Model <- coxph(child_surv ~ m.age + sex, data = child)
summary(Model)

#Getting code for LaTex
stargazer(Model)
