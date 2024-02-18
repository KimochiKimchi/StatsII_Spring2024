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

lapply(c("tidyverse","stargazer","xtable"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

#inspecting the data
head(climateSupport)
summary(climateSupport)

#Q1: Fit an additive model. Provide the summary output, 
# the global null hypothesis, and p-value. 
# Please describe the results and provide a conclusion.

# Releveling the variables and changing the reference category (for Q2a) and Q2b))
climateSupport$sanctions <- relevel(factor(climateSupport$sanctions), ref="5%")
climateSupport$countries <- relevel(factor(climateSupport$countries), ref="20 of 192")

# First, I run an additive GLM model for logistic regression 
additive <- glm(choice ~ countries + sanctions, data= climateSupport, 
                family = binomial(link="logit"))
summary(additive)

# I run a null model and use the anova() function to conduct a 
# likelihood-ratio test (LRT):
nullModel <- glm(choice~1, data=climateSupport, 
                 family = binomial(link = "logit"))
anova1 <- anova(nullModel, additive, test="LRT")
print(anova1)

# The p-value is below our critical threshold of 0.05 which means we can 
# reject our null hypothesis and find support for the alternate hypothesis 
# that our additive model with variables for countries and sanctions explains 
# the variation in our outcome variable better than the null model. 

#Q2a) For the policy in which nearly all countries participate [160 of 192], 
# how does increasing sanctions from 5% to 15% change the odds that an 
#individual will support the policy? (Interpretation of a coefficient)


# Answer 2a): Increasing sanctions from 5% to 15%, on average, 
# leads to a decrease of 0.325 in the log odds that an individual will 
# support the policy. This coefficient is highly significant at the 0.01 level. 

# I use stargazer to get tables for LaTex
stargazer(additive)
# Using xtable for the anova table (took help from ChatGPT)
xtable(anova1)


#Q2b) What is the estimated probability that an individual will support a 
# policy if there are 80 of 192 countries participating with no sanctions?

#Answer2b) Using predict function to get the estimated probability with the conditions
predict(additive, newdata = data.frame(countries="80 of 192", sanctions="None"), type="response")

#The estimated probability that an individual will support a policy if there
# are 80 of 192 countries participating with no sanctions is 0.52.

# 2c) fitting an interactive model

interactive <- glm(choice ~ countries * sanctions, data = climateSupport,
                   family = binomial(link="logit"))

summary(interactive)

#I use stargazer to get code for making a table in LaTex
stargazer(interactive)

# I use the likelihood-ratio test (LRT) to compare this to the additive model
anova2 <- anova(additive,interactive, test="LRT")
print(anova2)

#After running the likelihood-ratio test (LRT), we get a p-value of 0.3912 
# which is not below the critical threshold of 0.05, hence we cannot reject 
# our null hypothesis that the  the model without the interaction term 
# (Additive model) is just as good as the model with the interaction term 
# (Interactive model). As the LRT is not significant, we cannot conclude 
# that the model with the interaction term provides a better fit to the 
# data than the additive model. Hence, including an interaction term does 
# not provide value for this model.


#Using xtable to make the Anova table in LaTex
xtable(anova2)






