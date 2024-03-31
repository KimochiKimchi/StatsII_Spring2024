# Replication
# Aryan Goyal
rm(list=ls())
library(kableExtra)
library(MASS)
library(DescTools)
library(survey)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(ggthemes)
library(plotrix)
library(plyr)
#library(sjPlot)
library(coefplot)
library(questionr)
library(psych)
library(snakecase)
library(questionr)
#library(sjmisc)
library(coefplot)
library(stargazer)
library(tidyr)
library(forcats)
library(stringr)
library(readr)
library(tidyverse)
library(glue)
library(papaja)
library(nnet)

#Setting the working directory
getwd()
setwd("C:/Users/Lenovo/Desktop/2023 ASDS/Applied Stats 2/Replication/Pls Replicate")
#First, I input the data
load("CleanDataExperiment2")

# Replicating the model from the article
Homophily.model.ordinal <- polr(ChoiceNFactor~Match.GenderD+Match.EducationD+Match.subClassD+
                                  Match.subFamClassD+Match.HomeStatusD+
                                  Match.regionD+  
                                  Match.religionD+Match.ethnicityD+
                                  Match.incomeD+Match.AgeGroupD
                                , weights=data.experiment2.Second$W8, 
                                data = data.experiment2.Second)
summary(Homophily.model.ordinal)

#Using stargazer for LaTex code
stargazer(Homophily.model.ordinal)

#Converting log odds to odd ratios
OriginalCoef <- coef(Homophily.model.ordinal)

# Exponentiate the coefficients to get odds ratios
Original_odds_ratios <- exp(OriginalCoef)
Original_odds_ratios

# Recoding all -1s and 0s to be 0s, leaving 1s unchanged for Profile A
# Recode the variable to show that the respondent chose profile A or not
data.experiment2.Second$ChoiceNFactorA <- ifelse(data.experiment2.Second$ChoiceNFactor %in% c(-1, 0), 0,
                                                 ifelse(data.experiment2.Second$ChoiceNFactor == 1, 1, NA))

#I run a logistic regression model to see whether a profile 
# with a matching characteristic was chosen or not for Profile As
ProfileA <- glm(ChoiceNFactorA ~Match.GenderA +Match.EducationA+Match.subClassA+
                  Match.subFamClassA+Match.HomeStatusA+
                  Match_regionA+  
                  Match_religionA+Match_ethnicityA+
                  Match_incomeA+AgeClosenessA
                , weights=data.experiment2.Second$W8,
                family = binomial(link = "logit"),
                data = data.experiment2.Second)

summary(ProfileA)

#Getting code for LaTex
stargazer(ProfileA)

#Converting log odds to odds ratio
ProfileACoef <- coef(ProfileA)
ProfileAOdds <- exp(ProfileACoef)
ProfileAOdds

#I run a logistic regression model to see whether a profile with a matching characteristic was chosen or not for Profile Bs
# Recode the variable
data.experiment2.Second$ChoiceNFactorB <- ifelse(data.experiment2.Second$ChoiceNFactor == -1, 1,
                                                 ifelse(data.experiment2.Second$ChoiceNFactor %in% c(0, 1), 0, NA))


ProfileB <- glm(ChoiceNFactorB ~Match.GenderB +Match.EducationB+Match.subClassB+
                  Match.subFamClassB+Match.HomeStatusB+
                  Match_regionB+  
                  Match_religionB+Match_ethnicityB+
                  Match_incomeB+AgeClosenessB
                , weights=data.experiment2.Second$W8,
                family = binomial(link = "logit"),
                data = data.experiment2.Second)


summary(ProfileB)

stargazer(ProfileB)
#Converting log odds to odds ratio
ProfileBCoef <- coef(ProfileB)
ProfileBOdds <- exp(ProfileBCoef)
ProfileBOdds

#To assess model fit:
# I plot residuals vs fitted values
plot(fitted(ProfileB), residuals(ProfileB, type = "deviance"),
     xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Then, I make a QQ plot of residuals
qqnorm(residuals(ProfileB, type = "deviance"))
qqline(residuals(ProfileB, type = "deviance"))


#################
#################
# Next, I only use code to replicate the figures from the article

fadecol <- function(col,alpha){
  components <- col2rgb(col)
  return(rgb(components[1,]/255,components[2,]/255,components[3,]/255,alpha=alpha))
}

CovInteraction <- function(m1, x) {
  vcov_2 <- vcov(m1) %>%
    data.frame() %>% 
    tibble::rownames_to_column("term")
  vcov_x <-vcov_2 %>%
    dplyr::filter(str_detect(term, glue("{x}")))
  vcov_x <-bind_rows(vcov_x[,c("term",x)])
  colnames(vcov_x)[colnames(vcov_x)==x] <- "Covariance"
  return(vcov_x)
}

#Replicating the figures from the article
Homophily1 <- coef(summary(Homophily.model.ordinal)) %>%
  data.frame() %>% 
  tibble::rownames_to_column("term")
Homophily1 <- Homophily1[-c(11,12),]
glimpse(Homophily1)

Homophily1<- Homophily1 %>%
  mutate(term = dplyr::recode(term, "Match.AgeGroupD" = "Age", "Match.EducationD" = "Education",
                              "Match.ethnicityD" = "Ethnicity", "Match.incomeD" = "Income", "Match.GenderD" = "Gender",
                              "Match.HomeStatusD" = "HomeStatus", "Match.regionD" = "Region",
                              "Match.religionD" = "Religion", "Match.subClassD" = "Subjective class",
                              "Match.subFamClassD" = "Subjective family Class"))
Homophily1 <- Homophily1 %>%
  mutate(term = fct_rev(term))

P.Homophily.1<-ggplot(Homophily1, aes(x=term, y=exp(Value))) + 
  geom_errorbar(aes(ymin=exp(Value-2*Std..Error), ymax=exp(Value+2*Std..Error)), width=.1) +
  geom_point(shape=19, size=2, alpha=0.7) +
  geom_hline(yintercept=1, linetype="dashed", 
             size=1, color="blue", alpha=0.5)+
  theme_clean()+ 
  ggtitle("Political commonality for each social category. Multivariate Model (odds ratio)") +
  theme(plot.title = element_text(hjust = 0.5, face = "plain")) +
  xlab("") + ylab("")+
  coord_flip(ylim = c(-1, 3))+
  theme(legend.position = "none")

One_At_A_Time <- function(varind){
  model<- polr(paste("ChoiceNFactor~",varind),
               weights=data.experiment2.Second$w8, data=data.experiment2.Second, Hess = T )
  M<-data.frame(coef(summary(model))[1,1])
  M[1,2]<-data.frame(coef(summary(model))[1,2])
  rownames(M)[1]<-varind
  colnames(M)[1]<-"Value"
  colnames(M)[2]<-"Std. Error"
  return(M) 
}


OneAtTime<-data.frame(matrix(NA, 10,2))
rownames(OneAtTime)<-c("Match.GenderD", "Match.EducationD", "Match.subClassD", "Match.subFamClassD", "Match.HomeStatusD",
                       "Match.regionD", "Match.religionD", "Match.ethnicityD", "Match.incomeD", "Match.AgeGroupD")
for (i in rownames(OneAtTime)){
  OneAtTime[i,1:2]<-  One_At_A_Time(i)
}
colnames(OneAtTime)[c(1,2)]<-c("Value","Std.Error")
OneAtTime<- OneAtTime %>%
  tibble::rownames_to_column("Name")
OneAtTime<- OneAtTime %>%
  mutate(Name = dplyr::recode(Name, "Match.AgeGroupD" = "Age", "Match.EducationD" = "Education",
                              "Match.ethnicityD" = "Ethnicity", "Match.incomeD" = "Income", "Match.GenderD" = "Gender",
                              "Match.HomeStatusD" = "HomeStatus", "Match.regionD" = "Region",
                              "Match.religionD" = "Religion", "Match.subClassD" = "Subjective class",
                              "Match.subFamClassD" = "Subjective family Class"))

P.OneAtTime.1<-ggplot(OneAtTime, aes(x=Name, y=exp(Value))) + 
  geom_errorbar(aes(ymin=exp(Value-2*Std.Error), ymax=exp(Value+2*Std.Error)), width=.1) +
  geom_point(shape=19, size=2, alpha=0.7) +
  geom_hline(yintercept=1, linetype="dashed", 
             size=1, color="blue", alpha=0.5)+
  theme_clean()+ 
  ggtitle("Political commonality for each social category. Bivariate models (odds ratio)") + theme(plot.title = element_text(hjust = 0.5, face = "plain")) +
  xlab("") + ylab("")+
  coord_flip(ylim = c(-1, 3))+
  theme(legend.position = "none",
        strip.text.y = element_blank(), panel.background = element_rect(fill="#00000000"))+
  facet_grid(Name~., scale="free", space= "free")

#Code to plot figure 2
grid.arrange(P.Homophily.1, P.OneAtTime.1, nrow=2)

#Next, I run the code for Figure 3
data.experiment2.Second<-data.experiment2.Second %>%
  dplyr::  mutate(pastvote_party = recode_factor(pastvote_2017, Conservative  = "Conservative",
                                                 Labour = "Labour",
                                                 .default=NA_character_))
Homophily.Party.ordinal<- polr(ChoiceNFactor~Match.GenderD*pastvote_party+Match.EducationD*pastvote_party+
                                 Match.subClassD*pastvote_party+Match.subFamClassD*pastvote_party+Match.HomeStatusD*pastvote_party+
                                 Match.regionD*pastvote_party+Match.religionD*pastvote_party+Match.ethnicityD*pastvote_party+
                                 Match.incomeD*pastvote_party+Match.AgeGroupD*pastvote_party,
                               weights=data.experiment2.Second$W8, data = data.experiment2.Second)

data.experiment2.Second<-data.experiment2.Second %>%
  mutate(pastvote_Brexit = recode_factor(pastvote_EURef, "I voted to Remain"  = "Remain",
                                         "I voted to Leave" = "Leave",
                                         .default=NA_character_))
Homophily.Brexit.ordinal<- polr(ChoiceNFactor~Match.GenderD*pastvote_Brexit+Match.EducationD*pastvote_Brexit+
                                  Match.subClassD*pastvote_Brexit+Match.subFamClassD*pastvote_Brexit+Match.HomeStatusD*pastvote_Brexit+
                                  Match.regionD*pastvote_Brexit+Match.religionD*pastvote_Brexit+Match.ethnicityD*pastvote_Brexit+
                                  Match.incomeD*pastvote_Brexit+Match.AgeGroupD*pastvote_Brexit
                                , weights=data.experiment2.Second$W8, data = data.experiment2.Second)

summary(Homophily.Brexit.ordinal)
#Interaction with party


#Function to extract covariances


Cov.Education<-CovInteraction(Homophily.Party.ordinal, "Match.EducationD")
Cov.Ethnicity<-CovInteraction(Homophily.Party.ordinal, "Match.ethnicityD")
Cov.Home<-CovInteraction(Homophily.Party.ordinal, "Match.HomeStatusD")
Cov.Gender<-CovInteraction(Homophily.Party.ordinal, "Match.GenderD")
Cov.Region<-CovInteraction(Homophily.Party.ordinal, "Match.regionD")
Cov.Religion<-CovInteraction(Homophily.Party.ordinal, "Match.religionD")
Cov.Class<-CovInteraction(Homophily.Party.ordinal, "Match.subClassD")
Cov.Age<-CovInteraction(Homophily.Party.ordinal, "Match.AgeGroupD")
Cov.Income<- CovInteraction(Homophily.Party.ordinal, "Match.incomeD")
Cov.FamilyClass <- CovInteraction(Homophily.Party.ordinal, "Match.subFamClassD")

Homophily3 <- coef(summary(Homophily.Party.ordinal)) %>%
  data.frame() %>% 
  tibble::rownames_to_column("term")
#vcov_2 <- vcov(Homophily.Party.ordinal) %>%
#  data.frame() %>% 
#  tibble::rownames_to_column("term")


Covariances<-rbind(Cov.Education, Cov.Ethnicity,
                   Cov.Home, Cov.Gender, Cov.Region, Cov.Religion, Cov.Class,Cov.FamilyClass, Cov.Age, Cov.Income)


InteractionIdentityP<- left_join(Covariances, Homophily3, by = "term")  

InteractionIdentityP<- InteractionIdentityP %>%
  mutate(term, term = str_replace(term,"Match.GenderD:pastvote_partyLabour", "pastvote_partyLabour:Match.GenderD"))
InteractionIdentityP<- InteractionIdentityP %>%
  mutate(term, Coefficient = str_glue("Conservative:{term}"))
InteractionIdentityP<- InteractionIdentityP %>%
  mutate(term, Coefficient = str_remove(Coefficient,"Conservative:pastvote_party"))           
InteractionIdentityP<- InteractionIdentityP %>%
  mutate(term, Coefficient = str_remove(Coefficient,"Match."))
InteractionIdentityP<- InteractionIdentityP %>%
  mutate(term, Coefficient = str_remove(Coefficient,"D")) 

#Obtain coefficents

InteractionIdentityCoefP<- InteractionIdentityP %>%
  dplyr::select(Coefficient, Value)
InteractionIdentityCoefP<- InteractionIdentityCoefP %>%
  tidyr::separate(col=Coefficient, into=c("type","Coefficient"))
InteractionIdentityCoefP<- InteractionIdentityCoefP %>%
  tidyr::spread(type, Value, convert=T)

InteractionIdentityCoefP<- InteractionIdentityCoefP %>%
  mutate(Labour= InteractionIdentityCoefP$Conservative + InteractionIdentityCoefP$Labour)

#Obtain standard errors

InteractionIdentityCovP<- InteractionIdentityP %>%
  dplyr::select(Coefficient, Covariance)
InteractionIdentityCovP<- InteractionIdentityCovP %>%
  tidyr::separate(col=Coefficient, into=c("type", "Coefficient"))
InteractionIdentityCovP<- InteractionIdentityCovP %>%
  tidyr::spread(type, Covariance, convert=T)
InteractionIdentityCovP<- InteractionIdentityCovP %>%
  dplyr::rename("Conservative.Cov00"= "Conservative" ,
                "Labour.Cov01" ="Labour")

InteractionIdentitySEP<- InteractionIdentityP %>%
  dplyr::select(Coefficient, Std..Error)
InteractionIdentitySEP<- InteractionIdentitySEP %>%
  tidyr::separate(col=Coefficient, into=c("type", "Coefficient"))
InteractionIdentitySEP<- InteractionIdentitySEP %>%
  tidyr::spread(type, Std..Error, convert=T)
InteractionIdentitySEP<- InteractionIdentitySEP %>%
  dplyr::rename("Conservative.SE"= "Conservative" ,
                "Labour.SE" ="Labour")

InteractionIdentitySE.tidyP<- left_join(InteractionIdentitySEP, InteractionIdentityCovP, by = "Coefficient")
InteractionIdentitySE.tidyP[,3]<-InteractionIdentitySE.tidyP[,3]^2
InteractionIdentitySE.tidyP[,5]<-InteractionIdentitySE.tidyP[,5]*2
InteractionIdentitySE.tidyP<- InteractionIdentitySE.tidyP %>%
  mutate(Labour.SE=
           sqrt( InteractionIdentitySE.tidyP$Conservative.Cov00 +InteractionIdentitySE.tidyP$Labour.SE + InteractionIdentitySE.tidyP$Labour.Cov01))



#Tyding up

InteractionIdentitySE.tidyP <- InteractionIdentitySE.tidyP %>%
  dplyr::select( "Name"=Coefficient, ends_with("SE"))
InteractionIdentitySE.tidyP <- InteractionIdentitySE.tidyP %>%
  gather(key="level", value = "SE", ends_with("SE"))
InteractionIdentitySE.tidyP <- InteractionIdentitySE.tidyP %>%
  mutate(level = str_remove(level, ".SE"))
InteractionIdentitySE.tidyP <- InteractionIdentitySE.tidyP %>%
  unite("Name", c(Name, level))
InteractionIdentitySE.tidyP <- InteractionIdentitySE.tidyP %>%
  filter(!is.na(SE))

InteractionIdentityCoef.tidyP <- InteractionIdentityCoefP %>%
  dplyr::select( "Name"=Coefficient, Conservative, Labour)
InteractionIdentityCoef.tidyP <- InteractionIdentityCoef.tidyP %>%
  gather(key="level", value = "Coefficient", c(Conservative, Labour))
InteractionIdentityCoef.tidyP <- InteractionIdentityCoef.tidyP %>%
  unite("Name", c(Name, level))
InteractionIdentityCoef.tidyP <- InteractionIdentityCoef.tidyP %>%
  filter(!is.na(Coefficient))

InteractionIdentity.tidyP <- left_join(InteractionIdentitySE.tidyP, InteractionIdentityCoef.tidyP, by = "Name")
InteractionIdentity.tidyP <-InteractionIdentity.tidyP[order(InteractionIdentity.tidyP$Name),]


InteractionIdentity.tidyP2<- InteractionIdentity.tidyP %>%
  tidyr::separate(col=Name, into=c("Name","Party"), sep="_")

InteractionIdentity.tidyP2<- InteractionIdentity.tidyP2 %>%
  mutate(Name = dplyr::recode(Name, "AgeGroup" = "Age", "ethnicity" = "Ethnicity",  "income" = "Income", "HomeStatus"="Home Status"
                              , "region" = "Region", "religion" = "Religion", "subClass" = "Subjective class", "subFamClass" = "Subjective Family Class"))


P.Homophily.3<-ggplot(InteractionIdentity.tidyP2, aes(x=Name, y=exp(Coefficient), col=Party)) + 
  geom_errorbar(aes(ymin=exp(Coefficient-2*SE), ymax=exp(Coefficient+2*SE)), width=.2, position = position_dodge(width = 0.5)) +
  geom_point(shape=19, size=2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept=1, linetype="dashed", 
             size=1, color="grey", alpha=0.5)+
  theme_clean()+ 
  ggtitle("Political commonality for each social category by party vote in 2017 (odds ratio)") +
  labs(color = "General Election") +
  theme(plot.title = element_text(hjust = 0.5, face = "plain")) +
  xlab("") + ylab("")+
  coord_flip(ylim = c(0, 4))+
  facet_grid(Name~., scales="free", space = "free")+
  theme(strip.text.y = element_blank())+
  scale_color_manual(values= c("#0087DC", "#E4003B"), guide = guide_legend(reverse=TRUE))

#Interaction with Brexit


#Function to extract covariances


Cov.Education<-CovInteraction(Homophily.Brexit.ordinal, "Match.EducationD")
Cov.Ethnicity<-CovInteraction(Homophily.Brexit.ordinal, "Match.ethnicityD")
Cov.Home<-CovInteraction(Homophily.Brexit.ordinal, "Match.HomeStatusD")
Cov.Gender<-CovInteraction(Homophily.Brexit.ordinal, "Match.GenderD")
Cov.Region<-CovInteraction(Homophily.Brexit.ordinal, "Match.regionD")
Cov.Religion<-CovInteraction(Homophily.Brexit.ordinal, "Match.religionD")
Cov.Class<-CovInteraction(Homophily.Brexit.ordinal, "Match.subClassD")
Cov.Age<-CovInteraction(Homophily.Brexit.ordinal, "Match.AgeGroupD")
Cov.Income<- CovInteraction(Homophily.Brexit.ordinal, "Match.incomeD")
Cov.FamilyClass <- CovInteraction(Homophily.Brexit.ordinal, "Match.subFamClassD")

Homophily4 <- coef(summary(Homophily.Brexit.ordinal)) %>%
  data.frame() %>% 
  tibble::rownames_to_column("term")
#vcov_2 <- vcov(Homophily.Party.ordinal) %>%
#  data.frame() %>% 
#  tibble::rownames_to_column("term")


Covariances<-rbind(Cov.Education, Cov.Ethnicity,
                   Cov.Home, Cov.Gender, Cov.Region, Cov.Religion, Cov.Class,Cov.FamilyClass, Cov.Age, Cov.Income)


InteractionIdentityB<- left_join(Covariances, Homophily4, by = "term")  

InteractionIdentityB<- InteractionIdentityB %>%
  mutate(term, term = str_replace(term,"Match.GenderD:pastvote_BrexitLeave", "pastvote_BrexitLeave:Match.GenderD"))
InteractionIdentityB<- InteractionIdentityB %>%
  mutate(term, Coefficient = str_glue("Remain:{term}"))
InteractionIdentityB<- InteractionIdentityB %>%
  mutate(term, Coefficient = str_remove(Coefficient,"Remain:pastvote_Brexit"))           
InteractionIdentityB<- InteractionIdentityB %>%
  mutate(term, Coefficient = str_remove(Coefficient,"Match."))
InteractionIdentityB<- InteractionIdentityB %>%
  mutate(term, Coefficient = str_remove(Coefficient,"D")) 

#Obtain coefficents

InteractionIdentityCoefB<- InteractionIdentityB %>%
  dplyr::select(Coefficient, Value)
InteractionIdentityCoefB<- InteractionIdentityCoefB %>%
  tidyr::separate(col=Coefficient, into=c("type","Coefficient"))
InteractionIdentityCoefB<- InteractionIdentityCoefB %>%
  tidyr::spread(type, Value, convert=T)

InteractionIdentityCoefB<- InteractionIdentityCoefB %>%
  mutate(Leave= InteractionIdentityCoefB$Remain + InteractionIdentityCoefB$Leave)

#Obtain standard errors

InteractionIdentityCovB<- InteractionIdentityB %>%
  dplyr::select(Coefficient, Covariance)
InteractionIdentityCovB<- InteractionIdentityCovB %>%
  tidyr::separate(col=Coefficient, into=c("type", "Coefficient"))
InteractionIdentityCovB<- InteractionIdentityCovB %>%
  tidyr::spread(type, Covariance, convert=T)
InteractionIdentityCovB<- InteractionIdentityCovB %>%
  dplyr::rename("Remain.Cov00"= "Remain" ,
                "Leave.Cov01" ="Leave")

InteractionIdentitySEB<- InteractionIdentityB %>%
  dplyr::select(Coefficient, Std..Error)
InteractionIdentitySEB<- InteractionIdentitySEB %>%
  tidyr::separate(col=Coefficient, into=c("type", "Coefficient"))
InteractionIdentitySEB<- InteractionIdentitySEB %>%
  tidyr::spread(type, Std..Error, convert=T)
InteractionIdentitySEB<- InteractionIdentitySEB %>%
  dplyr::rename("Remain.SE"= "Remain" ,
                "Leave.SE" ="Leave")

InteractionIdentitySE.tidyB<- left_join(InteractionIdentitySEB, InteractionIdentityCovB, by = "Coefficient")
InteractionIdentitySE.tidyB[,2]<-InteractionIdentitySE.tidyB[,2]^2
InteractionIdentitySE.tidyB[,4]<-InteractionIdentitySE.tidyB[,4]*2
InteractionIdentitySE.tidyB<- InteractionIdentitySE.tidyB %>%
  mutate(Leave.SE=
           sqrt( InteractionIdentitySE.tidyB$Remain.Cov00 +InteractionIdentitySE.tidyB$Leave.SE + InteractionIdentitySE.tidyB$Leave.Cov01))

#Tyding up

InteractionIdentitySE.tidyB <- InteractionIdentitySE.tidyB %>%
  dplyr::select( "Name"=Coefficient, ends_with("SE"))
InteractionIdentitySE.tidyB <- InteractionIdentitySE.tidyB %>%
  gather(key="level", value = "SE", ends_with("SE"))
InteractionIdentitySE.tidyB <- InteractionIdentitySE.tidyB %>%
  mutate(level = str_remove(level, ".SE"))
InteractionIdentitySE.tidyB <- InteractionIdentitySE.tidyB %>%
  unite("Name", c(Name, level))
InteractionIdentitySE.tidyB <- InteractionIdentitySE.tidyB %>%
  filter(!is.na(SE))

InteractionIdentityCoef.tidyB <- InteractionIdentityCoefB %>%
  dplyr::select( "Name"=Coefficient, Remain, Leave)
InteractionIdentityCoef.tidyB <- InteractionIdentityCoef.tidyB %>%
  gather(key="level", value = "Coefficient", c(Remain, Leave))
InteractionIdentityCoef.tidyB <- InteractionIdentityCoef.tidyB %>%
  unite("Name", c(Name, level))
InteractionIdentityCoef.tidyB <- InteractionIdentityCoef.tidyB %>%
  filter(!is.na(Coefficient))

InteractionIdentity.tidyB <- left_join(InteractionIdentitySE.tidyB, InteractionIdentityCoef.tidyB, by = "Name")
InteractionIdentity.tidyB <-InteractionIdentity.tidyB[order(InteractionIdentity.tidyB$Name),]


InteractionIdentity.tidyB2<- InteractionIdentity.tidyB %>%
  tidyr::separate(col=Name, into=c("Name","Referendum.Vote"), sep="_")

InteractionIdentity.tidyB2<- InteractionIdentity.tidyB2 %>%
  mutate(Name = dplyr::recode(Name, "AgeGroup" = "Age", "ethnicity" = "Ethnicity", "HomeStatus"="Home Status",  "income" = "Income", "HomeStatus"="Home Status", "region" = "Region", "religion" = "Religion", "subClass" = "Subjective class", "subFamClass" = "Subjective family Class"))


P.Homophily.4<-ggplot(InteractionIdentity.tidyB2, aes(x=Name, y=exp(Coefficient), col=Referendum.Vote)) + 
  geom_errorbar(aes(ymin=exp(Coefficient-2*SE), ymax=exp(Coefficient+2*SE)), width=.2, position = position_dodge(width = 0.5)) +
  geom_point(shape=19, size=2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept=1, linetype="dashed", 
             size=1, color="grey", alpha=0.5)+
  theme_clean()+ 
  ggtitle("Political commonality for each social category by 2016 EU referendum vote (odds ratio)") +
  labs(color = "EU referendum") +
  theme(plot.title = element_text(hjust = 0.5, face = "plain")) +
  xlab("") + ylab("")+
  coord_flip(ylim = c(0, 4))+
  facet_grid(Name~., scales="free", space = "free")+
  theme(strip.text.y = element_blank())+
  scale_color_manual(values= c("#12B6CF", "#ffd700"), guide = guide_legend(reverse=TRUE))

#Code to plot Figure 3
grid.arrange(P.Homophily.3, P.Homophily.4, nrow=2) 

