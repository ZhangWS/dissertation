####################################
# CHAPTER THREE STATISTICAL ANALYSIS
####################################

####PRELIMINARIES
library(foreign)
library(dplyr)
library(ggplot2)
library(reshape2)
library(DescTools)
library(tidyr)
library(lsr)
library(MASS)

#Your data should be in the same directory as your R Code.
#Otherwise, customize as needed
setwd(".")
#read in master data set
d.data <- read.csv("20171120_dissertation_data.csv")
attach(d.data)

#subset into the data that we need to analyze
chn <- filter(d.data, Version == 1 & Lang == 2)

#pooled sample
eng <- filter(d.data, Lang == 1) #pooled sample
eng2<- filter(d.data, Lang == 1 & Version == 2) #V2 only

###BASIC CHARACTERISTICS
#do some basic descriptives on the set

#how many of each version?
nrow(chn) #91
nrow(eng) #246
nrow(eng2) #147

table(eng$Q52_Age) #table of ages, most between 18 and 21
sum(d.eng$Q52_Age <= 21 & d.v1$Q52_Age >=18, na.rm=T) 
#in fact, 157 between 18 and 21, 164 between 18 and 22

#Total Sample
sum(!is.na(d.v1$Q52_Age)) #185 people gave age info
mean(d.v1$Q52_Age, na.rm=T) #average age 20.08 yo
sd(d.v1$Q52_Age, na.rm=T) #sd 1.91 y
median(d.v1$Q52_Age, na.rm=T) #median & modal respondent is 20 yo

table(d.v1$Q53_Gender) #28 male, 159 female, 187 respondents overall

#Language ability - MSM proficiency
sum(!is.na(chn$Q58_Chinese_ability)) #90 responses
mean(chn$Q58_Chinese_ability, na.rm=T) #8.41
sd(chn$Q58_Chinese_ability, na.rm=T) #1.29

#English Proficiency
sum(!is.na(eng$Q58_Chinese_ability)) #98 responses
mean(eng$Q58_Chinese_ability, na.rm=T) #7.34
sd(eng$Q58_Chinese_ability, na.rm=T) #2.04

#Compare to pooled sample and also to L2-V2
eng1 <- filter (d.data, Lang == 1)
sum(!is.na(eng1$Q58_Chinese_ability)) #240 responses
mean(eng1$Q58_Chinese_ability, na.rm=T) #6.8
sd(eng1$Q58_Chinese_ability, na.rm=T) #2.19

#Compare to L2-V2

sum(!is.na(eng2$Q58_Chinese_ability)) #98 responses
mean(eng2$Q58_Chinese_ability, na.rm=T) #7.34
sd(eng2$Q58_Chinese_ability, na.rm=T) #1.04


#check proficiency of Foreign Language majors only
chn.chn <- filter(chn, Q55_English == 1)
nrow(chn.chn)
sum(!is.na(chn.chn$Q58_Chinese_ability)) #240 responses
mean(chn.chn$Q58_Chinese_ability, na.rm=T) #6.8
sd(chn.chn$Q58_Chinese_ability, na.rm=T) #2.19