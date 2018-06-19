##################################
# CHAPTER ONE preliminary setup
##################################
#Make sure you run this before embarking on analyses for Sections 1-3


library(foreign)
library(dplyr)
library(ggplot2)
library(reshape2)
library(DescTools)
library(tidyr)
library(lsr)

#Read in initial data
setwd(".") #make sure your data is in the same directory!
d.data <- read.csv("20171120_dissertation_data.csv")
attach(d.data)

eng <- filter(d.data, Lang == 1)
chn <- filter(d.data, Lang == 2)

gain <- filter(d.data, Q1Form==1)
loss <- filter(d.data, Q1Form==2)

#Basic Data Ex
nrow(d.data) #477 respondents
table(Q52_Age)

mean(Q52_Age, na.rm=T) #20.16 years
sd(Q52_Age, na.rm=T) #1.51 years
median(Q52_Age, na.rm=T) #20

table(Q53_Gender) #107 males, 364 females

table(Q54_Education)

#Language ability - MSM proficiency
sum(!is.na(chn$Q58_Chinese_ability)) #224
mean(chn$Q58_Chinese_ability, na.rm=T)
sd(chn$Q58_Chinese_ability, na.rm=T) #1.38

#English Proficiency
sum(!is.na(eng$Q58_Chinese_ability)) #98 responses
mean(eng$Q58_Chinese_ability, na.rm=T) #7.34
sd(eng$Q58_Chinese_ability, na.rm=T) #2.04#Questions analyzed: 
