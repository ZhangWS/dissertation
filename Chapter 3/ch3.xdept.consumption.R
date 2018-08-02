####################################
# CHAPTER THREE STATISTICAL ANALYSIS FL ONLY ANALYSIS
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
d.v1 <- subset(d.data, Version == 1)

#make a new department number in V1

d.v1$Department <- 0
d.v1$Department[d.v1$SurveyNo < 2200] <- 1
detach(d.data)
attach(d.v1)

chn3 <- filter(d.v1, Lang == 1)
eng3 <- filter(d.v1, Lang ==2)

#Q3
fligner.test(Q3_Media_newspaper~Department, data = chn3)
wilcox.test(Q3_Media_newspaper~Department, data = chn3)
t.test(chn2$Q3_Media_newspaper, chn$Q3_Media_newspaper)

q3.aov<-aov(Q3_Media_newspaper~Department, data=chn3)
etaSquared(q3.aov, type = 2)

fligner.test(Q3_Media_newspaper~Department, data = eng3)
wilcox.test(Q3_Media_newspaper~Department, data = eng3)
t.test(eng2$Q3_Media_newspaper, eng$Q3_Media_newspaper)

q3.aov<-aov(Q3_Media_newspaper~Department, data=eng3)
etaSquared(q3.aov, type = 2)

#Q4
fligner.test(Q4_Media_tv~Department, data = chn3)
wilcox.test(Q4_Media_tv~Department, data = chn3)
t.test(chn2$Q4_Media_tv, chn$Q4_Media_tv)

q4.aov <- aov(Q4_Media_tv~Lang, data=d.chn3)
etaSquared(q4.aov, type = 2)

fligner.test(Q4_Media_tv~Department, data = eng3)
wilcox.test(Q4_Media_tv~Department, data = eng3)
t.test(eng2$Q4_Media_tv, eng$Q4_Media_tv)
q4.aov <- aov(Q4_Media_tv~Lang, data=eng3)
etaSquared(q4.aov, type = 2)


#Q5

fligner.test(Q5_Media_radio~Department, data = chn3)
wilcox.test(Q5_Media_radio~Department, data = chn3)
t.test(chn$Q5_Media_radio, chn2$Q5_Media_radio)

q5.aov<-aov(Q5_Media_radio~Lang, data=chn3)
etaSquared(q5.aov, type = 2)

fligner.test(Q5_Media_radio~Department, data = eng3)
wilcox.test(Q5_Media_radio~Department, data = eng3)
t.test(eng$Q5_Media_radio, eng2$Q5_Media_radio)

q5.aov<-aov(Q5_Media_radio~Lang, data=eng3)
etaSquared(q5.aov, type = 2)

#Q6

fligner.test(Q6_Media_internet~Department, data = chn3)
wilcox.test(Q6_Media_internet~Department, data = chn3)
t.test(chn$Q6_Media_internet, chn2$Q6_Media_internet)

q6.aov<-aov(Q6_Media_internet~Lang, data=chn3)
etaSquared(q6.aov, type = 2)

fligner.test(Q6_Media_internet~Department, data = eng3)
wilcox.test(Q6_Media_internet~Department, data = eng3)
t.test(eng$Q6_Media_internet, eng2$Q6_Media_internet)

q6.aov<-aov(Q6_Media_internet~Lang, data=eng3)
etaSquared(q6.aov, type = 2)

#Q7

fligner.test(Q7_Pol_discussion~Department, data = chn3)
wilcox.test(Q7_Pol_discussion~Department, data = chn3)
t.test(chn$Q7_Pol_discussion, chn2$Q7_Pol_discussion)

q7.aov<-aov(Q7_Pol_discussion~Lang, data=chn3)
etaSquared(q7.aov, type = 2)

fligner.test(Q7_Pol_discussion~Department, data = eng3)
wilcox.test(Q7_Pol_discussion~Department, data = eng3)
t.test(eng$Q7_Pol_discussion, eng2$Q7_Pol_discussion)

q7.aov<-aov(Q7_Pol_discussion~Lang, data=eng3)
etaSquared(q7.aov, type = 2)

cohensD(Q3_Media_newspaper~Department, data=chn3)
cohensD(Q4_Media_tv~Department, data=chn3)
cohensD(Q5_Media_radio~Department, data=chn3)
cohensD(Q6_Media_internet~Department, data=chn3)
cohensD(Q7_Pol_discussion~Department, data=chn3)

cohensD(Q3_Media_newspaper~Department, data=eng3)
cohensD(Q4_Media_tv~Department, data=eng3)
cohensD(Q5_Media_radio~Department, data=eng3)
cohensD(Q6_Media_internet~Department, data=eng3)
cohensD(Q7_Pol_discussion~Department, data=eng3)