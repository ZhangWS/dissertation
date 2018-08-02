##################################
# CHAPTER FOUR STATISTICAL ANALYSIS
##################################

####PRELIMINARIES
library(foreign)
library(dplyr)
library(ggplot2)
library(reshape2)
library(DescTools)
library(tidyr)
library(lsr)

#Read in initial data
d.data <- read.csv("20171120_dissertation_data.csv")
attach(d.data)

#We only want people who have not traveled abroad
#Additional filter for one respondent whose age was 1.
#I currently can't find the survey to confirm if the mistake was
#on the survey or a mistype, so I've left it out for now.
d.data1 <- filter(d.data, Q59_Travel_abroad >2)
detach(d.data)




#General descriptives
#I like distributions
nrow(d.data1)
table(Q52_Age)

mean(Q52_Age, na.rm=T)
sd(Q52_Age, na.rm=T)
median(Q52_Age, na.rm=T)

table(Q53_Gender)

d.data1$Lang[d.data1$Lang==2] <- 0

attach(d.data1)
#Then split into language groups for separate analysis
eng <- filter(d.data1, Lang == 1)
chn <- filter(d.data1, Lang == 0)

#proficiency rating of Chinese
mean(chn$Q58_Chinese_ability)
sd(chn$Q58_Chinese_ability)

#Proficiency rating of English
mean(eng$Q58_Chinese_ability)
sd(eng$Q58_Chinese_ability)


####FACTOR ANALYSIS
#Pooled Version. This code is similar for all 3 versions
#only difference is in the dataset that is used.

items<-cbind(Q18_Authority_parents,Q19_Authority_teachers,Q20_Collective_neighbors_conflict,Q21_Collective_family,Q22_Collective_unconformity,Q23_Collective_leaders,Q24_Authority_government)
princomp(na.omit(items)) #PCA just for confirmation's sake

#PCA has one important factor, then drops sharply to 2nd. However, it
#tapers off from there. I check three factors on seven questions, 
#but three is probably too many
values.fa3 <- factanal(na.omit(items),factors=3,rotation="varimax",scores="regression")
values.fa3

values.fa2 <- factanal(na.omit(items),factors=2,rotation="varimax",scores="regression")
values.fa2

values.fa1 <- factanal(na.omit(items),factors=1,rotation="varimax")
values.fa1
#one is not enough, so two is sufficient. 
#See Table 2.1 for factor loadings. Factor 1 explains 22.1% of variance,
#Factor 2 explains 6.2% of variance

#MSM next, repeating code
detach(d.data1)
attach(chn)

#PCA
items<-cbind(Q18_Authority_parents,Q19_Authority_teachers,Q20_Collective_neighbors_conflict,Q21_Collective_family,Q22_Collective_unconformity,Q23_Collective_leaders,Q24_Authority_government)
princomp(na.omit(items))

#Factor analysis on 3,2,1 factors
values.fa3 <- factanal(na.omit(items),factors=3,rotation="varimax",scores="regression")
values.fa3

values.fa2 <- factanal(na.omit(items),factors=2,rotation="varimax",scores="regression")
values.fa2

values.fa1 <- factanal(na.omit(items),factors=1,rotation="varimax")
values.fa1

#Again, 1 is not enough, so 2. See Table 2.2 for factor loadings.
#Factor 1 explains 21.7% of variance, Factor 2 explains 9.3% of variance
#Extra question (Q23 on conflict between leaders) loaded onto Factor 2.

#English last
detach(chn)
attach(eng)

#PCA
items<-cbind(Q18_Authority_parents,Q19_Authority_teachers,Q20_Collective_neighbors_conflict,Q21_Collective_family,Q22_Collective_unconformity,Q23_Collective_leaders,Q24_Authority_government)
princomp(na.omit(items))

#Factor analysis on 3,2,1 factors
values.fa3 <- factanal(na.omit(items),factors=3,rotation="varimax",scores="regression")
values.fa3

values.fa2 <- factanal(na.omit(items),factors=2,rotation="varimax",scores="regression")
values.fa2

values.fa1 <- factanal(na.omit(items),factors=1,rotation="varimax")
values.fa1

#Two factors, this time Factor 1 explains 22.4% of variance
#and Factor 2 explains 4.7% of variance.

#going back to pooled set
detach(eng)
attach(d.data1)

#may want to filter just a little more