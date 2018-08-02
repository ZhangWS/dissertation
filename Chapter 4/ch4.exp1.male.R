#################################################
# CHAPTER TWO STATISTICAL ANALYSIS, MALE ANALYSIS
#################################################

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
d.data1 <- filter(d.data, Q59_Travel_abroad >2 & Q53_Gender == 1)
detach(d.data)

d.data1$Lang[d.data1$Lang==2] <- 0
attach(d.data1)

#General descriptives
#I like distributions
nrow(d.data1)
table(Q52_Age)

mean(Q52_Age, na.rm=T)
sd(Q52_Age, na.rm=T)
median(Q52_Age, na.rm=T)

table(Q53_Gender)

#Then split into language groups for separate analysis
eng <- filter(d.data1, Lang == 1)
chn <- filter(d.data1, Lang == 0) 
#CHN is actually coded as 2, but you may change it to 0 
#bc it's recoded to make the legend work

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



####QUESTION ANALYSIS

#Q18: Even though parental demands may be unreasonable, 
#children should still do as they're told

q18.count <- table(Lang, Q18_Authority_parents)
colnames(q18.count) <- c("Strongly Agree\nMore Traditionalist", "Agree", "Disagree", "Strongly Disagree\nMore Individualist")
rownames(q18.count) <- c("MSM","English")
q18.count

q18.freq <- rbind(round(q18.count[1,]/sum(q18.count[1,]),2),round(q18.count[2,]/sum(q18.count[2,]),2))
rownames(q18.freq) <- c("MSM","English")
q18.freq

q18.melt <- melt(q18.freq)
colnames(q18.melt) <- c("Language", "Attitude", "RelFreq")

q18.plot <- ggplot(q18.melt, aes(x=Attitude, y=RelFreq)) + 
  geom_bar(aes(fill=Language), position="dodge", stat="identity") + ylim(breaks=c(0,0.8)) +
  labs(x="\nEven if parents' demands are unreasonable,\nchildren should still do what they ask.",
       y="Relative frequency", title="Figure 4.8b: Relative frequency distribution for\nTraditionalism Battery, item A, male respondents only")
q18.plot 


##Q19
q19.count <- table(Lang, Q19_Authority_teachers)
colnames(q19.count) <- c("Strongly Agree\nMore Traditionalist", "Agree", "Disagree", "Strongly Disagree\nMore Individualist")
rownames(q19.count) <- c("MSM","English")
q19.count

q19.freq <- rbind(round(q19.count[1,]/sum(q19.count[1,]),2),round(q19.count[2,]/sum(q19.count[2,]),2))
rownames(q19.freq) <- c("MSM","English")
q19.freq

q19.melt <- melt(q19.freq)
colnames(q19.melt) <- c("Language", "Attitude", "RelFreq")

q19.plot <- ggplot(q19.melt, aes(x=Attitude, y=RelFreq)) + 
  geom_bar(aes(fill=Language), position="dodge", stat="identity") + ylim(breaks=c(0,0.8)) +
  labs(x="\nBeing a student,\none should not question the authority of their teacher.",
       y="Relative frequency", title="Figure 4.9b: Relative frequency distribution for\nTraditionalism Battery, item B, male respondents only")
q19.plot


###Q20
table(Lang, Q20_Collective_neighbors_conflict)
##we've got 0 in a column. customize all the matrices!
q20.count <- rbind(c(0,11,30,7), c(0,16,12,13))
colnames(q20.count) <- c("Strongly Agree\nMore Traditionalist", "Agree", "Disagree", "Strongly Disagree\nMore Individualist")
rownames(q20.count) <- c("MSM","English")
q20.count

q20.freq <- rbind(round(q20.count[1,]/sum(q20.count[1,]),2),round(q20.count[2,]/sum(q20.count[2,]),2))
rownames(q20.freq) <- c("MSM","English")
q20.freq

q20.melt <- melt(q20.freq)
colnames(q20.melt) <- c("Language", "Attitude", "RelFreq")

q20.plot <- ggplot(q20.melt, aes(x=Attitude, y=RelFreq)) + 
  geom_bar(aes(fill=Language), position="dodge", stat="identity") + ylim(breaks=c(0,0.80)) +
  labs(x="\nWhen one has a conflict with a neighbor,\nthe best way to deal with it is to yield to the other person.",
       y="Relative frequency", title="Figure 4.10b: Relative frequency distribution for\nTraditionalism Battery, item C, male respondents only")
q20.plot


###Q21
q21.count <- table(Lang, Q21_Collective_family)
colnames(q21.count) <- c("Strongly Agree\nMore Traditionalist", "Agree", "Disagree", "Strongly Disagree\nMore Individualist")
rownames(q21.count) <- c("MSM","English")
q21.count

q21.freq <- rbind(round(q21.count[1,]/sum(q21.count[1,]),2),round(q21.count[2,]/sum(q21.count[2,]),2))
rownames(q21.freq) <- c("MSM","English")
q21.freq

q21.melt <- melt(q21.freq)
colnames(q21.melt) <- c("Language", "Attitude", "RelFreq")

q21.plot <- ggplot(q21.melt, aes(x=Attitude, y=RelFreq)) + 
  geom_bar(aes(fill=Language), position="dodge", stat="identity") + ylim(breaks=c(0,0.80)) +
  labs(x="\nFor the sake of the family,\nthe individual should put his personal interests second.",
       y="Relative frequency", title="Figure 4.11b: Relative frequency distribution for\nTraditionalism Battery, item D, male respondents only")
q21.plot


#will need to reverse code Q22 

d.data1$Q22recode <- -1*d.data1$Q22_Collective_unconformity

q22.count <- table(Lang, d.data1$Q22recode)
colnames(q22.count) <- c("Strongly Disgree\nMore Traditionalist", "Disgree", "Agree", "Strongly Agree\nMore Individualist")
rownames(q22.count) <- c("MSM","English")
q22.count

q22.freq <- rbind(round(q22.count[1,]/sum(q22.count[1,]),2),round(q22.count[2,]/sum(q22.count[2,]),2))
rownames(q22.freq) <- c("MSM","English")
q22.freq

q22.melt <- melt(q22.freq)
colnames(q22.melt) <- c("Language", "Attitude", "RelFreq")

q22.plot <- ggplot(q22.melt, aes(x=Attitude, y=RelFreq)) + 
  geom_bar(aes(fill=Language), position="dodge", stat="identity") + ylim(breaks=c(0,0.8)) +
  labs(x="\nSometimes one has to follow one's own beliefs regardless of what other people think.",
       y="Relative frequency", title="Figure 4.12b: Relative frequency distribution for\nTraditionalism Battery, item E, male respondents only")
q22.plot




q23.count <- table(Lang, Q23_Collective_leaders)
colnames(q23.count) <- c("Strongly Agree\nMore Traditionalist", "Agree", "Disagree", "Strongly Disagree\nMore Individualist")
rownames(q23.count) <- c("MSM","English")
q23.count

q23.freq <- rbind(round(q23.count[1,]/sum(q23.count[1,]),2),round(q23.count[2,]/sum(q23.count[2,]),2))
rownames(q23.freq) <- c("MSM","English")
q23.freq

q23.melt <- melt(q23.freq)
colnames(q23.melt) <- c("Language", "Attitude", "RelFreq")

q23.plot <- ggplot(q23.melt, aes(x=Attitude, y=RelFreq)) + 
  geom_bar(aes(fill=Language), position="dodge", stat="identity") + ylim(breaks=c(0,0.80)) +
  labs(x="\nOpen conflicts among politicians are harmful to society.",
       y="Relative frequency", title="Figure 4.13b: Relative frequency distribution for\nTraditionalism Battery, item F, male respondents only")
q23.plot


#Q24


q24.count <- table(Lang, Q24_Authority_government)
colnames(q24.count) <- c("Strongly Agree\nMore Traditionalist", "Agree", "Disagree", "Strongly Disagree\nMore Individualist")
rownames(q24.count) <- c("MSM","English")
q24.count

q24.freq <- rbind(round(q24.count[1,]/sum(q24.count[1,]),2),round(q24.count[2,]/sum(q24.count[2,]),2))
rownames(q24.freq) <- c("MSM","English")
q24.freq

q24.melt <- melt(q24.freq)
colnames(q24.melt) <- c("Language", "Attitude", "RelFreq")

q24.plot <- ggplot(q24.melt, aes(x=Attitude, y=RelFreq)) + 
  geom_bar(aes(fill=Language), position="dodge", stat="identity") + ylim(breaks=c(0,0.80)) +
  labs(x="\nG.	People should always support the decisions\nof their government even if they disagree with them.",
       y="Relative frequency", title="Figure 4.14b: Relative frequency distribution for\nTraditionalism Battery, item G, male respondents only")
q24.plot


#chisq tests!
fisher.test(q18.count)
fisher.test(q19.count)
fisher.test(q20.count)
fisher.test(q21.count)
fisher.test(q22.count)
fisher.test(q23.count)
fisher.test(q24.count)


##################
#DESCRIPTIVE STATISTICS!
##################

median(chn$Q18_Authority_parents, na.rm=T)
median(chn$Q19_Authority_teachers, na.rm=T)
median(chn$Q20_Collective_neighbors_conflict, na.rm=T)
median(chn$Q21_Collective_family, na.rm=T)
median(chn$Q22_Collective_unconformity, na.rm=T)
median(chn$Q23_Collective_leaders, na.rm=T)
median(chn$Q24_Authority_government, na.rm=T)

mean(chn$Q18_Authority_parents, na.rm=T)
mean(chn$Q19_Authority_teachers, na.rm=T)
mean(chn$Q20_Collective_neighbors_conflict, na.rm=T)
mean(chn$Q21_Collective_family, na.rm=T)
mean(chn$Q22_Collective_unconformity, na.rm=T)
mean(chn$Q23_Collective_leaders, na.rm=T)
mean(chn$Q24_Authority_government, na.rm=T)

sd(chn$Q18_Authority_parents, na.rm=T)
sd(chn$Q19_Authority_teachers, na.rm=T)
sd(chn$Q20_Collective_neighbors_conflict, na.rm=T)
sd(chn$Q21_Collective_family, na.rm=T)
sd(chn$Q22_Collective_unconformity, na.rm=T)
sd(chn$Q23_Collective_leaders, na.rm=T)
sd(chn$Q24_Authority_government, na.rm=T)

median(eng$Q18_Authority_parents, na.rm=T)
median(eng$Q19_Authority_teachers, na.rm=T)
median(eng$Q20_Collective_neighbors_conflict, na.rm=T)
median(eng$Q21_Collective_family, na.rm=T)
median(eng$Q22_Collective_unconformity, na.rm=T)
median(eng$Q23_Collective_leaders, na.rm=T)
median(eng$Q24_Authority_government, na.rm=T)


mean(eng$Q18_Authority_parents, na.rm=T)
mean(eng$Q19_Authority_teachers, na.rm=T)
mean(eng$Q20_Collective_neighbors_conflict, na.rm=T)
mean(eng$Q21_Collective_family, na.rm=T)
mean(eng$Q22_Collective_unconformity, na.rm=T)
mean(eng$Q23_Collective_leaders, na.rm=T)
mean(eng$Q24_Authority_government, na.rm=T)

sd(eng$Q18_Authority_parents, na.rm=T)
sd(eng$Q19_Authority_teachers, na.rm=T)
sd(eng$Q20_Collective_neighbors_conflict, na.rm=T)
sd(eng$Q21_Collective_family, na.rm=T)
sd(eng$Q22_Collective_unconformity, na.rm=T)
sd(eng$Q23_Collective_leaders, na.rm=T)
sd(eng$Q24_Authority_government, na.rm=T)


#2 sample t-test and MW's U Test for every combination
t.test(eng$Q18_Authority_parents, chn$Q18_Authority_parents)
wilcox.test(eng$Q18_Authority_parents, chn$Q18_Authority_parents)

t.test(eng$Q19_Authority_teachers, chn$Q19_Authority_teachers)
wilcox.test(eng$Q19_Authority_teachers, chn$Q19_Authority_teachers)

t.test(eng$Q20_Collective_neighbors_conflict, chn$Q20_Collective_neighbors_conflict)
wilcox.test(eng$Q20_Collective_neighbors_conflict, chn$Q20_Collective_neighbors_conflict)

t.test(eng$Q21_Collective_family, chn$Q21_Collective_family)
wilcox.test(eng$Q21_Collective_family, chn$Q21_Collective_family)

t.test(eng$Q22_Collective_unconformity, chn$Q22_Collective_unconformity)
wilcox.test(eng$Q22_Collective_unconformity, chn$Q22_Collective_unconformity)

t.test(eng$Q23_Collective_leaders, chn$Q23_Collective_leaders)
wilcox.test(eng$Q23_Collective_leaders, chn$Q23_Collective_leaders)

t.test(eng$Q24_Authority_government, chn$Q24_Authority_government)
wilcox.test(eng$Q24_Authority_government, chn$Q24_Authority_government)
ks.test(eng$Q24_Authority_government, chn$Q24_Authority_government)


detach(d.data)
attach(d.data1)
q18 <- aov(Q18_Authority_parents~Lang) 
summary(q18)
q18.eta <- EtaSq(q18,type = 2, anova = T)
q18.eta

q19<- aov(Q19_Authority_teachers~Lang) 
summary(q19)
q19.eta <- EtaSq(q19,type = 2, anova = T)
q19.eta

q20<- aov(Q20_Collective_neighbors_conflict~Lang) 
summary(q20)
q20.eta <- EtaSq(q20,type = 2, anova = T)
q20.eta

q21<- aov(Q21_Collective_family~Lang) 
summary(q21)
q21.eta <- EtaSq(q21,type = 2, anova = T)
q21.eta

q22<- aov(Q22_Collective_unconformity~Lang) 
summary(q22)
q22.eta <- EtaSq(q22,type = 2, anova = T)
q22.eta

q23<- aov(Q23_Collective_leaders~Lang) 
summary(q23)
q23.eta <- EtaSq(q23,type = 2, anova = T)
q23.eta
q24<- aov(Q24_Authority_government~Lang) 
summary(q24)
q24.eta <- EtaSq(q24,type = 2, anova = T)
q24.eta