###################################################
# CHAPTER TWO STATISTICAL ANALYSIS, FEMALE ANALYSIS
###################################################

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
d.data1 <- filter(d.data, Version == 1)
d.data1 <- filter(d.data1, !is.na(Q34_Democratic_assessment) | 
                    !is.na(Q35_Democratic_desired) |
                    !is.na(Q36_Democratic_appropriateness))
detach(d.data)

##Let's work on Experiment 2, which has ONLY THREE QUESTIONS :D:D:D

nrow(d.data1)
table(Q52_Age)

mean(Q52_Age, na.rm=T)
sd(Q52_Age, na.rm=T)
median(Q52_Age, na.rm=T)

table(Q53_Gender)

d.data1$Lang[d.data1$Lang == 2] <-0
attach(d.data1)

#Then split into language groups for separate analysis
eng <- filter(d.data1, Lang == 1)
chn <- filter(d.data1, Lang == 0)

#proficiency rating of Chinese
mean(chn$Q58_Chinese_ability, na.rm=T)
sd(chn$Q58_Chinese_ability, na.rm=T)

#Proficiency rating of English
mean(eng$Q58_Chinese_ability, na.rm=T)
sd(eng$Q58_Chinese_ability, na.rm=T)


#################
#Okay, let get this last "official" analysis on the road!

#Q34: Assess how democratic your country is

q34.count <- table(Lang, Q34_Democratic_assessment)
rownames(q34.count) <- c("MSM","English")
q34.count

q34.freq <- rbind(round(q34.count[1,]/sum(q34.count[1,]),2),round(q34.count[2,]/sum(q34.count[2,]),2))
rownames(q34.freq) <- c("MSM","English")
q34.freq

q34.temp <- melt(q34.freq)
colnames(q34.temp) <- c("Language","Score","RelFreq")


q34.plot <- ggplot(d.data1, aes(Q34_Democratic_assessment, fill=factor(Lang))) +
  geom_histogram(aes(y=..density..), position="identity", bins=10) + 
  scale_fill_discrete(name="Language", labels=c("MSM", "English"))+ylim(breaks=c(0,0.3)) + 
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +  geom_density(alpha=0) +
  facet_wrap(~Lang,nrow=2) + theme(strip.text.x = element_blank()) +labs(x="1 = Not a democracy, 10 = Completely democratic", 
                                                                         y="Relative Freq", title="Figure 4.15: Relative frequency distribution of assessing democracy\n in PRC by language group")
q34.plot



###Q35

q35.count <- table(d.data1$Lang, d.data1$Q35_Democratic_desired)
rownames(q35.count) <- c("MSM","English")
q35.count

q35.freq <- rbind(round(q35.count[1,]/sum(q35.count[1,]),2),round(q35.count[2,]/sum(q35.count[2,]),2))
rownames(q35.freq) <- c("MSM","English")
q35.freq

q35.temp <- melt(q35.freq)
colnames(q35.temp) <- c("Language","Score","RelFreq")


q35.plot <- ggplot(d.data1, aes(Q35_Democratic_desired, fill=factor(Lang))) +
  geom_histogram(aes(y=..density..), position="identity", bins=10) + 
  scale_fill_discrete(name="Language", labels=c("MSM", "English"))+ylim(breaks=c(0,0.3)) + 
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +  geom_density(alpha=0) +
  facet_wrap(~Lang,nrow=2) + theme(strip.text.x = element_blank()) +labs(x="1 = Not a democracy, 10 = Completely Democratic", 
                                                                         y="Relative Freq", title="Figure 4.16: Relative frequency distribution of desire for democracy\nin PRC by language group")
q35.plot




#Q36
#Q35: Desire for democracy

q36.count <- table(Lang, Q36_Democratic_appropriateness)
rownames(q36.count) <- c("MSM","English")
q36.count

q36.freq <- rbind(round(q36.count[1,]/sum(q36.count[1,]),2),round(q36.count[2,]/sum(q36.count[2,]),2))
rownames(q36.freq) <- c("MSM","English")
q36.freq

q36.temp <- melt(q36.freq)
colnames(q36.temp) <- c("Language","Score","RelFreq")


q36.plot <- ggplot(d.data1, aes(Q36_Democratic_appropriateness, fill=factor(Lang))) +
  geom_histogram(aes(y=..density..), position="identity", bins=10) + 
  scale_fill_discrete(name="Language", labels=c("MSM", "English"))+ylim(breaks=c(0,0.3)) + 
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +  geom_density(alpha=0) +
  facet_wrap(~Lang,nrow=2) + theme(strip.text.x = element_blank()) +labs(x="1 = Completely unsuitable, 10 = Completely suitable", 
                                                                         y="Relative Freq", title="Figure 4.17: Relative frequency distribution of suitability\nof democracy for PRC by language group")
q36.plot


####

###Calculate a bunch of descriptives

fisher.test(q34.count)
fisher.test(q35.count)
fisher.test(q36.count)


detach(d.data1)
chn <- filter(d.data1, Lang == 0)
eng <- filter(d.data1, Lang == 1)

attach(chn)
median(Q34_Democratic_assessment, na.rm=T)
mean(Q34_Democratic_assessment, na.rm=T)
sd(Q34_Democratic_assessment, na.rm=T)

median(Q35_Democratic_desired, na.rm=T)
mean(Q35_Democratic_desired, na.rm=T)
sd(Q35_Democratic_desired, na.rm=T)

median(Q36_Democratic_appropriateness, na.rm=T)
mean(Q36_Democratic_appropriateness, na.rm=T)
sd(Q36_Democratic_appropriateness, na.rm=T)

detach(chn)


attach(eng)
median(Q34_Democratic_assessment, na.rm=T)
mean(Q34_Democratic_assessment, na.rm=T)
sd(Q34_Democratic_assessment, na.rm=T)

median(Q35_Democratic_desired, na.rm=T)
mean(Q35_Democratic_desired, na.rm=T)
sd(Q35_Democratic_desired, na.rm=T)

median(Q36_Democratic_appropriateness, na.rm=T)
mean(Q36_Democratic_appropriateness, na.rm=T)
sd(Q36_Democratic_appropriateness, na.rm=T)

detach(eng)


t.test(eng$Q34_Democratic_assessment, chn$Q34_Democratic_assessment)
t.test(eng$Q35_Democratic_desired, chn$Q35_Democratic_desired)
t.test(eng$Q36_Democratic_appropriateness, chn$Q36_Democratic_appropriateness)

#Eta-square?

q34 <- aov(d.data1$Q34_Democratic_assessment~d.data1$Lang)
q34.eta <- EtaSq(q34,type = 2, anova = T)
q34.eta

q35 <- aov(d.data1$Q35_Democratic_desired~d.data1$Lang)
q35.eta <- EtaSq(q35,type = 2, anova = T)
q35.eta

q36 <- aov(d.data1$Q36_Democratic_appropriateness~d.data1$Lang)
q36.eta <- EtaSq(q36,type = 2, anova = T)
q36.eta

wilcox.test(eng$Q34_Democratic_assessment, chn$Q34_Democratic_assessment)
wilcox.test(eng$Q35_Democratic_desired, chn$Q35_Democratic_desired)
wilcox.test(eng$Q36_Democratic_appropriateness, chn$Q36_Democratic_appropriateness)

ks.test(eng$Q34_Democratic_assessment, chn$Q34_Democratic_assessment)
ks.test(eng$Q35_Democratic_desired, chn$Q35_Democratic_desired)
ks.test(eng$Q36_Democratic_appropriateness, chn$Q36_Democratic_appropriateness)


table(chn$Q34_Democratic_assessment)

chn.q34 <- c(6,3,14,13,14,12,16,7,3,0)
fisher.test(chn.q34,table(eng$Q34_Democratic_assessment) )

ks.test(eng$Q35_Democratic_desired, chn$Q35_Democratic_desired)
ks.test(eng$Q36_Democratic_appropriateness, chn$Q36_Democratic_appropriateness)

