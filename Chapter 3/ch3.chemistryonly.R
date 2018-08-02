####################################
# CHAPTER THREE STATISTICAL ANALYSIS CHEM ONLY ANALYSIS
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

#subset into the data that we need to analyze
d.chem <- filter(d.data, SurveyNo >=2000 & SurveyNo <= 2150) ##extra, to test non English-majors sample
chn <- filter(d.chem, Lang == 2)
eng <- filter(d.chem, Lang == 1)

#how many of each version?
nrow(d.chem) #95
nrow(chn) #44
nrow(eng) #51

table(d.chem$Q52_Age) #table of ages, most between 18 and 21
sum(d.chem$Q52_Age <= 21 & d.chem$Q52_Age >=18, na.rm=T) 


#Total Sample
sum(!is.na(d.chem$Q52_Age)) #95 people gave age info
mean(d.chem$Q52_Age, na.rm=T) #average age 19.23 yo
sd(d.chem$Q52_Age, na.rm=T) #sd 0.764 y
median(d.chem$Q52_Age, na.rm=T) #median & modal respondent is 19.5 yo

table(d.chem$Q53_Gender) #17 male, 78 female, 95 respondents overall

#Language ability - MSM proficiency
sum(!is.na(chn$Q58_Chinese_ability)) #44 responses
mean(chn$Q58_Chinese_ability, na.rm=T) #8.61
sd(chn$Q58_Chinese_ability, na.rm=T) #1.26

#English Proficiency
sum(!is.na(eng$Q58_Chinese_ability)) #51 responses
mean(eng$Q58_Chinese_ability, na.rm=T) #7.23
sd(eng$Q58_Chinese_ability, na.rm=T) #2.17

####################
#MEDIA QUESTIONS



###Q3: political news via newspapers (1 Everyday - 5 Not at all)
table(chn$Q3_Media_newspaper) #there is no 2 in chem!
eng.chem.newspaper <- c(2,0,7,12,18)

q3table <- rbind(eng.chem.newspaper, table(eng$Q3_Media_newspaper))
q3sum <- rbind(sum(table(chn$Q3_Media_newspaper)), sum(eng.chem.newspaper))
q3.count <- cbind(q3table,q3sum) 
rownames(q3.count) <- c("MSM", "English")
colnames(q3.count) <- c("Everyday", "Several times", "Once or twice", "Less than once", "Do not use", "Total N")
q3.count

q3.freq <- rbind(q3.count[1,1:5]/q3.count[1,6], q3.count[2,1:5]/q3.count[2,6])
rownames(q3.freq) <- c("MSM","English")
colnames(q3.freq) <- c("Everyday", "Several times", "Once or twice", "Less than once", "Do not use")
q3.freq

q3.total <- rbind(q3.count[1,1:5], q3.freq[1,1:5], q3.count[2,1:5],q3.freq[2,1:5])
q3.blah <- rbind(sum(q3.total[1,]),sum(q3.total[2,]), sum(q3.total[3,]), sum(q3.total[4,]) ) #add in totals
q3.total <- cbind(q3.total, q3.blah)
q3.total

colnames(q3.total) <- c("Everyday", "Several times", "Once or twice", "Less than once", "Do not use", "Total")
rownames(q3.total) <- c("MSM", "MSM Rel Freq", "English", "Eng Rel Freq")
#Table with counts and relative frequency for Q3
q3.total

q3.stats <- rbind(cbind(mean(chn$Q3_Media_newspaper, na.rm=T), sd(chn$Q3_Media_newspaper, na.rm=T), median(chn$Q3_Media_newspaper, na.rm=T)), cbind(mean(eng$Q3_Media_newspaper, na.rm=T), sd(eng$Q3_Media_newspaper, na.rm=T), median(chn$Q3_Media_newspaper, na.rm=T)))
rownames(q3.stats) <- c("MSM", "English")
colnames(q3.stats) <- c("Mean", "Std Dev", "Median")
#Table with basic stats
q3.stats


#What we want are the relative frequencies when making charts for Hadley Wickham
q3.temp <- melt(q3.freq)
colnames(q3.temp) <- c("Language", "Usage", "RelFrequency")
q3.cplot <- ggplot(q3.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,0.75) + xlab("Figure 3.6a: Relative frequency of newspaper consumption for political news, \nChemistry only") + ylab("Proportional Freq")
q3.cplot


#use d.chem for all media questions

q3.ols <-lm(Q3_Media_newspaper~Lang, data=d.chem)
summary(q3.ols)

q3.probit <- polr(as.ordered(Q3_Media_newspaper) ~ Lang, data = d.chem, method="probit", Hess=TRUE)
summary(q3.probit)
logLik(q3.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
#test to see if variances are similar
fligner.test(Q3_Media_newspaper~Lang, data=d.chem)

wilcox.test(Q3_Media_newspaper ~ Lang, data=d.chem) 
t.test(chn$Q3_Media_newspaper, eng$Q3_Media_newspaper)


q3.aov<-aov(Q3_Media_newspaper~Lang, data=d.chem)
etaSquared(q3.aov, type = 2)

###q4: political news via TV (1 Everyday - 5 Not at all)
chn.chem.tv<- c(0, 5,9, 12, 13)

q4table <- rbind(chn.chem.tv, table(eng$Q4_Media_tv))
q4sum <- rbind(sum(chn.chem.tv), sum(table(eng$Q4_Media_tv)))
q4.count <- cbind(q4table,q4sum) 
rownames(q4.count) <- c("MSM", "English")
colnames(q4.count) <- c("Everyday", "Several times", "Once or twice", "Less than once", "Do not use", "Total N")
q4.count

q4.freq <- rbind(q4.count[1,1:5]/q4.count[1,6], q4.count[2,1:5]/q4.count[2,6])
rownames(q4.freq) <- c("MSM","English")
colnames(q4.freq) <- c("Everyday", "Several times", "Once or twice", "Less than once", "Do not use")
q4.freq

q4.total <- rbind(q4.count[1,1:5], q4.freq[1,1:5], q4.count[2,1:5],q4.freq[2,1:5])
q4.blah <- rbind(sum(q4.total[1,]),sum(q4.total[2,]), sum(q4.total[3,]), sum(q4.total[4,]) ) #add in totals
q4.total <- cbind(q4.total, q4.blah)

colnames(q4.total) <- c("Everyday", "Several times", "Once or twice", "Less than once", "Do not use", "Total")
rownames(q4.total) <- c("MSM", "MSM Rel Freq", "English", "Eng Rel Freq")
#Table with counts and relative frequency for q4
q4.total

q4.stats <- rbind(cbind(mean(chn$Q4_Media_tv, na.rm=T), sd(chn$Q4_Media_tv, na.rm=T), median(chn$Q4_Media_tv, na.rm=T)), cbind(mean(eng$Q4_Media_tv, na.rm=T), sd(eng$Q4_Media_tv, na.rm=T), median(chn$Q4_Media_tv, na.rm=T)))

rownames(q4.stats) <- c("MSM", "English")
colnames(q4.stats) <- c("Mean", "Std Dev", "Median")
#Table with basic stats
q4.stats

q4.temp <- melt(q4.freq)
colnames(q4.temp) <- c("Language", "Usage", "RelFrequency")
q4.cplot <- ggplot(q4.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,0.75) + xlab("Figure 3.7a: Relative frequency of TV consumption for political news,\nChemistry only") + ylab("Proportional Freq")
q4.cplot

#test to see if variances are similar
fligner.test(Q4_Media_tv~Lang, data=d.chem)
q4.ols <-lm(Q4_Media_tv~Lang, data=d.chem)
summary(q4.ols)

q4.probit <- polr(as.ordered(Q4_Media_tv) ~ Lang, data = d.chem, method="probit", Hess=TRUE)
summary(q4.probit)
logLik(q4.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
fligner.test(Q4_Media_tv~Lang, data=d.chem)
wilcox.test(Q4_Media_tv ~ Lang, data=d.chem) 
t.test(chn$Q4_Media_tv, eng$Q4_Media_tv)
ks.test(chn$Q4_Media_tv, eng$Q4_Media_tv)

q4.aov <- aov(Q4_Media_tv~Lang, data=d.chem)
etaSquared(q4.aov, type = 2)


###Q5: political news via Radio (1 Everyday - 5 Not at all)
chn.chem.radio <- c(0,4,7,12,16)
eng.chem.radio <- c(0,4,3,17,20)
q5table <- rbind(chn.chem.radio, eng.chem.radio)
q5sum <- rbind(sum(chn.chem.radio), sum(eng.chem.radio))
q5.count <- cbind(q5table,q5sum) 
rownames(q5.count) <- c("MSM", "English")
colnames(q5.count) <- c("Everyday", "Several times", "Once or twice", "Less than once", "Do not use", "Total N")
q5.count

q5.freq <- rbind(q5.count[1,1:5]/q5.count[1,6], q5.count[2,1:5]/q5.count[2,6])
rownames(q5.freq) <- c("MSM","English")
colnames(q5.freq) <- c("Everyday", "Several times", "Once or twice", "Less than once", "Do not use")
q5.freq

q5.total <- rbind(q5.count[1,1:5], q5.freq[1,1:5], q5.count[2,1:5],q5.freq[2,1:5])
q5.blah <- rbind(sum(q5.total[1,]),sum(q5.total[2,]), sum(q5.total[3,]), sum(q5.total[4,]) ) #add in totals
q5.total <- cbind(q5.total, q5.blah)

colnames(q5.total) <- c("Everyday", "Several times", "Once or twice", "Less than once", "Do not use", "Total")
rownames(q5.total) <- c("MSM", "MSM Rel Freq", "English", "Eng Rel Freq")
#Table with counts and relative frequency for q5
q5.total

q5.stats <- rbind(cbind(mean(chn$Q5_Media_radio, na.rm=T), sd(chn$Q5_Media_radio, na.rm=T), median(chn$Q5_Media_radio, na.rm=T)), cbind(mean(eng$Q5_Media_radio, na.rm=T), sd(eng$Q5_Media_radio, na.rm=T), median(chn$Q5_Media_radio, na.rm=T)))

rownames(q5.stats) <- c("MSM", "English")
colnames(q5.stats) <- c("Mean", "Std Dev", "Median")
#Table with basic stats
q5.stats


q5.temp <- melt(q5.freq)
colnames(q5.temp) <- c("Language", "Usage", "RelFrequency")
q5.cplot <- ggplot(q5.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,0.75) + xlab("Figure 3.8a: Relative frequency of radio consumption for political news, \nChemistry only") + ylab("Proportional Freq")
q5.cplot

q5.ols <-lm(Q5_Media_radio~Lang, data=d.chem)
summary(q5.ols)

q5.probit <- polr(as.ordered(Q5_Media_radio) ~ Lang, data = d.chem, method="probit", Hess=TRUE)
summary(q5.probit)
logLik(q5.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
#test to see if variances are similar
fligner.test(Q5_Media_radio~Lang, data=d.chem)
wilcox.test(Q5_Media_radio ~ Lang, data=d.chem) 
t.test(chn$Q5_Media_radio, eng$Q5_Media_radio)

q5.aov<-aov(Q5_Media_radio~Lang, data=d.chem)
etaSquared(q5.aov, type = 2)

####q6: political news via Radio (1 Everyday - 5 Not at all)
q6table <- rbind(table(chn$Q6_Media_internet), table(eng$Q6_Media_internet))
q6sum <- rbind(sum(table(chn$Q6_Media_internet)), sum(table(eng$Q6_Media_internet)))
q6.count <- cbind(q6table,q6sum) 
rownames(q6.count) <- c("MSM", "English")
colnames(q6.count) <- c("Everyday", "Several times", "Once or twice", "Less than once", "Do not use", "Total N")
q6.count

q6.freq <- rbind(q6.count[1,1:5]/q6.count[1,6], q6.count[2,1:5]/q6.count[2,6])
rownames(q6.freq) <- c("MSM","English")
colnames(q6.freq) <- c("Everyday", "Several times", "Once or twice", "Less than once", "Do not use")
q6.freq

q6.total <- rbind(q6.count[1,1:5], q6.freq[1,1:5], q6.count[2,1:5],q6.freq[2,1:5])
q6.blah <- rbind(sum(q6.total[1,]),sum(q6.total[2,]), sum(q6.total[3,]), sum(q6.total[4,]) ) #add in totals
q6.total <- cbind(q6.total, q6.blah)

colnames(q6.total) <- c("Everyday", "Several times", "Once or twice", "Less than once", "Do not use", "Total")
rownames(q6.total) <- c("MSM", "MSM Rel Freq", "English", "Eng Rel Freq")
#Table with counts and relative frequency for q6
q6.total

q6.stats <- rbind(cbind(mean(chn$Q6_Media_internet, na.rm=T), sd(chn$Q6_Media_internet, na.rm=T), median(chn$Q6_Media_internet, na.rm=T)), cbind(mean(eng$Q6_Media_internet, na.rm=T), sd(eng$Q6_Media_internet, na.rm=T), median(chn$Q6_Media_internet, na.rm=T)))

rownames(q6.stats) <- c("MSM", "English")
colnames(q6.stats) <- c("Mean", "Std Dev", "Median")
#Table with basic stats
q6.stats


q6.temp <- melt(q6.freq)
colnames(q6.temp) <- c("Language", "Usage", "RelFrequency")
q6.cplot <- ggplot(q6.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,0.75) + xlab("Figure 3.9a: Relative frequency of Internet consumption for political news, \nChemistry only") + ylab("Proportional Freq")
q6.cplot


q6.ols <-lm(Q6_Media_internet~Lang, data=d.chem)
summary(q6.ols)

q6.probit <- polr(as.ordered(Q6_Media_internet) ~ Lang, data = d.chem, method="probit", Hess=TRUE)
summary(q6.probit)
logLik(q6.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
#test to see if variances are similar
fligner.test(Q6_Media_internet~Lang, data=d.chem)
wilcox.test(Q6_Media_internet ~ Lang, data=d.chem) 
t.test(chn$Q6_Media_internet, eng$Q6_Media_internet)

q6.aov<-aov(Q6_Media_internet~Lang, data=d.chem)
etaSquared(q6.aov, type = 2)




###Q7 Political discussion analysis
q7table <- rbind(table(chn$Q7_Pol_discussion), table(eng$Q7_Pol_discussion))
q7sum <- rbind(sum(table(chn$Q7_Pol_discussion)), sum(table(eng$Q7_Pol_discussion)))
q7.count <- cbind(q7table,q7sum) 
rownames(q7.count) <- c("MSM", "English")
colnames(q7.count) <- c("Often", "Occasionally", "Never", "Total N")
q7.count7

q7.freq <- rbind(q7.count[1,1:3]/q7.count[1,4], q7.count[2,1:3]/q7.count[2,4])
rownames(q7.freq) <- c("MSM","English")
colnames(q7.freq) <- c("Often", "Occasionally", "Never")
q7.freq

q7.total <- rbind(q7.count[1,1:3], q7.freq[1,1:3], q7.count[2,1:3],q7.freq[2,1:3])
q7.blah <- rbind(sum(q7.total[1,]),sum(q7.total[2,]), sum(q7.total[3,]), sum(q7.total[4,]) ) #add in totals
q7.total <- cbind(q7.total, q7.blah)

colnames(q7.total) <- c("Often", "Occasionally", "Never", "Total")
rownames(q7.total) <- c("MSM", "MSM Rel Freq", "English", "Eng Rel Freq")
#Table with counts and relative frequency for q7
q7.total

q7.stats <- rbind(cbind(mean(chn$Q7_Pol_discussion, na.rm=T), sd(chn$Q7_Pol_discussion, na.rm=T), median(chn$Q7_Pol_discussion, na.rm=T)), cbind(mean(eng$Q7_Pol_discussion, na.rm=T), sd(eng$Q7_Pol_discussion, na.rm=T), median(chn$Q7_Pol_discussion, na.rm=T)))

rownames(q7.stats) <- c("MSM", "English")
colnames(q7.stats) <- c("Mean", "Std Dev", "Median")
#Table with basic stats
q7.stats


q7.temp <- melt(q7.freq)
colnames(q7.temp) <- c("Language", "Usage", "RelFrequency")
q7.cplot <- ggplot(q7.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,1) + xlab("Figure 3.10a: Relative frequency of political discussion with family and close friends, \nChemistry only") + ylab("Proportional Freq")
q7.cplot

q7.ols <-lm(Q7_Pol_discussion~Lang, data=d.chem)
summary(q7.ols)

q7.probit <- polr(as.ordered(Q7_Pol_discussion) ~ Lang, data = d.chem, method="probit", Hess=TRUE)
summary(q7.probit)
logLik(q7.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
#test to see if variances are similar
fligner.test(Q7_Pol_discussion~Lang, data=d.chem)
wilcox.test(Q7_Pol_discussion ~ Lang, data=d.chem) 
t.test(chn$Q7_Pol_discussion, eng$Q7_Pol_discussion)

q7.aov<-aov(Q7_Pol_discussion~Lang, data=d.chem)
etaSquared(q7.aov, type = 2)


cohensD(Q3_Media_newspaper~Lang, data=d.chem)
cohensD(Q4_Media_tv~Lang, data=d.chem)
cohensD(Q5_Media_radio~Lang, data=d.chem)
cohensD(Q6_Media_internet~Lang, data=d.chem)
cohensD(Q7_Pol_discussion~Lang, data=d.chem)
