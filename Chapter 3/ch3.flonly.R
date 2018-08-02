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


#subset into the data that we need to analyze
d.fl <- filter(d.data, SurveyNo >= 2200 & SurveyNo <= 2350) ##extra, to test non English-majors sample
chn2 <- filter(d.fl, Lang == 2)
eng2 <- filter(d.fl, Lang == 1)

#how many of each version?
nrow(d.fl) #95
nrow(chn2) #47
nrow(eng2) #48

sum(d.fl$Q55_English == 1, na.rm=T)

table(d.fl$Q52_Age) #table of ages, most between 18 and 21
sum(d.fl$Q52_Age <= 21 & d.fl$Q52_Age >=18, na.rm=T) 


#Total Sample
sum(!is.na(d.fl$Q52_Age)) #90 people gave age info
mean(d.fl$Q52_Age, na.rm=T) #average age 20.98 yo
sd(d.fl$Q52_Age, na.rm=T) #sd 2.31 y
median(d.fl$Q52_Age, na.rm=T) #median & modal respondent is 20 yo

table(d.fl$Q53_Gender) #11 male, 81 female, 92 respondents overall

#Language ability - MSM proficiency
sum(!is.na(chn2$Q58_Chinese_ability)) #46 responses
mean(chn2$Q58_Chinese_ability, na.rm=T) #8.22
sd(chn2$Q58_Chinese_ability, na.rm=T) #1.3

#English Proficiency
sum(!is.na(eng2$Q58_Chinese_ability)) #47 responses
mean(eng2$Q58_Chinese_ability, na.rm=T) #7.19
sd(eng2$Q58_Chinese_ability, na.rm=T) #1.90

####################
#MEDIA QUESTIONS



###Q3: political news via newspapers (1 Everyday - 5 Not at all)
q3table <- rbind(table(chn2$Q3_Media_newspaper), table(eng2$Q3_Media_newspaper))
q3sum <- rbind(sum(table(chn2$Q3_Media_newspaper)), sum(table(eng2$Q3_Media_newspaper)))
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
rownames(q3.total) <- c("MSM", "MSM Rel Freq", "English", "eng2 Rel Freq")
#Table with counts and relative frequency for Q3
q3.total

q3.stats <- rbind(cbind(mean(chn2$Q3_Media_newspaper, na.rm=T), sd(chn2$Q3_Media_newspaper, na.rm=T), median(chn2$Q3_Media_newspaper, na.rm=T)), cbind(mean(eng2$Q3_Media_newspaper, na.rm=T), sd(eng2$Q3_Media_newspaper, na.rm=T), median(chn2$Q3_Media_newspaper, na.rm=T)))
rownames(q3.stats) <- c("MSM", "English")
colnames(q3.stats) <- c("Mean", "Std Dev", "Median")
#Table with basic stats
q3.stats


#What we want are the relative frequencies when making charts for Hadley Wickham
q3.temp <- melt(q3.freq)
colnames(q3.temp) <- c("Language", "Usage", "RelFrequency")
q3.cplot <- ggplot(q3.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,0.75) + xlab("Figure 3.6b: Relative frequency of newspaper consumption for political news, \nFL only") + ylab("Proportional Freq")
q3.cplot


#use d.fl for all media questions

q3.ols <-lm(Q3_Media_newspaper~Lang, data=d.fl)
summary(q3.ols)

q3.probit <- polr(as.ordered(Q3_Media_newspaper) ~ Lang, data = d.fl, method="probit", Hess=TRUE)
summary(q3.probit)
logLik(q3.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
#test to see if variances are similar
fligner.test(Q3_Media_newspaper~Lang, data=d.fl)

wilcox.test(Q3_Media_newspaper ~ Lang, data=d.fl) 
t.test(chn2$Q3_Media_newspaper, eng2$Q3_Media_newspaper)

q3.aov<-aov(Q3_Media_newspaper~Lang, data=d.fl)
etaSquared(q3.aov, type = 2)

###q4: political news via TV (1 Everyday - 5 Not at all)
q4table <- rbind(table(chn2$Q4_Media_tv), table(eng2$Q4_Media_tv))
q4sum <- rbind(sum(table(chn2$Q4_Media_tv)), sum(table(eng2$Q4_Media_tv)))
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
rownames(q4.total) <- c("MSM", "MSM Rel Freq", "English", "eng2 Rel Freq")
#Table with counts and relative frequency for q4
q4.total

q4.stats <- rbind(cbind(mean(chn2$Q4_Media_tv, na.rm=T), sd(chn2$Q4_Media_tv, na.rm=T), median(chn2$Q4_Media_tv, na.rm=T)), cbind(mean(eng2$Q4_Media_tv, na.rm=T), sd(eng2$Q4_Media_tv, na.rm=T), median(chn2$Q4_Media_tv, na.rm=T)))
rownames(q4.stats) <- c("MSM", "English")
colnames(q4.stats) <- c("Mean", "Std Dev", "Median")
#Table with basic stats
q4.stats

q4.temp <- melt(q4.freq)
colnames(q4.temp) <- c("Language", "Usage", "RelFrequency")
q4.cplot <- ggplot(q4.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,0.75) + xlab("Figure 3.7b: Relative frequency of TV consumption for political news, \nFL only") + ylab("Proportional Freq")
q4.cplot

#test to see if variances are similar
fligner.test(Q4_Media_tv~Lang, data=d.fl)
q4.ols <-lm(Q4_Media_tv~Lang, data=d.fl)
summary(q4.ols)

q4.probit <- polr(as.ordered(Q4_Media_tv) ~ Lang, data = d.fl, method="probit", Hess=TRUE)
summary(q4.probit)
logLik(q4.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
fligner.test(Q4_Media_tv~Lang, data=d.fl)
wilcox.test(Q4_Media_tv ~ Lang, data=d.fl) 
t.test(chn2$Q4_Media_tv, eng2$Q4_Media_tv)

q4.aov <- aov(Q4_Media_tv~Lang, data=d.fl)
etaSquared(q4.aov, type = 2)


###Q5: political news via Radio (1 Everyday - 5 Not at all)
q5table <- rbind(table(chn2$Q5_Media_radio), table(eng2$Q5_Media_radio))
q5sum <- rbind(sum(table(chn2$Q5_Media_radio)), sum(table(eng2$Q5_Media_radio)))
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
rownames(q5.total) <- c("MSM", "MSM Rel Freq", "English", "eng2 Rel Freq")
#Table with counts and relative frequency for q5
q5.total

q5.stats <- rbind(cbind(mean(chn2$Q5_Media_radio, na.rm=T), sd(chn2$Q5_Media_radio, na.rm=T), median(chn2$Q5_Media_radio, na.rm=T)), cbind(mean(eng2$Q5_Media_radio, na.rm=T), sd(eng2$Q5_Media_radio, na.rm=T), median(chn2$Q5_Media_radio, na.rm=T)))

rownames(q5.stats) <- c("MSM", "English")
colnames(q5.stats) <- c("Mean", "Std Dev", "Median")
#Table with basic stats
q5.stats


q5.temp <- melt(q5.freq)
colnames(q5.temp) <- c("Language", "Usage", "RelFrequency")
q5.cplot <- ggplot(q5.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,0.75) + xlab("Figure 3.8b: Relative frequency of radio consumption for political news,\nFL only") + ylab("Proportional Freq")
q5.cplot

q5.ols <-lm(Q5_Media_radio~Lang, data=d.fl)
summary(q5.ols)

q5.probit <- polr(as.ordered(Q5_Media_radio) ~ Lang, data = d.fl, method="probit", Hess=TRUE)
summary(q5.probit)
logLik(q5.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
#test to see if variances are similar
fligner.test(Q5_Media_radio~Lang, data=d.fl)
wilcox.test(Q5_Media_radio ~ Lang, data=d.fl) 
t.test(chn2$Q5_Media_radio, eng2$Q5_Media_radio)

q5.aov<-aov(Q5_Media_radio~Lang, data=d.fl)
etaSquared(q5.aov, type = 2)


####q6: political news via Radio (1 Everyday - 5 Not at all)
table(chn2$Q6_Media_internet)
chn.fl.internet <- c(30,7,8,0,1)
q6table <- rbind(chn.fl.internet, table(eng2$Q6_Media_internet))
q6sum <- rbind(sum(chn.fl.internet), sum(table(eng2$Q6_Media_internet)))
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
rownames(q6.total) <- c("MSM", "MSM Rel Freq", "English", "eng2 Rel Freq")
#Table with counts and relative frequency for q6
q6.total

q6.stats <- rbind(cbind(mean(chn2$Q6_Media_internet, na.rm=T), sd(chn2$Q6_Media_internet, na.rm=T), median(chn2$Q6_Media_internet, na.rm=T)), cbind(mean(eng2$Q6_Media_internet, na.rm=T), sd(eng2$Q6_Media_internet, na.rm=T), median(chn2$Q6_Media_internet, na.rm=T)))

rownames(q6.stats) <- c("MSM", "English")
colnames(q6.stats) <- c("Mean", "Std Dev", "Median")
#Table with basic stats
q6.stats


q6.temp <- melt(q6.freq)
colnames(q6.temp) <- c("Language", "Usage", "RelFrequency")
q6.cplot <- ggplot(q6.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,0.75) + xlab("Figure 3.9b: Relative frequency of Internet consumption for political news,\nFL only") + ylab("Proportional Freq")
q6.cplot


q6.ols <-lm(Q6_Media_internet, data=d.fl)
summary(q6.ols)

q6.probit <- polr(as.ordered(Q6_Media_internet) ~ Lang, data = d.fl, method="probit", Hess=TRUE)
summary(q6.probit)
logLik(q6.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
#test to see if variances are similar
fligner.test(Q6_Media_internet~Lang, data=d.fl)
wilcox.test(Q6_Media_internet ~ Lang, data=d.fl) 
t.test(chn2$Q6_Media_internet, eng2$Q6_Media_internet)

q6.aov<-aov(Q6_Media_internet~Lang, data=d.fl)
etaSquared(q6.aov, type = 2)



###Q7 Political discussion analysis
q7table <- rbind(table(chn2$Q7_Pol_discussion), table(eng2$Q7_Pol_discussion))
q7sum <- rbind(sum(table(chn2$Q7_Pol_discussion)), sum(table(eng2$Q7_Pol_discussion)))
q7.count <- cbind(q7table,q7sum) 
rownames(q7.count) <- c("MSM", "English")
colnames(q7.count) <- c("Often", "Occasionally", "Never", "Total N")
q7.count

q7.freq <- rbind(q7.count[1,1:3]/q7.count[1,4], q7.count[2,1:3]/q7.count[2,4])
rownames(q7.freq) <- c("MSM","English")
colnames(q7.freq) <- c("Often", "Occasionally", "Never")
q7.freq

q7.total <- rbind(q7.count[1,1:3], q7.freq[1,1:3], q7.count[2,1:3],q7.freq[2,1:3])
q7.blah <- rbind(sum(q7.total[1,]),sum(q7.total[2,]), sum(q7.total[3,]), sum(q7.total[4,]) ) #add in totals
q7.total <- cbind(q7.total, q7.blah)

colnames(q7.total) <- c("Often", "Occasionally", "Never", "Total")
rownames(q7.total) <- c("MSM", "MSM Rel Freq", "English", "eng2 Rel Freq")
#Table with counts and relative frequency for q7
q7.total

q7.stats <- rbind(cbind(mean(chn2$Q7_Pol_discussion, na.rm=T), sd(chn2$Q7_Pol_discussion, na.rm=T), median(chn2$Q7_Pol_discussion, na.rm=T)), cbind(mean(eng2$Q7_Pol_discussion, na.rm=T), sd(eng2$Q7_Pol_discussion, na.rm=T), median(chn2$Q7_Pol_discussion, na.rm=T)))

rownames(q7.stats) <- c("MSM", "English")
colnames(q7.stats) <- c("Mean", "Std Dev", "Median")
#Table with basic stats
q7.stats


q7.temp <- melt(q7.freq)
colnames(q7.temp) <- c("Language", "Usage", "RelFrequency")
q7.cplot <- ggplot(q7.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,1) + xlab("Figure 3.10b: Relative frequency of political discussion with family and close friends,\nFL only") + ylab("Proportional Freq")
q7.cplot

q7.ols <-lm(Q7_Pol_discussion~Lang, data=d.fl)
summary(q7.ols)

q7.probit <- polr(as.ordered(Q7_Pol_discussion) ~ Lang, data = d.fl, method="probit", Hess=TRUE)
summary(q7.probit)
logLik(q7.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
#test to see if variances are similar
fligner.test(Q7_Pol_discussion~Lang, data=d.fl)
wilcox.test(Q7_Pol_discussion ~ Lang, data=d.fl) 
t.test(chn2$Q7_Pol_discussion, eng2$Q7_Pol_discussion)

q7.aov<-aov(Q7_Pol_discussion~Lang, data=d.fl)
etaSquared(q7.aov, type = 2)


cohensD(Q3_Media_newspaper~Lang, data=d.fl)
cohensD(Q4_Media_tv~Lang, data=d.fl)
cohensD(Q5_Media_radio~Lang, data=d.fl)
cohensD(Q6_Media_internet~Lang, data=d.fl)
cohensD(Q7_Pol_discussion~Lang, data=d.fl)
