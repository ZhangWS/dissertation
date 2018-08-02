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
library(gridExtra)
library(MASS)

#Your data should be in the same directory as your R Code.
#Otherwise, customize as needed
setwd(".")

#read in master data set
d.data <- read.csv("20171120_dissertation_data.csv")
attach(d.data)
detach(d.data)

#This version, we will only work with the V1 data.
#Not everyone filled out all of the media questions
#Therefore I needed to eliminate all who did NOT fill out the entire battery

d.v1 <- subset(d.data, Version == 1)

##RECODE into 1-0? 0 = Do not use, 1 = Everything else
d.v1$Q3_Media_newspaper[d.v1$Q3_Media_newspaper < 5] <- 1
d.v1$Q3_Media_newspaper[d.v1$Q3_Media_newspaper > 4] <- 0

d.v1$Q4_Media_tv[d.v1$Q4_Media_tv < 5] <- 1
d.v1$Q4_Media_tv[d.v1$Q4_Media_tv > 4] <- 0

d.v1$Q5_Media_radio[d.v1$Q5_Media_radio < 5] <- 1
d.v1$Q5_Media_radio[d.v1$Q5_Media_radio > 4] <- 0

d.v1$Q6_Media_internet[d.v1$Q6_Media_internet < 5] <- 1
d.v1$Q6_Media_internet[d.v1$Q6_Media_internet > 4] <- 0

d.v1$Q7_Pol_discussion[d.v1$Q7_Pol_discussion < 3] <- 1
d.v1$Q7_Pol_discussion[d.v1$Q7_Pol_discussion > 2] <- 0



#split into English and MSM sets
eng <- filter(d.v1, Lang == 1)
chn <- filter(d.v1, Lang == 2)




#do some basic descriptives on the set
nrow(d.v1) #total 190 respondents



####################
#MEDIA QUESTIONS

##############Q3: political news via newspapers (1 Everyday - 5 Not at all)
q3table <- rbind(table(chn$Q3_Media_newspaper), table(eng$Q3_Media_newspaper))
q3sum <- rbind(sum(table(chn$Q3_Media_newspaper)), sum(table(eng$Q3_Media_newspaper)))
q3.count <- cbind(q3table,q3sum) 
rownames(q3.count) <- c("MSM", "English")
colnames(q3.count) <- c("MSM", "Several times", "Once or twice", "Less than once", "Do not use", "Total N")
q3.count

q3.freq <- rbind(q3.count[1,1:5]/q3.count[1,6], q3.count[2,1:5]/q3.count[2,6])
rownames(q3.freq) <- c("MSM","English")
colnames(q3.freq) <- c("Everyday", "Several times", "Once or twice", "Less than once", "Do not use")
q3.freq

q3.total <- rbind(q3.count[1,1:5], q3.freq[1,1:5], q3.count[2,1:5],q3.freq[2,1:5])
q3.blah <- rbind(sum(q3.total[1,]),sum(q3.total[2,]), sum(q3.total[3,]), sum(q3.total[4,]) ) #add in totals
q3.total <- cbind(q3.total, q3.blah)

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
q3.cplot <- ggplot(q3.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,0.45) + xlab("Obtain news about politics in the newspaper") + ylab("Relative Frequency")

#use d.v1 for all media questions

q3.ols <-lm(Q3_Media_newspaper~Lang, data=d.v1)
summary(q3.ols)

q3.probit <- polr(as.ordered(Q3_Media_newspaper) ~ Lang, data = d.v1, method="probit", Hess=TRUE)
summary(q3.probit)
logLik(q3.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
#test to see if variances are similar
fligner.test(Q3_Media_newspaper~Lang, data=d.v1)

wilcox.test(Q3_Media_newspaper ~ Lang, data=d.v1) 
t.test(chn$Q3_Media_newspaper, eng$Q3_Media_newspaper)


##############q4: political news via TV (1 Everyday - 5 Not at all)
q4table <- rbind(table(chn$Q4_Media_tv), table(eng$Q4_Media_tv))
q4sum <- rbind(sum(table(chn$Q4_Media_tv)), sum(table(eng$Q4_Media_tv)))
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
q4.cplot <- ggplot(q4.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,0.45) + xlab("Obtain news about politics on television") + ylab("Relative Frequency")

q4.ols <-lm(Q4_Media_tv~Lang, data=d.v1)
summary(q4.ols)

q4.probit <- polr(as.ordered(Q4_Media_tv) ~ Lang, data = d.v1, method="probit", Hess=TRUE)
summary(q4.probit)
logLik(q4.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
#test to see if variances are similar
fligner.test(Q4_Media_tv~Lang, data=d.v1)

wilcox.test(Q4_Media_tv ~ Lang, data=d.v1) 
t.test(chn$Q4_Media_tv, eng$Q4_Media_tv)

####Q5: political news via Radio (1 Everyday - 5 Not at all)
q5table <- rbind(table(chn$Q5_Media_radio), table(eng$Q5_Media_radio))
q5sum <- rbind(sum(table(chn$Q5_Media_radio)), sum(table(eng$Q5_Media_radio)))
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
q5.cplot <- ggplot(q5.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,0.45) + xlab("Obtain news about politics from the radio") + ylab("Relative Frequency")


q5.ols <-lm(Q5_Media_radio~Lang, data=d.v1)
summary(q5.ols)

q5.probit <- polr(as.ordered(Q5_Media_radio) ~ Lang, data = d.v1, method="probit", Hess=TRUE)
summary(q5.probit)
logLik(q5.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
#test to see if variances are similar
fligner.test(Q5_Media_radio~Lang, data=d.v1)

wilcox.test(Q5_Media_radio ~ Lang, data=d.v1) 
t.test(chn$Q5_Media_radio, eng$Q5_Media_radio)


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
q6.cplot <- ggplot(q6.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,0.75) + xlab("Obtain news about politics from the Internet") + ylab("Relative Frequency")

q6.ols <-lm(Q6_Media_internet, data=d.v1)
summary(q6.ols)

q6.probit <- polr(as.ordered(Q6_Media_internet) ~ Lang, data = d.v1, method="probit", Hess=TRUE)
summary(q6.probit)
logLik(q6.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
#test to see if variances are similar
fligner.test(Q6_Media_internet~Lang, data=d.v1)

wilcox.test(Q6_Media_internet ~ Lang, data=d.v1) 
t.test(chn.eligible$Q6_Media_internet, eng.eligible$Q6_Media_internet)

##############
#Q7 Political discussion analysis

q7table <- rbind(table(chn$Q7_Pol_discussion), table(eng$Q7_Pol_discussion))
q7sum <- rbind(sum(table(chn$Q7_Pol_discussion)), sum(table(eng$Q7_Pol_discussion)))
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
q7.cplot <- ggplot(q7.temp, aes(x=Usage, y=RelFrequency)) + geom_bar(position="dodge", stat="identity", aes(fill=Language)) + ylim(0,1) + xlab("Obtain news about politics from the radio") + ylab("Relative Frequency")
q7.cplot

q7.ols <-lm(Q7_Pol_discussion~Lang, data=d.v1)
summary(q7.ols)

q7.probit <- polr(as.ordered(Q7_Pol_discussion) ~ Lang, data = d.v1, method="probit", Hess=TRUE)
summary(q7.probit)
logLik(q7.probit)

#We can also do it the psych way with (1) a Mann Whitney U Test, which is basically a Kendall's Tau-C (2) Ind-samples t-test
#test to see if variances are similar
fligner.test(Q7_Pol_discussion~Lang, data=d.v1)

wilcox.test(Q7_Pol_discussion ~ Lang, data=d.v1) 
t.test(chn$Q7_Pol_discussion, eng$Q7_Pol_discussion)
