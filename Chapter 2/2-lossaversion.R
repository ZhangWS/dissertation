################
#LOSS AVERSION: Q8-Q13, Q39-Q44 V1 / Q33-Q38 V2 
#UG: Q27 V1 / Q26 V2, Q51 V1 / Q44 V2


#let's plot some expected L/G values to see if they are monotonic per BJG's emails
lowev <- read.csv("ch1.lowev.csv")
highev <- read.csv("ch1.highev.csv")


lowev.1<-subset(lowev, select=-c(1,2))
highev.1<- subset(highev, select=-c(1,2))

lowev.melt <- melt(lowev.1,id.vars = 4)
highev.melt <- melt(highev.1, id.vars=4)


lowev.plot <- ggplot(data=lowev.melt, aes(x=L.G, y=value, colour=factor(variable))) 
lowev.plot + geom_point()


highev.plot <- ggplot(data=highev.melt, aes(x=L.G, y=value, colour=factor(variable)))
highev.plot + geom_point() 

######SMALL WAGERS
q8 <- table(Lang, Q8_Betting_8)
q8.ctable <- rbind(q8[1,]/sum(q8[1,]), q8[2,]/sum(q8[2,]))
colnames(q8.ctable) <- c("Accept Bet", "Decline Bet")
rownames(q8.ctable) <- c("English", "Chinese")

q9 <- table(Lang, Q9_Betting_7)
q9.ctable <- rbind(q9[1,]/sum(q9[1,]), q9[2,]/sum(q9[2,]))
colnames(q9.ctable) <- c("Accept Bet", "Decline Bet")
rownames(q9.ctable) <- c("English", "Chinese")

q10 <- table(Lang, Q10_Betting_10)
q10.ctable <- rbind(q10[1,]/sum(q10[1,]), q10[2,]/sum(q10[2,]))
colnames(q10.ctable) <- c("Accept Bet", "Decline Bet")
rownames(q10.ctable) <- c("English", "Chinese")


q11 <- table(Lang, Q11_Betting_9)
q11.ctable <- rbind(q11[1,]/sum(q11[1,]), q11[2,]/sum(q11[2,]))
colnames(q11.ctable) <- c("Accept Bet", "Decline Bet")
rownames(q11.ctable) <- c("English", "Chinese")


q12 <- table(Lang, Q12_Betting_5)
q12.ctable <- rbind(q12[1,]/sum(q12[1,]), q12[2,]/sum(q12[2,]))
colnames(q12.ctable) <- c("Accept Bet", "Decline Bet")
rownames(q12.ctable) <- c("English", "Chinese")


q13 <- table(Lang, Q13_Betting_6)
q13.ctable <- rbind(q13[1,]/sum(q13[1,]), q13[2,]/sum(q13[2,]))
colnames(q13.ctable) <- c("Accept Bet", "Decline Bet")
rownames(q13.ctable) <- c("English", "Chinese")



small.wagers.english <- rbind(q8.ctable[1,1],q9.ctable[1,1],q10.ctable[1,1],q11.ctable[1,1], q12.ctable[1,1], q13.ctable[1,1])
small.wagers.chinese <- rbind(q8.ctable[2,1],q9.ctable[2,1],q10.ctable[2,1],q11.ctable[2,1], q12.ctable[2,1], q13.ctable[2,1])

lgratio <- c(.55,.4,.91,.75,.125,.25)

small.wagers <- as.data.frame(cbind(small.wagers.english, small.wagers.chinese, lgratio))
rownames(small.wagers) <- c("L8W14.5","L7W17.50","L10W11", "L9W12","L5W40","L6W24")
colnames(small.wagers) <- c("English", "MSM", "LGRatio")

small.wagers<-arrange(small.wagers, LGRatio)


#Make a graph similar to KHA for small wagers

small.plot <- ggplot(small.wagers, aes(x=LGRatio,y=English, colour="L2 English"))

small.plot+geom_point()+geom_line()+geom_line(data=small.wagers,aes(y=MSM, colour="L1 MSM"))+geom_point(data=small.wagers,aes(y=MSM, colour="L1 MSM"))+labs(x="Loss/Gain Ratio", y="Bet Acceptance %", title = "Figure 2.2a: Percentage of bilingual respondents \naccepting low-stakes bets")+theme(legend.title=element_blank())+scale_y_continuous(limits=c(0,1),breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+scale_x_continuous(limits=c(0,1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))


#####HIGH STAKES

q39 <- table(Lang, Q39_Betting_850)
q39.ctable <- rbind(q39[1,]/sum(q39[1,]), q39[2,]/sum(q39[2,]))
colnames(q39.ctable) <- c("Accept Bet", "Decline Bet")
rownames(q39.ctable) <- c("English", "Chinese")


q40 <- table(Lang, Q40_Betting_550)
q40.ctable <- rbind(q40[1,]/sum(q40[1,]), q40[2,]/sum(q40[2,]))
colnames(q40.ctable) <- c("Accept Bet", "Decline Bet")
rownames(q40.ctable) <- c("English", "Chinese")


q41 <- table(Lang, Q41_Betting_450)
q41.ctable <- rbind(q41[1,]/sum(q41[1,]), q41[2,]/sum(q41[2,]))
colnames(q41.ctable) <- c("Accept Bet", "Decline Bet")
rownames(q41.ctable) <- c("English", "Chinese")

q42 <- table(Lang, Q42_Betting_650)
q42.ctable <- rbind(q42[1,]/sum(q42[1,]), q42[2,]/sum(q42[2,]))
colnames(q42.ctable) <- c("Accept Bet", "Decline Bet")
rownames(q42.ctable) <- c("English", "Chinese")


q43 <- table(Lang, Q43_Betting_950)
q43.ctable <- rbind(q43[1,]/sum(q43[1,]), q43[2,]/sum(q43[2,]))
colnames(q43.ctable) <- c("Accept Bet", "Decline Bet")
rownames(q43.ctable) <- c("English", "Chinese")


q44 <- table(Lang, Q44_Betting_750)
q44.ctable <- rbind(q44[1,]/sum(q44[1,]), q44[2,]/sum(q44[2,]))
colnames(q44.ctable) <- c("Accept Bet", "Decline Bet")
rownames(q44.ctable) <- c("English", "Chinese")


large.wagers.english <- rbind(q39.ctable[1,1],q40.ctable[1,1],q41.ctable[1,1],q42.ctable[1,1], q43.ctable[1,1], q44.ctable[1,1])

large.wagers.chinese <- rbind(q39.ctable[2,1],q40.ctable[2,1],q41.ctable[2,1],q42.ctable[2,1], q43.ctable[2,1], q44.ctable[2,1])

lgratio <- c(.75,.25,.125,.4,.91,.56)

large.wagers <- as.data.frame(cbind(large.wagers.english, large.wagers.chinese, lgratio))
rownames(large.wagers) <- c("L850W1130","L550W2200","L450W3600", "L650W1625","L950W1045","L750W1340")
colnames(large.wagers) <- c("English", "MSM", "LGRatio")


large.wagers<-round(arrange(large.wagers, LGRatio),3)

#Make a graph similar to KHA for large wagers
large.plot <- ggplot(large.wagers, aes(x=LGRatio,y=English, colour="L2 English"))

large.plot+geom_point()+geom_line()+geom_line(data=large.wagers,aes(y=MSM, colour="L1 MSM"))+geom_point(data=large.wagers,aes(y=MSM, colour="L1 MSM"))+labs(x="Loss/Gain Ratio", y="Bet Acceptance %", title="Figure 2.2b: Percentage of bilingual respondents \naccepting high-stakes bets")+theme(legend.title=element_blank())+scale_y_continuous(limits=c(0,1),breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+scale_x_continuous(limits=c(0,1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))



###Now do all the tests


d.filtered <- filter(d.data, !is.na(d.data$Q8_Betting_8) & !is.na(d.data$Q9_Betting_7) &
                       !is.na(d.data$Q10_Betting_10) & !is.na(d.data$Q11_Betting_9) & !is.na(d.data$Q12_Betting_5)
                     & !is.na(d.data$Q13_Betting_6) & !is.na(d.data$Q39_Betting_850) &
                       !is.na(d.data$Q40_Betting_550) & !is.na(d.data$Q41_Betting_450) &
                       !is.na(d.data$Q42_Betting_650) & !is.na(d.data$Q43_Betting_950) &
                       !is.na(d.data$Q44_Betting_750))


#Keysar et al also did an F-test of how many bets each person took in toto
d.filtered$Q8_Betting_8[d.filtered$Q8_Betting_8 == 2] <- 0
d.filtered$Q9_Betting_7[d.filtered$Q9_Betting_7 == 2] <- 0
d.filtered$Q10_Betting_10[d.filtered$Q10_Betting_10 == 2] <- 0
d.filtered$Q11_Betting_9[d.filtered$Q11_Betting_9 == 2] <- 0
d.filtered$Q12_Betting_5[d.filtered$Q12_Betting_5 == 2] <- 0
d.filtered$Q13_Betting_6[d.filtered$Q13_Betting_6 == 2] <- 0

d.filtered$Q39_Betting_850[d.filtered$Q39_Betting_850 == 2] <- 0
d.filtered$Q40_Betting_550[d.filtered$Q40_Betting_550 == 2] <- 0
d.filtered$Q41_Betting_450[d.filtered$Q41_Betting_450 == 2] <- 0
d.filtered$Q42_Betting_650[d.filtered$Q42_Betting_650 == 2] <- 0
d.filtered$Q43_Betting_950[d.filtered$Q43_Betting_950 == 2] <- 0
d.filtered$Q44_Betting_750[d.filtered$Q44_Betting_750 == 2] <- 0

detach(d.data)
attach(d.filtered)
d.filtered$smallbetstotal <- Q8_Betting_8 + Q9_Betting_7 + Q10_Betting_10+Q11_Betting_9+Q12_Betting_5+Q13_Betting_6
d.filtered$bigbetstotal <- Q39_Betting_850 + Q40_Betting_550+Q41_Betting_450+Q42_Betting_650+Q43_Betting_950+Q44_Betting_750


nrow(d.filtered)
eng <- filter(d.filtered, Lang == 1)
chn <- filter(d.filtered, Lang == 2)

nrow(eng) #239
nrow(chn) #221


nrow(eng)*6
nrow(chn)*6

#check sum number of bets!
sum(chn$smallbetstotal)
sum(eng$smallbetstotal)


nrow(eng)*6 - sum(eng$smallbetstotal)
nrow(chn)*6 - sum(chn$smallbetstotal)

sum(chn$bigbetstotal)
sum(eng$bigbetstotal)

nrow(eng)*6 -eng$bigbetstotal
nrow(chn)*6 -chn$bigbetstotal

#now let's make a table

small.chi <- rbind(c(788, 538), c(867,567))
small.chi.prop <- rbind(small.chi[1,]/1326, small.chi[2,]/1434)
rownames(small.chi) <- c("MSM", "English")
chisq.test(small.chi)


big.chi <- rbind(c(sum(chn$bigbetstotal), nrow(chn)*6-sum(chn$bigbetstotal)), c(sum(eng$bigbetstotal), nrow(eng)*6 -sum(eng$bigbetstotal)))

big.chi
big.chi.prop <-  rbind(big.chi[1,]/1326, big.chi[2,]/1434)
chisq.test(big.chi)

#check for normality
#really not normal!
shapiro.test(eng$smallbetstotal)
shapiro.test(chn$smallbetstotal)

shapiro.test(eng$bigbetstotal)
shapiro.test(chn$bigbetstotal)


#all right, no F-tests
ks.test(eng$smallbetstotal, chn$smallbetstotal)
ks.test(eng$bigbetstotal, chn$bigbetstotal)


wilcox.test(eng$smallbetstotal, chn$smallbetstotal)
wilcox.test(eng$bigbetstotal, chn$bigbetstotal)
