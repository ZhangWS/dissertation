######################
#PART THREE: ULTIMATUM GAME


#build a frequency table. There are 349 respondents overall who did numerical answers
q27.response.bylang <- table(Lang, Q27_filled_in)
colnames(q27.response.bylang) <- c("No Response", "Numerical", "Tickmarks")
rownames(q27.response.bylang) <- c("English", "Chinese")
q27.response.bylang

#and now the relative frequency table with labels
q27.response.bylang.freq <- rbind(q27.response.bylang[1,]/sum(q27.response.bylang[1,]), q27.response.bylang[2,]/sum(q27.response.bylang[2,]))
rownames(q27.response.bylang.freq) <- c("English", "Chinese")
q27.response.bylang.freq


d.filtered2 <- filter(d.data, !is.na(Q27AA_Ultimatum_60_give) & !is.na(Q27BA_Ultimatum_5_give) 
                      & !is.na(Q27CA_Ultimatum_475_give) & Q27_filled_in == 1)

nrow(d.filtered2)
eng.filled <- filter(d.filtered2, Lang ==1)
chn.filled <- filter(d.filtered2, Lang ==2)

detach(d.data)
detach(d.filtered)
attach(d.filtered2)


y5.num <- rbind(cbind(mean(eng.filled$Q27BA_Ultimatum_5_give), 
                      sd(eng.filled$Q27BA_Ultimatum_5_give), 
                      mean(eng.filled$Q27BB_Ultimatum_5_keep), 
                      sd(eng.filled$Q27BB_Ultimatum_5_keep)), 
                cbind(mean(chn.filled$Q27BA_Ultimatum_5_give), 
                      sd(chn.filled$Q27BA_Ultimatum_5_give), 
                      mean(chn.filled$Q27BB_Ultimatum_5_keep), 
                      sd(chn.filled$Q27BB_Ultimatum_5_keep)))

colnames(y5.num) <- c("Give", "Give SD", "Keep", "Keep Sd")
rownames(y5.num) <- c("English", "Chinese")
y5.num



y60.num <- rbind(cbind(mean(eng.filled$Q27AA_Ultimatum_60_give), 
                       sd(eng.filled$Q27AA_Ultimatum_60_give), 
                       mean(eng.filled$Q27AB_Ultimatum_60_keep), 
                       sd(eng.filled$Q27AB_Ultimatum_60_keep)), 
                 cbind(mean(chn.filled$Q27AA_Ultimatum_60_give), 
                       sd(chn.filled$Q27AA_Ultimatum_60_give), 
                       mean(chn.filled$Q27AB_Ultimatum_60_keep), 
                       sd(chn.filled$Q27AB_Ultimatum_60_keep)))

colnames(y60.num) <- c("Give", "Give SD", "Keep", "Keep SD")
rownames(y60.num) <- c("English", "Chinese")
y60.num



y475.num <- rbind(cbind(mean(eng.filled$Q27CA_Ultimatum_475_give), 
                        sd(eng.filled$Q27CA_Ultimatum_475_give), 
                        mean(eng.filled$Q27CB_Ultimatum_475_keep), 
                        sd(eng.filled$Q27CB_Ultimatum_475_keep)), 
                  cbind(mean(chn.filled$Q27CA_Ultimatum_475_give), 
                        sd(chn.filled$Q27CA_Ultimatum_475_give), 
                        mean(chn.filled$Q27CB_Ultimatum_475_keep), 
                        sd(chn.filled$Q27CB_Ultimatum_475_keep)))

colnames(y475.num) <- c("Give", "Give SD", "Keep", "Keep SD")
rownames(y475.num) <- c("English", "Chinese")
y475.num



###HISTOGRAMS


#Q27, how to split 5 yuan?

d.filtered2$Lang[d.filtered2$Lang==2] <-0

y5.plot <- ggplot(d.filtered2, aes(Q27BA_Ultimatum_5_give, fill=factor(Lang))) +
  geom_histogram(aes(y=0.5*..density..),position="dodge", binwidth=0.5) + labs(x="Amount Offered", y="Relative Freq",title="Figure 2.3a: Relative frequency of offers in the ¥5 condition") +
  scale_x_discrete(limits=c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)) + scale_y_continuous(limit=c(0,0.4)) + scale_fill_discrete(name="Language", labels=c("MSM", "English"))

y5.plot


#27, how to split 60 yuan?
y60.plot <- ggplot(d.filtered2, aes(Q27AA_Ultimatum_60_give, fill=factor(Lang))) +
  geom_histogram(aes(y=5*..density..), position="dodge", binwidth=5) + labs(x="Amount Offered", y="Relative Freq",title="Figure 2.3b: Relative frequency of offers in the ¥60 condition") +
  scale_y_continuous(limits=c(0,0.8)) + scale_x_discrete(limits=c(0,10,20,30,40,50,60)) + 
  scale_fill_discrete(name="Language", labels=c("MSM", "English"))

y60.plot

#27, how to split 475 yuan?

y475.plot <- ggplot(d.filtered2, aes(Q27CA_Ultimatum_475_give, fill=factor(Lang))) +
  geom_histogram(aes(y=10*..density..), position="dodge", binwidth=10) + labs(x="Amount Offered", y="Relative Freq",title="Figure 2.3c: Relative frequency of offers in the ¥475 condition") +
  scale_y_continuous(limits=c(0,0.5)) + scale_x_discrete(limits=c(0,50,100,150,200,250,300,350,400)) + 
  scale_fill_discrete(name="Language", labels=c("MSM", "English"))

y475.plot


y5.freq <- table(Q27BA_Ultimatum_5_give,Lang)
y5.prop <- cbind(y5.freq[,2]/178,y5.freq[,1]/169)
colnames(y5.prop) <- c("MSM","English")
y5.prop

y60.freq <- table(Q27AA_Ultimatum_60_give,Lang)
y60.prop <- cbind(y60.freq[,2]/178,y60.freq[,1]/169)
colnames(y60.prop) <- c("MSM","English")
y60.prop


y475.freq <- table(Q27CA_Ultimatum_475_give,Lang)
y475.prop <- cbind(y475.freq[,2]/178,y475.freq[,1]/169)
colnames(y475.prop) <- c("MSM","English")
y475.prop


shapiro.test(eng.filled$Q27BA_Ultimatum_5_give)
shapiro.test(chn.filled$Q27BA_Ultimatum_5_give)

shapiro.test(eng.filled$Q27AA_Ultimatum_60_give)
shapiro.test(chn.filled$Q27AA_Ultimatum_60_give)

shapiro.test(eng.filled$Q27CA_Ultimatum_475_give)
shapiro.test(chn.filled$Q27CA_Ultimatum_475_give)

#assuming unequal variance
t.test(eng.filled$Q27BA_Ultimatum_5_give, chn.filled$Q27BA_Ultimatum_5_give)
t.test(eng.filled$Q27AA_Ultimatum_60_give,chn.filled$Q27AA_Ultimatum_60_give)
t.test(eng.filled$Q27CA_Ultimatum_475_give, chn.filled$Q27CA_Ultimatum_475_give)


t.test(eng.filled$Q27BA_Ultimatum_5_give, chn.filled$Q27BA_Ultimatum_5_give, var.equal=T)
t.test(eng.filled$Q27AA_Ultimatum_60_give,chn.filled$Q27AA_Ultimatum_60_give, var.equal=T)
t.test(eng.filled$Q27CA_Ultimatum_475_give, chn.filled$Q27CA_Ultimatum_475_give, var.equal=T)


ks.test(eng.filled$Q27BA_Ultimatum_5_give, chn.filled$Q27BA_Ultimatum_5_give)
ks.test(eng.filled$Q27AA_Ultimatum_60_give,chn.filled$Q27AA_Ultimatum_60_give)
ks.test(eng.filled$Q27CA_Ultimatum_475_give, chn.filled$Q27CA_Ultimatum_475_give)

wilcox.test(d.filtered2$Q27BA_Ultimatum_5_give~Lang)
wilcox.test(d.filtered2$Q27AA_Ultimatum_60_give~Lang)
wilcox.test(d.filtered2$Q27CA_Ultimatum_475_give~Lang)

median(eng.filled$Q27BA_Ultimatum_5_give, na.rm=T)
median(chn.filled$Q27BA_Ultimatum_5_give, na.rm=T)

median(eng.filled$Q27AA_Ultimatum_60_give, na.rm=T)
median(chn.filled$Q27AA_Ultimatum_60_give, na.rm=T)

median(eng.filled$Q27CA_Ultimatum_475_give, na.rm=T)
median(chn.filled$Q27CA_Ultimatum_475_give, na.rm=T)


##As Player 2, the Receiver
###Splitting 20 Yuan

detach(d.data)
detach(d.filtered)
detach(d.filtered2)
detach(d.v1)

d.filtered3 <- filter(d.data, !is.na(Q51B_Ultimatum_large) & !is.na(Q51A_Ultimatum_small))
attach(d.filtered3)

nrow(d.filtered3)


q51a.response.bylang <- table(Lang, Q51A_Ultimatum_small)
colnames(q51a.response.bylang) <- c("Accept", "Reject")
rownames(q51a.response.bylang) <- c("English", "Chinese")
q51a.response.bylang


q51b.response.bylang <- table(Lang, Q51B_Ultimatum_large)
colnames(q51b.response.bylang) <- c("Accept", "Reject")
rownames(q51b.response.bylang) <- c("English", "Chinese")
q51b.response.bylang

chisq.test(q51a.response.bylang)
chisq.test(q51b.response.bylang)