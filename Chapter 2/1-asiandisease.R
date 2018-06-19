#########################################################
#EXP 1: Q1 ASIAN DISEASE
#########################################################

table(gain$Q1Form, gain$Lang)
table(loss$Q1Form, loss$Lang)


###GAIN FRAME
f1.table <- rbind(c(62, 53, 115), c(60, 64, 124))

#Add labels
rownames(f1.table) <- c("MSM (L1)", "English (L2)")
colnames(f1.table) <- c("Medicine A", "Medicine B", "Total N")


#make a relative freq table
f1.ctable <- rbind(f1.table[1,1:2]/f1.table[1,3], f1.table[2,1:2]/f1.table[2,3])
rownames(f1.ctable) <- c("MSM (L1)", "English (L2)")
colnames(f1.table) <- c("Medicine A", "Medicine B", "Total N")

f1.temp <- melt(f1.ctable)
colnames(f1.temp) <- c("Language", "Medicine", "RelFrequency")

f1.cplot <- ggplot(f1.temp, aes(x=Language, y=RelFrequency))+ 
  geom_bar(position="dodge", stat="identity", aes(fill=Medicine)) + 
  ylim(0,1) + labs(y="Proportional Freq", title="Figure 2.1a: Relative frequency of responses, gain frame")

f1.cplot 

f1.table
f1.ctable


#LOSS FRAME
f2.table <- rbind(c(50, 64, 114), c(65, 57, 122))

#Add labels
rownames(f2.table) <- c("MSM (L1)", "English (L2)")
colnames(f2.table) <- c("Medicine A", "Medicine B", "Total N")


#make a relative freq table
f2.ctable <- rbind(f2.table[1,1:2]/f2.table[1,3], f2.table[2,1:2]/f2.table[2,3])
rownames(f2.ctable) <- c("MSM (L1)", "English (L2)")
colnames(f2.table) <- c("Medicine A", "Medicine B", "Total N")

#melt, recast into Hadley's favoritest form, then graph!

f2.temp <- melt(f2.ctable)
colnames(f2.temp) <- c("Language", "Medicine", "RelFrequency")

f2.cplot <- ggplot(f2.temp, aes(x=Language, y=RelFrequency)) + 
  geom_bar(position="dodge", stat="identity", aes(fill=Medicine)) + 
  ylim(0, 1) + labs(y="Proportional Freq", title="Figure 2.1b: Relative frequency of responses, loss frame")

f2.cplot 
f2.table
f2.ctable



#Now compare Chi-square
#MSM 
msm.matrix <- rbind(f1.table[1,1:2], f2.table[1, 1:2])
chisq.test(msm.matrix)
chisq.test(msm.matrix)$statistic

#English
eng.matrix <- rbind(f1.table[2,1:2], f2.table[2, 1:2])
chisq.test(eng.matrix)
chisq.test(eng.matrix)$statistic