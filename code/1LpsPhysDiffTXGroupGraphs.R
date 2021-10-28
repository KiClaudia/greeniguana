# Graphically looking at basic phys difference before and after 1st LPS treatment
# Comparing to post glucose pre LPS 0423to see effects of Glucose on LPS challenge
gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(gi)
library("tidyverse")
library("dplyr")

# 03/24 is pre-study baseline, no treatment or diet
# 04/23 post glucose, pre-lps
# 04/28 24hr post LPS
# 04/30 72hr post LPS
# 05/04 1wk post LPS
# 05/11 2wk post LPS
# 05/18 3wk post LPS
# 05/25 4wk post LPS

glucose <- gi %>%
  filter(diet == "g") 
water <- gi %>%
  filter(diet == "w")
LPS <- gi %>%
  filter(lps == "l")
ctrl <- gi %>%
  filter(lps == "c")

GC <- gi %>%
  filter(tx == "G-C")
View(GC)
WC <- gi %>%
  filter(tx == "W-C")
View(WC)
GL <- gi %>%
  filter(tx == "G-L")
View(GL)
WL<- gi %>%
  filter(tx == "W-L")
View(WL)

#------------------------24Hr post LPS challenge-----------------------
pdf('24hrLps1BKA.pdf',
    width = 11, height = 7)
boxplot(GC$X0423bka, GC$X0428bka, WC$X0423bka, WC$X0428bka, GL$X0423bka, GL$X0428bka, WL$X0423bka, WL$X0428bka,
        xlab = "Treatment Groups", 
        ylab = "Bacterial percent killed", 
        main = "Effects of diet on BKA 24-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('24hrLps1Agg.pdf',
    width = 11, height = 7)
boxplot(GC$X0423agg, GC$X0428agg, WC$X0423agg, WC$X0428agg, GL$X0423agg, GL$X0428agg, WL$X0423agg, WL$X0428agg,
        xlab = "Treatment Groups", 
        ylab = "Agglutination Score", 
        main = "Effects of diet on agglutination 24-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('24hrLps1Lys.pdf',
    width = 11, height = 7)
boxplot(GC$X0423lys, GC$X0428lys, WC$X0423lys, WC$X0428lys, GL$X0423lys, GL$X0428lys, WL$X0423lys, WL$X0428lys,
        xlab = "Treatment Groups", 
        ylab = "Lysis Score", 
        main = "Effects of diet on lysis 24-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('24hrLps1OSI.pdf',
    width = 11, height = 7)
boxplot(GC$X0423oi, GC$X0428oi, WC$X0423oi, WC$X0428oi, GL$X0423oi, GL$X0428oi, WL$X0423oi, WL$X0428oi,
        xlab = "Treatment Groups", 
        ylab = "Oxidative Stress Index", 
        main = "Effects of diet on OSI 24-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('24hrLps1drom.pdf',
    width = 11, height = 7)
boxplot(GC$X0423drom, GC$X0428drom, WC$X0423drom, WC$X0428drom, GL$X0423drom, GL$X0428drom, WL$X0423drom, WL$X0428drom,
        xlab = "Treatment Groups", 
        ylab = "dROM mg H2O2/mL", 
        main = "Effects of diet on dROM 24-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('24hrLps1Oxy.pdf',
    width = 11, height = 7)
boxplot(GC$X0423oxy, GC$X0428oxy, WC$X0423oxy, WC$X0428oxy, GL$X0423oxy, GL$X0428oxy, WL$X0423oxy, WL$X0428oxy,
        xlab = "Treatment Groups", 
        ylab = "OXY mg HClO/mL", 
        main = "Effects of diet on Oxy 24-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('24hrLps1Mass.pdf',
    width = 11, height = 7)
boxplot(GC$X0423mass, GC$X0428mass, WC$X0423mass, WC$X0428mass, GL$X0423mass, GL$X0428mass, WL$X0423mass, WL$X0428mass,
        xlab = "Treatment Groups", 
        ylab = "Mass g", 
        main = "Effects of diet on mass 24-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('24hrLps1Gly.pdf',
    width = 11, height = 7)
boxplot(GC$X0423gly, GC$X0428gly, WC$X0423gly, WC$X0428gly, GL$X0423gly, GL$X0428gly, WL$X0423gly, WL$X0428gly,
        xlab = "Treatment Groups", 
        ylab = "Glycerol mg/mL", 
        main = "Effects of diet on glycerol 24-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('24hrLps1Totri.pdf',
    width = 11, height = 7)
boxplot(GC$X0423totri, GC$X0428totri, WC$X0423totri, WC$X0428totri, GL$X0423totri, GL$X0428totri, WL$X0423totri, WL$X0428totri,
        xlab = "Treatment Groups", 
        ylab = "Total Triglycerides mg/mL", 
        main = "Effects of diet on total triglycerides 24-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('24hrLps1Trtri.pdf',
    width = 11, height = 7)
boxplot(GC$X0423trtri, GC$X0428trtri, WC$X0423trtri, WC$X0428trtri, GL$X0423trtri, GL$X0428trtri, WL$X0423trtri, WL$X0428trtri,
        xlab = "Treatment Groups", 
        ylab = "True Triglycerides mg/mL", 
        main = "Effects of diet on true triglycerides 24-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

#------------------------72Hr post LPS challenge---------------------
pdf('72hrLps1BKA.pdf',
    width = 11, height = 7)
boxplot(GC$X0423bka, GC$X0430bka, WC$X0423bka, WC$X0430bka, GL$X0423bka, GL$X0430bka, WL$X0430bka, WL$X0430bka,
        xlab = "Treatment Groups", 
        ylab = "Bacterial percent killed", 
        main = "Effects of diet on BKA 72-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('72hrLps1Agg.pdf',
    width = 11, height = 7)
boxplot(GC$X0423agg, GC$X0430agg, WC$X0423agg, WC$X0430agg, GL$X0423agg, GL$X0430agg, WL$X0423agg, WL$X0430agg,
        xlab = "Treatment Groups", 
        ylab = "Agglutination Score", 
        main = "Effects of diet on agglutination 72-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('72hrLps1Lys.pdf',
    width = 11, height = 7)
boxplot(GC$X0423lys, GC$X0430lys, WC$X0423lys, WC$X0430lys, GL$X0423lys, GL$X0430lys, WL$X0423lys, WL$X0430lys,
        xlab = "Treatment Groups", 
        ylab = "Lysis Score", 
        main = "Effects of diet on lysis 72-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('72hrLps1OSI.pdf',
    width = 11, height = 7)
boxplot(GC$X0423oi, GC$X0430oi, WC$X0423oi, WC$X0430oi, GL$X0423oi, GL$X0430oi, WL$X0423oi, WL$X0430oi,
        xlab = "Treatment Groups", 
        ylab = "Oxidative Stress Index", 
        main = "Effects of diet on OSI 72-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('72hrLps1drom.pdf',
    width = 11, height = 7)
boxplot(GC$X0423drom, GC$X0430drom, WC$X0423drom, WC$X0430drom, GL$X0423drom, GL$X0430drom, WL$X0423drom, WL$X0430drom,
        xlab = "Treatment Groups", 
        ylab = "dROM mg H2O2/mL", 
        main = "Effects of diet on dROM 72-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('72hrLps1Oxy.pdf',
    width = 11, height = 7)
boxplot(GC$X0423oxy, GC$X0430oxy, WC$X0423oxy, WC$X0430oxy, GL$X0423oxy, GL$X0430oxy, WL$X0423oxy, WL$X0430oxy,
        xlab = "Treatment Groups", 
        ylab = "OXY mg HClO/mL", 
        main = "Effects of diet on Oxy 72-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('72hrLps1Mass.pdf',
    width = 11, height = 7)
boxplot(GC$X0423mass, GC$X0430mass, WC$X0423mass, WC$X0430mass, GL$X0423mass, GL$X0430mass, WL$X0423mass, WL$X0430mass,
        xlab = "Treatment Groups", 
        ylab = "Mass g", 
        main = "Effects of diet on mass 72-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('72hrLps1Gly.pdf',
    width = 11, height = 7)
boxplot(GC$X0423gly, GC$X0430gly, WC$X0423gly, WC$X0430gly, GL$X0423gly, GL$X0430gly, WL$X0423gly, WL$X0430gly,
        xlab = "Treatment Groups", 
        ylab = "Glycerol mg/mL", 
        main = "Effects of diet on glycerol 72-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('72hrLps1Totri.pdf',
    width = 11, height = 7)
boxplot(GC$X0423totri, GC$X0430totri, WC$X0423totri, WC$X0430totri, GL$X0423totri, GL$X0430totri, WL$X0423totri, WL$X0430totri,
        xlab = "Treatment Groups", 
        ylab = "Total Triglycerides mg/mL", 
        main = "Effects of diet on total triglycerides 72-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('72hrLps1Trtri.pdf',
    width = 11, height = 7)
boxplot(GC$X0423trtri, GC$X0430trtri, WC$X0423trtri, WC$X0430trtri, GL$X0423trtri, GL$X0430trtri, WL$X0423trtri, WL$X0430trtri,
        xlab = "Treatment Groups", 
        ylab = "True Triglycerides mg/mL", 
        main = "Effects of diet on true triglycerides 72-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()



#------------------------ 1Wk post LPS challenge------------------
pdf('1wkLps1BKA.pdf',
    width = 11, height = 7)
boxplot(GC$X0423bka, GC$X0504bka, WC$X0423bka, WC$X0504bka, GL$X0423bka, GL$X0504bka, WL$X0430bka, WL$X0504bka,
        xlab = "Treatment Groups", 
        ylab = "Bacterial percent killed", 
        main = "Effects of diet on BKA 1-week post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('1wkLps1Agg.pdf',
    width = 11, height = 7)
boxplot(GC$X0423agg, GC$X0504agg, WC$X0423agg, WC$X0504agg, GL$X0423agg, GL$X0504agg, WL$X0423agg, WL$X0504agg,
        xlab = "Treatment Groups", 
        ylab = "Agglutination Score", 
        main = "Effects of diet on agglutination 1 week post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('1wkLps1Lys.pdf',
    width = 11, height = 7)
boxplot(GC$X0423lys, GC$X0504lys, WC$X0423lys, WC$X0504lys, GL$X0423lys, GL$X0504lys, WL$X0423lys, WL$X0504lys,
        xlab = "Treatment Groups", 
        ylab = "Lysis Score", 
        main = "Effects of diet on lysis 1 week post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('1wkLps1OSI.pdf',
    width = 11, height = 7)
boxplot(GC$X0423oi, GC$X0504oi, WC$X0423oi, WC$X0504oi, GL$X0423oi, GL$X0504oi, WL$X0423oi, WL$X0504oi,
        xlab = "Treatment Groups", 
        ylab = "Oxidative Stress Index", 
        main = "Effects of diet on OSI 1 week post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

#------------------------ 2wk post LPS challenge-------------
pdf('2wkLps1BKA.pdf',
    width = 11, height = 7)
boxplot(GC$X0423bka, GC$X0511bka, WC$X0423bka, WC$X0511bka, GL$X0423bka, GL$X0511bka, WL$X0430bka, WL$X0511bka,
        xlab = "Treatment Groups", 
        ylab = "Bacterial percent killed", 
        main = "Effects of diet on BKA 2-week post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('2wkLps1Agg.pdf',
    width = 11, height = 7)
boxplot(GC$X0423agg, GC$X0511agg, WC$X0423agg, WC$X0511agg, GL$X0423agg, GL$X0511agg, WL$X0423agg, WL$X0511agg,
        xlab = "Treatment Groups", 
        ylab = "Agglutination Score", 
        main = "Effects of diet on agglutination 2 week post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('2wkLps1Lys.pdf',
    width = 11, height = 7)
boxplot(GC$X0423lys, GC$X0511lys, WC$X0423lys, WC$X0511lys, GL$X0423lys, GL$X0511lys, WL$X0423lys, WL$X0511lys,
        xlab = "Treatment Groups", 
        ylab = "Lysis Score", 
        main = "Effects of diet on lysis 2 week post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

pdf('2wkLps1OSI.pdf',
    width = 11, height = 7)
boxplot(GC$X0423oi, GC$X0511oi, WC$X0423oi, WC$X0511oi, GL$X0423oi, GL$X0511oi, WL$X0423oi, WL$X0511oi,
        xlab = "Treatment Groups", 
        ylab = "Oxidative Stress Index", 
        main = "Effects of diet on OSI 2 week post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()
