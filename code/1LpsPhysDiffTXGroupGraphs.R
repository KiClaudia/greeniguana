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

pdf('24hrLps1BKA.pdf',
    width = 11, height = 7)
boxplot(GC$X0423bka, GC$X0428bka, WC$X0423bka, WC$X0428bka, GL$X0423bka, GL$X0428bka, WL$X0423bka, WL$X0428bka,
        xlab = "Treatment Groups", 
        ylab = "Bacterial percent killed", 
        main = "Effects of diet on BKA 24-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()

as.numeric(WC$X0428agg)
WC$X0428agg <- na.omit(WC$X0428agg)

pdf('24hrLps1Agg.pdf',
    width = 11, height = 7)
boxplot(GC$X0423agg, GC$X0428agg, WC$X0423agg, WC$X042agg, GL$X0423agg, GL$X0428agg, WL$X0423agg, WL$X0428agg,
        xlab = "Treatment Groups", 
        ylab = "Agglutination Score", 
        main = "Effects of diet on agglutination 24-hour post LPS challenge",
        names = c("G pre-Ctrl", "G post-Ctrl", "W pre-Ctrl", "W post-Ctrl", "G pre-LPS", "G post-LPS", "W pre-LPS", "W post-LPS)"))
dev.off()
