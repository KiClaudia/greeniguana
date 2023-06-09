gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/AggLong.csv")
library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library('patchwork')
gi$time <- as.factor(gi$time)
gi$iguanaID <- as.factor(gi$iguanaID)
gi$tx <- as.factor(gi$tx)
gi$lps <- as.factor(gi$lps)
gi$diet <- as.factor(gi$diet)
str(gi)

data <- gi %>%
 filter(!time %in% c("0324agg", "0628agg"))
# ----------------main effect of LPS line graph-----------------------------
df <- data.frame(data %>%
                   group_by(lps, time) %>%
                   get_summary_stats(agg, type = "mean_se"))
View(df)

pdf('AggLPS1.pdf')
ggplot(data=df, aes(x=time, y=mean, group = lps)) +
  geom_line(aes(color = lps))+
  geom_point(aes(color = lps))+
  scale_y_continuous(limits = c(0,5), name = "Agglutination Score") +
  scale_x_discrete(labels = c("Pre-Injection1", "24hr", "72hr", "1wk", "2wk", "4wk/Pre-Injection2", "24hr", "72hr", "1wk", "2wk", "4wk"), name = "Time")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  labs(caption="Figure  XX. Main effect of LPS") +
  geom_vline(aes(xintercept=6),   
             color="red", linetype="dashed", size=0.4) +
  annotate("text", x = 1, y = 5, label = "ns") +
  annotate("text", x = 2, y = 5, label = "ns") +
  annotate("text", x = 3, y = 5, label = "**") +
  annotate("text", x = 4, y = 5, label = "**") +
  annotate("text", x = 5, y = 5, label = "*") +
  annotate("text", x = 6, y = 5, label = "p = 0.07") +
  annotate("text", x = 7, y = 5, label = "**") +
  annotate("text", x = 8, y = 5, label = "***") +
  annotate("text", x = 9, y = 5, label = "*") +
  annotate("text", x = 10, y = 5, label = "*") +
  annotate("text", x = 11, y = 5, label = "*") 
  dev.off()




