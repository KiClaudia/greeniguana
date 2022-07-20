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
# ----------------main effect of LPS line graph-----------------------------
data1 <- gi %>%
  filter(time %in% c("0423agg","0428agg", "0430agg")) #"0504agg", "0511agg", "0525agg")) # going to use the first few days to simplify data
View(data1)
data2 <- gi %>%
  filter(time %in% c("0525agg", "0528agg", "0530agg")) #"0610agg","0624agg")) 
View(data2)

df1 <- data.frame(data1 %>%
                   group_by(lps, time) %>%
                   get_summary_stats(agg, type = "mean_se"))
df2 <- data.frame(data2 %>%
                    group_by(lps, time) %>%
                    get_summary_stats(agg, type = "mean_se"))
View(df1)
head(df1)
pdf('AggLPS1.pdf')
p1 <- ggplot(data=df1, aes(x=time, y=mean, group = lps)) +
  geom_line(aes(color = lps))+
  geom_point(aes(color = lps))+
  scale_y_continuous(limits = c(0,5), name = "Agglutination Score 1st LPS") +
  scale_x_discrete(labels = c("Pre-Injection", "24hr Post", "72hr Post"), name = "Time")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  annotate("text", x = 1, y = 3, label = "ns") +
  annotate("text", x = 2, y = 3, label = "ns") +
  annotate("text", x = 3, y = 3, label = "**") +
  labs(caption="Figure  XX. Main effect of LPS")
dev.off()

pdf('AggLPS2.pdf')
p2 <- ggplot(data=df2, aes(x=time, y=mean, group = lps)) +
  geom_line(aes(color = lps))+
  geom_point(aes(color = lps))+
  scale_y_continuous(limits = c(0,5), name = "Agglutination Score 2nd LPS") +
  scale_x_discrete(labels = c("Pre-Injection", "24hr Post", "72hr Post"), name = "Time")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  annotate("text", x = 1, y = 5, label = "***") +
  annotate("text", x = 2, y = 5, label = "***") +
  annotate("text", x = 3, y = 5, label = "***") 
dev.off()

pdf('AggLPScombo.pdf')
nested <- (p1|p2) +
  plot_annotation(tag_levels = "A")
nested
dev.off()
  


#----------------effect of tx, bar graph------------
data3 <- gi %>%
  filter(time %in% c("0423agg","0428agg", "0430agg","0504agg", "0511agg", "0525agg")) 
View(data3)

df <- data.frame(data3 %>%
                   group_by(tx) %>%
                   get_summary_stats(agg, type = "mean_se"))
df

pdf('AggTx.pdf')

ggplot(data=df, aes(x=tx, y=mean, fill=tx)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  scale_y_continuous(name = "Agglutination Score",limits = c(0,4)) +
  scale_x_discrete(name = "Treatment Groups", labels = c("SC", "SL", "WC", "WL"))+
  theme(legend.position = "none") +
  labs(caption="Figure  XX. Effect of diet and LPS on agglutination score") +
  theme(plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0))) +
  geom_text(label = c("a", "b", "a", "ab"), aes(y =c(03.5,03.5,03.5,03.5), x = tx), size = 4) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position=position_dodge(0.9))

dev.off()



























