gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/triLong.csv")
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


#----------------effect of tx, bar graph------------
data1 <- gi %>%
  filter(time %in% c("0423totri","0428totri", "0430totri", "0504totri", "0511totri","0525totri")) 
data2 <- gi %>%
  filter(time %in% c("0525totri","0528totri", "0530totri", "0603totri", "0610totri","0628totri")) 
head(data1)
head(data2)

df1 <- data.frame(data1 %>%
                    group_by(tx) %>%
                    get_summary_stats(totri, type = "mean_se"))
df1
df2<- data.frame(data2 %>%
                   group_by(tx) %>%
                   get_summary_stats(totri, type = "mean_se"))
df2

p1 <- ggplot(data=df1, aes(x=tx, y=mean, fill=tx)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  scale_y_continuous(name = "Total Triglycerides",limits = c(0,8)) +
  scale_x_discrete(name = "Treatment Groups", labels = c("SC", "SL", "WC", "WL"))+
  theme(legend.position = "none") +
  labs(caption="Figure  XX. Main effect of diet 1st LPS challenge") +
  theme(plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0))) +
  geom_text(label = c("a", "a", "b", "b"), aes(y =c(7,7,7,7), x = tx), size = 4) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position=position_dodge(0.9))

p2 <- ggplot(data=df2, aes(x=tx, y=mean, fill=tx)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  scale_y_continuous(name = "Total Triglycerides",limits = c(0,8)) +
  scale_x_discrete(name = "Treatment Groups", labels = c("SC", "SL", "WC", "WL"))+
  theme(legend.position = "none") +
  labs(caption="Figure  XX. Main effect of diet 2nd LPS challenge") +
  theme(plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0))) +
  geom_text(label = c("a", "a", "b", "b"), aes(y =c(7,7,7,7), x = tx), size = 4) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position=position_dodge(0.9))


pdf('totriDIETcombo.pdf')
nested <- (p1|p2) +
  plot_annotation(tag_levels = "A")
nested
dev.off()

#-----------------diet time interaction--------------

df3<- data.frame(data2 %>%
                   group_by(diet,time) %>%
                   get_summary_stats(totri, type = "mean_se"))
df3
pdf('totriDIETtime.pdf')
ggplot(data=df3, aes(x=time, y=mean, group = diet)) +
  geom_line(aes(color = diet))+
  geom_point(aes(color = diet))+
  scale_y_continuous(limits = c(0,8), name = "Total Triglycerides") +
  scale_x_discrete(labels = c("Pre-Injection", "24hr Post", "72hr Post", "1week", "2week", "4week"), name = "Time")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  annotate("text", x = 1, y = 7, label = "ns") +
  annotate("text", x = 2, y = 7, label = "ns") +
  annotate("text", x = 3, y = 7, label = "ns") +
  annotate("text", x = 4, y = 7, label = "*") +
  annotate("text", x = 5, y = 7, label = "***") +
  annotate("text", x = 6, y = 7, label = "**")  +
  labs(caption="Figure  XX. Unteraction effect between diet and time for second challenge") 
dev.off()
























