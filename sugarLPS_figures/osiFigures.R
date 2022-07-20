
gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/OsiLong.csv")
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

#---------------------- Main effect of diet-----------------------
data1 <- gi %>%
  filter(time %in% c("0423oi","0428oi", "0430oi", "0504oi", "0511oi","0525oi")) 
data2 <- gi %>%
  filter(time %in% c("0525oi","0528oi", "0530oi", "0603oi", "0610oi","0624oi")) 
head(data1)
head(data2)

df1 <- data.frame(data1 %>%
                   group_by(tx) %>%
                   get_summary_stats(osi, type = "mean_se"))
df1
df2<- data.frame(data2 %>%
                    group_by(tx) %>%
                    get_summary_stats(osi, type = "mean_se"))
df2

p1 <- ggplot(data=df1, aes(x=tx, y=mean, fill=tx)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  scale_y_continuous(name = "Oxidative Stress Index",limits = c(-1.5,1.5)) +
  scale_x_discrete(name = "Treatment Groups", labels = c("SC", "SL", "WC", "WL"))+
  theme(legend.position = "none") +
  labs(caption="Figure  XX. Effect of diet on OSI during 1st LPS challenge") +
  theme(plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0))) +
  geom_text(label = c("a", "a", "b", "b"), aes(y =c(1.25,1.25,1.25,1.25), x = tx), size = 4) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position=position_dodge(0.9))

p2 <- ggplot(data=df2, aes(x=tx, y=mean, fill=tx)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  scale_y_continuous(name = "Oxidative Stress Index",limits = c(-1.5,1.5)) +
  scale_x_discrete(name = "Treatment Groups", labels = c("SC", "SL", "WC", "WL"))+
  theme(legend.position = "none") +
  labs(caption="Figure  XX. Effect of diet on OSI during 2nd LPS challenge") +
  theme(plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0))) +
  geom_text(label = c("a", "a", "b", "b"), aes(y =c(1.25,1.25,1.25,1.25), x = tx), size = 4) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position=position_dodge(0.9))

pdf('osiDIETcombo.pdf')
nested <- (p1|p2) +
  plot_annotation(tag_levels = "A")
nested
dev.off()

























