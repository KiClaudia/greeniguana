gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/LysLong.csv")
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
  filter(time %in% c("0423lys","0428lys", "0430lys"))  
View(data1)
data2 <- gi %>%
  filter(time %in% c("0525lys", "0528lys", "0530lys"))
View(data2)

df1 <- data.frame(data1 %>%
                    group_by(lps, time) %>%
                    get_summary_stats(lys, type = "mean_se"))
df2 <- data.frame(data2 %>%
                    group_by(lps, time) %>%
                    get_summary_stats(lys, type = "mean_se"))
View(df1)
head(df1)
pdf('LysLPS2.pdf') #only second challenge was significant
  
p1 <- ggplot(data=df1, aes(x=time, y=mean, group = lps)) +
  geom_line(aes(color = lps))+
  geom_point(aes(color = lps))+
  scale_y_continuous(limits = c(0,5), name = "Lysis Score 1st LPS") +
  scale_x_discrete(labels = c("Pre-Injection", "24hr Post", "72hr Post"), name = "Time")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  annotate("text", x = 1, y = 4, label = "ns") +
  annotate("text", x = 2, y = 4, label = "ns") +
  annotate("text", x = 3, y = 4, label = "ns") +
  labs(caption="Figure  XX. Main effect of 1st LPS challenge on Lysis")
p2 <- ggplot(data=df2, aes(x=time, y=mean, group = lps)) +
  geom_line(aes(color = lps))+
  geom_point(aes(color = lps))+
  scale_y_continuous(limits = c(0,5), name = "Lysis Score 2nd LPS") +
  scale_x_discrete(labels = c("Pre-Injection", "24hr Post", "72hr Post"), name = "Time")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  annotate("text", x = 1, y = 4, label = "**") +
  annotate("text", x = 2, y = 4, label = "**") +
  annotate("text", x = 3, y = 4, label = "**") +
  labs(caption="Figure  XX. Main effect of 2nd LPS challenge on Lysis")
dev.off()


pdf('LysLPScombo.pdf')
nested <- (p1|p2) +
  plot_annotation(tag_levels = "A")
nested
dev.off()

#----------------main effect of time line graph------------
data3 <- gi %>%
  filter(time %in% c("0525lys","0528lys", "0530lys", "0603lys", "0610lys","0624lys")) 
View(data3)

df <- data.frame(data3 %>%
                   group_by(time) %>%
                   get_summary_stats(lys, type = "mean_se"))
df

pdf('Lystime.pdf')

ggplot(data=df, aes(x=time, y=mean, group=1)) +
  geom_line()+
  scale_y_continuous(limits = c(0,3.5), name = "Lysis Score 2nd LPS") +
  scale_x_discrete(labels = c("Pre-Injection", "24hr Post", "72hr Post", "1wk Post", "2wk Post", "4wk Post"), name = "Time")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  annotate("text", x = 1, y = 3, label = "a") +
  annotate("text", x = 2, y = 3, label = "bc") +
  annotate("text", x = 3, y = 3, label = "c") +  
  annotate("text", x = 4, y = 3, label = "a") +
  annotate("text", x = 5, y = 3, label = "ac") +
  annotate("text", x = 6, y = 3, label = "abc") +
  labs(caption="Figure  XX. Main effect of time on Lysis")

dev.off()



























