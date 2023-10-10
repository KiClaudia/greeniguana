library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library('patchwork')

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/AggLong.csv")
data <- gi %>%
  filter(time %in% c("0423agg","0428agg", "0430agg", "0504agg", "0511agg", "0525agg")) 

df1 <- data.frame(data %>%
                   group_by(tx,time) %>%
                   get_summary_stats(agg, type = "mean_se"))%>%
                   rename("Treatments" = tx)
View(df1)
head(df1)
str(df1)
df1$Treatments <- as.factor(df1$Treatments
                           )

levels(df1$Treatments) <- list("SugarPBS" = "G-C",        
                               "SugarLPS" = "G-L",
                               "ControlPBS" = "W-C",
                               "ControlLPS" = "W-L")


df1 <- df1 %>% 
  mutate(timenum = str_replace(time, "0423agg", "0")) %>%
  mutate(timenum = str_replace(timenum, "0428agg", "2")) %>%
  mutate(timenum = str_replace(timenum, "0430agg", "5")) %>%
  mutate(timenum = str_replace(timenum, "0504agg", "8")) %>%
  mutate(timenum = str_replace(timenum, "0511agg", "15")) %>%
  mutate(timenum = str_replace(timenum, "0525agg", "29")) 
df1 <- df1 %>% 
  mutate(positionName = str_replace(time, "0423agg", "Pre-Injection")) %>%
  mutate(positionName = str_replace(positionName, "0428agg", "24hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0430agg", "72hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0504agg", "1weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0511agg", "2weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0525agg", "4weekPost")) 

df1
df1$timenum <- as.numeric(df1$timenum)




p1 <- ggplot(data = df1, aes(x = timenum, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size = 2)+
  geom_line(aes(linetype = Treatments, color = Treatments), size = 1.3)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,8), name = "Agglutination Scores") +
  scale_x_continuous(breaks = df1$timenum,labels = df1$positionName, 
                     name = "Immune Challenge 1 Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  scale_color_manual(values=c("navy", "navy", "steelblue1","steelblue1"))

data2 <- gi %>%
  filter(time %in% c("0525agg", "0528agg", "0530agg", "0603agg", "0610agg","0624agg")) 

df2 <- data.frame(data2 %>%
                   group_by(tx,time) %>%
                   get_summary_stats(agg, type = "mean_se"))%>%
  rename("Treatments" = tx)
df2$Treatments <- as.factor(df2$Treatments)
levels(df2$Treatments) <- list("SugarPBS" = "G-C",        
                               "SugarLPS" = "G-L",
                               "ControlPBS" = "W-C",
                               "ControlLPS" = "W-L")
df2 <- df2 %>% 
  mutate(timenum = str_replace(time, "0525agg", "0")) %>%
  mutate(timenum = str_replace(timenum, "0528agg", "2")) %>%
  mutate(timenum = str_replace(timenum, "0530agg", "5")) %>%
  mutate(timenum = str_replace(timenum, "0603agg", "8")) %>%
  mutate(timenum = str_replace(timenum, "0610agg", "15")) %>%
  mutate(timenum = str_replace(timenum, "0624agg", "29")) 

df2 <- df2 %>%
  mutate(positionName = str_replace(time, "0525agg", "Pre-Injection")) %>%
  mutate(positionName = str_replace(positionName, "0528agg", "24hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0530agg", "72hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0603agg", "1weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0610agg", "2weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0624agg", "4weekPost")) 
df2
df2$timenum <- as.numeric(df2$timenum)


p2 <- ggplot(data = df2, aes(x = timenum, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size =2 )+
  geom_line(aes(linetype = Treatments, color = Treatments), size = 1.3)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,8), name = "Agglutination Scores") +
  scale_x_continuous(breaks = df2$timenum,labels = df2$positionName, 
                     name = "Immune Challenge 2 Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  scale_color_manual(values=c("navy", "navy", "steelblue1","steelblue1"))


nested <- (p1/p2)+
  plot_annotation(tag_levels = 'A')
nested
library(patchwork)


pdf('aggline.pdf')
nested
dev.off()
