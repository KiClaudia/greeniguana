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

data1 <- gi %>%
  filter(time %in% c("0423totri","0428totri", "0430totri", "0504totri", "0511totri","0525totri")) 
data2 <- gi %>%
  filter(time %in% c("0525totri","0528totri", "0530totri", "0603totri", "0610totri","0628totri")) 
head(data1)
head(data2)

df1 <- data.frame(data1 %>%
                    group_by(tx,time) %>%
                    get_summary_stats(totri, type = "mean_se"))%>%
  rename("Treatments" = tx)
View(df1)
head(df1)
df1$Treatments <- as.factor(df1$Treatments)
levels(df1$Treatments) <- list("SugarPBS" = "G-C",        
                               "SugarLPS" = "G-L",
                               "ControlPBS" = "W-C",
                               "ControlLPS" = "W-L")

df2 <- data.frame(data2 %>%
                    group_by(tx,time) %>%
                    get_summary_stats(totri, type = "mean_se"))%>%
  rename("Treatments" = tx)
View(df2)
head(df2)
df2$Treatments <- as.factor(df2$Treatments)
levels(df2$Treatments) <- list("SugarPBS" = "G-C",        
                               "SugarLPS" = "G-L",
                               "ControlPBS" = "W-C",
                               "ControlLPS" = "W-L")

head(df2)
df1 <- df1 %>% 
  mutate(timenum = str_replace(time, "0423totri", "0")) %>%
  mutate(timenum = str_replace(timenum, "0428totri", "2")) %>%
  mutate(timenum = str_replace(timenum, "0430totri", "5")) %>%
  mutate(timenum = str_replace(timenum, "0504totri", "8")) %>%
  mutate(timenum = str_replace(timenum, "0511totri", "15")) %>%
  mutate(timenum = str_replace(timenum, "0525totri", "29")) 
df1 <- df1 %>% 
  mutate(positionName = str_replace(time, "0423totri", "Pre-Injection")) %>%
  mutate(positionName = str_replace(positionName, "0428totri", "24hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0430totri", "72hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0504totri", "1weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0511totri", "2weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0525totri", "4weekPost")) 

df1

df2 <- df2 %>% 
  mutate(timenum = str_replace(time, "0525totri", "0")) %>%
  mutate(timenum = str_replace(timenum, "0528totri", "2")) %>%
  mutate(timenum = str_replace(timenum, "0530totri", "5")) %>%
  mutate(timenum = str_replace(timenum, "0603totri", "8")) %>%
  mutate(timenum = str_replace(timenum, "0610totri", "15")) %>%
  mutate(timenum = str_replace(timenum, "0628totri", "29")) 

df2 <- df2 %>%
  mutate(positionName = str_replace(time, "0525totri", "Pre-Injection")) %>%
  mutate(positionName = str_replace(positionName, "0528totri", "24hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0530totri", "72hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0603totri", "1weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0610totri", "2weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0628totri", "4weekPost")) 
df2
df1$timenum <- as.numeric(df1$timenum)
df2$timenum <- as.numeric(df2$timenum)


p1 <- ggplot(data = df1, aes(x = timenum, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size = 2)+
  geom_line(aes(linetype = Treatments, color = Treatments), size = 1.3)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,8), name = "Total Triglycerides (mg/mL)") +
  scale_x_continuous(breaks = df1$timenum,labels = df1$positionName, 
                     name = "Immune Challenge 1 Timeline")+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  scale_color_manual(values=c("navy", "navy", "steelblue1","steelblue1"))


p2 <-  ggplot(data = df2, aes(x = timenum, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size = 2)+
  geom_line(aes(linetype = Treatments, color = Treatments), size = 1.3)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,8), name = "Total Triglycerides (mg/mL)") +
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
