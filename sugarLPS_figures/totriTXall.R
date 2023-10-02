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

head(df)

p1 <- ggplot(data = df1, aes(x = time, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size = 2)+
  geom_line(aes(linetype = Treatments, color = Treatments), size = 1.3)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,7.5), name = "Total Triglycerides (unit)") +
  scale_x_discrete(labels = c("Pre-Injection", "24hrPost", "72hrPost", "1wkPost", "2wkPost", "4wkPost"), name = "Immune Challenge 1 Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values=c("navy", "navy", "steelblue1","steelblue1"))


p2 <- ggplot(data = df2, aes(x = time, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), seix = 2)+
  geom_line(aes(linetype = Treatments, color = Treatments), size = 1.3)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,7.5), name = "Total Triglycerides (unit)") +
  scale_x_discrete(labels = c("Pre-Injection", "24hrPost", "72hrPost", "1wkPost", "2wkPost", "4wkPost"), name = "Immune Challenge 2 Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values=c("navy", "navy", "steelblue1","steelblue1"))



nested <- (p1/p2)+
  plot_annotation(tag_levels = 'A')
nested
library(patchwork)
