library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library('patchwork')

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/AggLong.csv")

df1 <- data.frame(gi %>%
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
  mutate(timenum = str_replace(time, "0423agg", "2")) %>%
  mutate(timenum = str_replace(timenum, "0428agg", "5")) %>%
  mutate(timenum = str_replace(timenum, "0430agg", "7")) %>%
  mutate(timenum = str_replace(timenum, "0504agg", "11")) %>%
  mutate(timenum = str_replace(timenum, "0511agg", "17")) %>%
  mutate(timenum = str_replace(timenum, "0525agg", "30")) %>%
  mutate(timenum = str_replace(timenum, "0528agg", "33")) %>%
  mutate(timenum = str_replace(timenum, "0530agg", "35")) %>%
  mutate(timenum = str_replace(timenum, "0603agg", "39")) %>%
  mutate(timenum = str_replace(timenum, "0610agg", "46")) %>%
  mutate(timenum = str_replace(timenum, "0624agg", "60")) 
df1 <- df1 %>% 
  mutate(positionName = str_replace(time, "0423agg", "Pre-Injection")) %>%
  mutate(positionName = str_replace(positionName, "0428agg", "24hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0430agg", "72hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0504agg", "1weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0511agg", "2weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0525agg", "4weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0528agg", "24hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0530agg", "72hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0603agg", "1weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0610agg", "2weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0624agg", "4weekPost")) 

df1
df1$timenum <- as.numeric(df1$timenum)




p1 <- ggplot(data = df1, aes(x = timenum, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size = 1.2)+
  geom_line(aes(linetype = Treatments, color = Treatments), size = .8)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,8), name = "Agglutination Scores") +
  scale_x_continuous(breaks = df1$timenum,labels = df1$positionName, 
                     name = "Immune Challenge Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 30, size = 7, hjust = 1, vjust = 1)) +
  scale_color_manual(values=c("navy", "navy", "steelblue1","steelblue1")) +
  geom_vline(xintercept = 32, col = "red", size = 0.75) +
  geom_vline(xintercept = 4, col = "red", size = 0.75) +
  annotate("text", x = c(10,38), y=8, colour= "red", label = c("1st\nChallenge", "2nd\nChallenge"))


p1


pdf('Fig3_agg.pdf', width = 6, height = 5)
p1
dev.off()
