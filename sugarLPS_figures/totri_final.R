library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library('patchwork')

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/triLong.csv")

df1 <- data.frame(gi %>%
                    group_by(tx,time) %>%
                    get_summary_stats(totri, type = "mean_se"))%>%
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
  mutate(timenum = str_replace(time, "0423totri", "2")) %>%
  mutate(timenum = str_replace(timenum, "0428totri", "5")) %>%
  mutate(timenum = str_replace(timenum, "0430totri", "7")) %>%
  mutate(timenum = str_replace(timenum, "0504totri", "11")) %>%
  mutate(timenum = str_replace(timenum, "0511totri", "17")) %>%
  mutate(timenum = str_replace(timenum, "0525totri", "30")) %>%
  mutate(timenum = str_replace(timenum, "0528totri", "33")) %>%
  mutate(timenum = str_replace(timenum, "0530totri", "35")) %>%
  mutate(timenum = str_replace(timenum, "0603totri", "39")) %>%
  mutate(timenum = str_replace(timenum, "0610totri", "46")) %>%
  mutate(timenum = str_replace(timenum, "0624totri", "60")) 
df1 <- df1 %>% 
  mutate(positionName = str_replace(time, "0423totri", "Pre-Injection")) %>%
  mutate(positionName = str_replace(positionName, "0428totri", "24hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0430totri", "72hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0504totri", "1weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0511totri", "2weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0525totri", "4weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0528totri", "24hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0530totri", "72hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0603totri", "1weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0610totri", "2weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0624totri", "4weekPost")) 

df1
df1$timenum <- as.numeric(df1$timenum)




p1 <- ggplot(data = df1, aes(x = timenum, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size = 1.2)+
  geom_line(aes(linetype = Treatments, color = Treatments), size = .8)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,9), name = "Total Triglycerides (mg mL^-1)") +
  scale_x_continuous(breaks = df1$timenum,labels = df1$positionName, 
                     name = "Immune Challenge Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 30, size = 7, hjust = 1, vjust = 1)) +
  scale_color_manual(values=c("navy", "navy", "steelblue1","steelblue1")) +
  geom_vline(xintercept = 32, col = "red", size = 0.75) +
  geom_vline(xintercept = 4, col = "red", size = 0.75) +
  annotate("text", x = c(10,38), y=9, colour= "red", label = c("1st\nChallenge", "2nd\nChallenge"))

p1

pdf('Fig5_totri.pdf', width = 6, height = 5)
p1
dev.off()
