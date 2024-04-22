gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/BKA2LPSlong.csv")
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
# ----------------main effect of diet line graph-----------------------------

df2 <- data.frame(gi %>%
                    group_by(tx, time) %>%
                    get_summary_stats(bka, type = "mean_se")) %>%
  rename("Treatments" = tx)
View(df2)
head(df2)

levels(df2$Treatments) <- list("SugarPBS" = "G-C",        
                               "SugarLPS" = "G-L",
                               "ControlPBS" = "W-C",
                               "ControlLPS" = "W-L")
# make new column that will describe the position in time of our points (slightly off to make it easier to read labels)
df2 <- df2 %>% 
  mutate(timenum = str_replace(time, "0525bka", "0")) %>%
  mutate(timenum = str_replace(timenum, "0528bka", "2")) %>%
  mutate(timenum = str_replace(timenum, "0530bka", "5")) %>%
  mutate(timenum = str_replace(timenum, "0603bka", "8")) %>%
  mutate(timenum = str_replace(timenum, "0610bka", "15")) %>%
  mutate(timenum = str_replace(timenum, "0624bka", "29")) 

# tell R that it is a number so it can work on a continuous scale
View(df2)
str(df2$timenum)
df2$timenum <- as.numeric(df2$timenum)

# make another column that will tell R eventually what you want each point to be called
df2 <- df2 %>%
  mutate(positionName = str_replace(time, "0525bka", "Pre-Injection")) %>%
  mutate(positionName = str_replace(positionName, "0528bka", "24hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0530bka", "72hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0603bka", "1weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0610bka", "2weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0624bka", "4weekPost")) 


pdf('Fig2_bka.pdf', width = 5, height = 6)
ggplot(data = df2, aes(x = timenum, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size = 1.2)+
  geom_line(aes(linetype = Treatments, color = Treatments), size = .8)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,100), name = "Percent of Bacteria Killed") +
  scale_x_continuous(breaks = df2$timenum,labels = df2$positionName, limits = c(0,30), 
                     name = "Immune Challenge 2 Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 30,size = 7, hjust = 1, vjust = 1)) +
  scale_color_manual(values=c("navy", "navy", "steelblue1","steelblue1"))+
  annotate("text", x = c(5), y=90, colour= "red", label = c("2nd\nChallenge"))+
  geom_vline(xintercept = 1, col = "red", size = 0.75) 
  

# now we can make a line plot in ggplot where we tell it that the breaks in x axis is based on our numbers above and to 
# call is positionName. We also rotate and the labels to make it legible


dev.off()





















