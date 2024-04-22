gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/igyLong.csv")
View(gi)

data <- gi %>%
  filter(time %in% c("0423igy","0428igy", "0430igy", "0504igy", "0511igy", "0525igy")) 
head(data)

df1 <- data.frame(data %>%
                   group_by(tx,time) %>%
                   get_summary_stats(igy, type = "mean_se"))%>%
                   rename("Treatments" = tx)
head(df1)
str(df1)
df1$Treatments <- as.factor(df1$Treatments
)

levels(df1$Treatments) <- list("SugarPBS" = "G-C",        
                              "SugarLPS" = "G-L",
                              "ControlPBS" = "W-C",
                              "ControlLPS" = "W-L")


head(df1)
data2 <- gi %>%
  filter(time %in% c("0525igy", "0528igy", "0530igy", "0603igy", "0610igy","0624igy")) 

df2 <- data.frame(data2 %>%
                    group_by(tx,time) %>%
                    get_summary_stats(igy, type = "mean_se"))%>%
  rename("Treatments" = tx)
head(df2)

df2$Treatments <- as.factor(df2$Treatments
)

levels(df2$Treatments) <- list("SugarPBS" = "G-C",        
                               "SugarLPS" = "G-L",
                               "ControlPBS" = "W-C",
                               "ControlLPS" = "W-L")

df1 <- df1 %>% 
  mutate(timenum = str_replace(time, "0423igy", "0")) %>%
  mutate(timenum = str_replace(timenum, "0428igy", "2")) %>%
  mutate(timenum = str_replace(timenum, "0430igy", "5")) %>%
  mutate(timenum = str_replace(timenum, "0504igy", "8")) %>%
  mutate(timenum = str_replace(timenum, "0511igy", "15")) %>%
  mutate(timenum = str_replace(timenum, "0525igy", "29")) 
df1 <- df1 %>% 
  mutate(positionName = str_replace(time, "0423igy", "Pre-Injection")) %>%
  mutate(positionName = str_replace(positionName, "0428igy", "24hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0430igy", "72hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0504igy", "1weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0511igy", "2weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0525igy", "4weekPost")) 

df1

df2 <- df2 %>% 
  mutate(timenum = str_replace(time, "0525igy", "0")) %>%
  mutate(timenum = str_replace(timenum, "0528igy", "2")) %>%
  mutate(timenum = str_replace(timenum, "0530igy", "5")) %>%
  mutate(timenum = str_replace(timenum, "0603igy", "8")) %>%
  mutate(timenum = str_replace(timenum, "0610igy", "15")) %>%
  mutate(timenum = str_replace(timenum, "0624igy", "29")) 

df2 <- df2 %>%
  mutate(positionName = str_replace(time, "0525igy", "Pre-Injection")) %>%
  mutate(positionName = str_replace(positionName, "0528igy", "24hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0530igy", "72hrPost")) %>%
  mutate(positionName = str_replace(positionName, "0603igy", "1weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0610igy", "2weekPost")) %>%
  mutate(positionName = str_replace(positionName, "0624igy", "4weekPost")) 
df2
df1$timenum <- as.numeric(df1$timenum)
df2$timenum <- as.numeric(df2$timenum)


p1 <- ggplot(data = df1, aes(x = timenum, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size =2 )+
  geom_line(aes(linetype = Treatments, color = Treatments), size = 1.3)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,0.2), name = "IgY Concentration (mg mL^-1)") +
  scale_x_continuous(breaks = df1$timenum,labels = df1$positionName, 
                     name = "Immune Challenge 1 Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  scale_color_manual(values=c("navy", "navy", "steelblue1","steelblue1"))


p2 <- ggplot(data = df2, aes(x = timenum, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size = 2)+
  geom_line(aes(linetype = Treatments, color = Treatments), size = 1.3)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,0.2), name = "IgY Concentration (mg mL^-1)") +
  scale_x_continuous(breaks = df2$timenum,labels = df2$positionName, 
                     name = "Immune Challenge 2 Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  scale_color_manual(values=c("navy", "navy", "steelblue1","steelblue1"))


p2
nested <- (p1/p2)+
  plot_annotation(tag_levels = 'A')
nested
library(patchwork)

pdf('Fig4.pdf', width = 5, height = 6)
nested
dev.off()
