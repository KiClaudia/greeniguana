gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/AggLong.csv")
data <- gi %>%
  filter(time %in% c("0423agg","0428agg", "0430agg", "0504agg", "0511agg", "0525agg")) 

df <- data.frame(data %>%
                   group_by(tx,time) %>%
                   get_summary_stats(agg, type = "mean_se"))%>%
                   rename("Treatments" = tx)
View(df)
head(df)

levels(df$Treatments) <- list("SugarPBS" = "G-C",        
                               "SugarLPS" = "G-L",
                               "WaterPBS" = "W-C",
                               "WaterLPS" = "W-L")

p1 <- ggplot(data = df, aes(x = time, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments))+
  geom_line(aes(linetype = Treatments, color = Treatments))+
  scale_linetype_manual(values=c("solid", "solid","dashed", "dashed")) +
  scale_y_continuous(limits = c(0,6), name = "Agglutination Scores") +
  scale_x_discrete(labels = c("Pre-Injection", "24hrPost", "72hrPost", "1wkPost", "2wkPost", "4wkPost"), name = "Immune Challenge 1 Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values=c("#0033FF", "#FF33CC", "#33FFFF","#FF0000"))

data2 <- gi %>%
  filter(time %in% c("0525agg", "0528agg", "0530agg", "0603agg", "0610agg","0624agg")) 

df2 <- data.frame(data2 %>%
                   group_by(tx,time) %>%
                   get_summary_stats(agg, type = "mean_se"))%>%
  rename("Treatments" = tx)

p2 <- ggplot(data = df2, aes(x = time, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments))+
  geom_line(aes(linetype = Treatments, color = Treatments))+
  scale_linetype_manual(values=c("solid", "solid","dashed", "dashed")) +
  scale_y_continuous(limits = c(0,6), name = "Agglutination Scores") +
  scale_x_discrete(labels = c("Pre-Injection", "24hrPost", "72hrPost", "1wkPost", "2wkPost", "4wkPost"), name = "Immune Challenge 2 Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values=c("#0033FF", "#FF33CC", "#33FFFF","#FF0000"))


nested <- (p1/p2)+
  plot_annotation(tag_levels = 'A')
nested
library(patchwork)
