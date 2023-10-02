gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/igyLong.csv")
View(gi)

data <- gi %>%
  filter(time %in% c("0423igy","0428igy", "0430igy", "0504igy", "0511igy", "0525igy")) 
head(data)

df <- data.frame(data %>%
                   group_by(tx,time) %>%
                   get_summary_stats(igy, type = "mean_se"))%>%
                   rename("Treatments" = tx)
head(df)
str(df)
df$Treatments <- as.factor(df$Treatments
)

levels(df$Treatments) <- list("SugarPBS" = "G-C",        
                              "SugarLPS" = "G-L",
                              "ControlPBS" = "W-C",
                              "ControlLPS" = "W-L")


head(df)

p1 <- ggplot(data = df, aes(x = time, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size =2 )+
  geom_line(aes(linetype = Treatments, color = Treatments), size = 1.3)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,0.2), name = "IgY Concentration") +
  scale_x_discrete(labels = c("Pre-Injection", "24hrPost", "72hrPost", "1wkPost", "2wkPost", "4wkPost"), name = "Immune Challenge 1 Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values=c("navy", "navy", "steelblue1","steelblue1"))


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


p2 <- ggplot(data = df2, aes(x = time, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size = 2)+
  geom_line(aes(linetype = Treatments, color = Treatments), size = 1.3)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,0.2), name = "IgY Concentration") +
  scale_x_discrete(labels = c("Pre-Injection", "24hrPost", "72hrPost", "1wkPost", "2wkPost", "4wkPost"), name = "Immune Challenge 2 Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values=c("navy", "navy", "steelblue1","steelblue1"))


p2
nested <- (p1/p2)+
  plot_annotation(tag_levels = 'A')
nested
library(patchwork)

pdf('igyline.pdf')
nested
dev.off()
