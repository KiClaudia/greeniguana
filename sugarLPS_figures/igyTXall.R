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

library(data.table)
setDT(df)

df[Treatments == "G-C", Treatments := "SugarPBS"]
df[Treatments == "G-L", Treatments := "SugarLPS"]
df[Treatments == "W-C", Treatments := "WaterPBS"]
df[Treatments == "W-L", Treatments := "WaterLPS"]

head(df)

p1 <- ggplot(data = df, aes(x = time, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments))+
  geom_line(aes(linetype = Treatments, color = Treatments))+
  scale_linetype_manual(values=c("solid", "solid","dashed", "dashed")) +
  scale_y_continuous(limits = c(0,0.2), name = "IgY Concentration") +
  scale_x_discrete(labels = c("Pre-Injection", "24hrPost", "72hrPost", "1wkPost", "2wkPost", "4wkPost"), name = "Immune Challenge 1 Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values=c("#0033FF", "#FF33CC", "#33FFFF","#FF0000"))

data2 <- gi %>%
  filter(time %in% c("0525igy", "0528igy", "0530igy", "0603igy", "0610igy","0624igy")) 

df2 <- data.frame(data2 %>%
                   group_by(tx,time) %>%
                   get_summary_stats(igy, type = "mean_se"))%>%
                  rename("Treatments" = tx)
head(df2)

library(data.table)
setDT(df2)

df2[Treatments == "G-C", Treatments := "SugarPBS"]
df2[Treatments == "G-L", Treatments := "SugarLPS"]
df2[Treatments == "W-C", Treatments := "WaterPBS"]
df2[Treatments == "W-L", Treatments := "WaterLPS"]

p2 <- ggplot(data = df2, aes(x = time, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments))+
  geom_line(aes(linetype = Treatments, color = Treatments))+
  scale_linetype_manual(values=c("solid", "solid","dashed", "dashed")) +
  scale_y_continuous(limits = c(0,0.2), name = "IgY Concentration") +
  scale_x_discrete(labels = c("Pre-Injection", "24hrPost", "72hrPost", "1wkPost", "2wkPost", "4wkPost"), name = "Immune Challenge 2 Timeline")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values=c("#0033FF", "#FF33CC", "#33FFFF","#FF0000"))



nested <- (p1/p2)+
  plot_annotation(tag_levels = 'A')
nested
library(patchwork)
