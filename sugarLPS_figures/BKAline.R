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
pdf('BKAline.pdf')

ggplot(data = df2, aes(x = time, y = mean, group = Treatments)) +
  geom_point(aes(color = Treatments), size = 2)+
  geom_line(aes(linetype = Treatments, color = Treatments), size = 1.3)+
  scale_linetype_manual(values=c("dotdash", "solid","dotdash", "solid")) +
  scale_y_continuous(limits = c(0,100), name = "Percent of Bacteria Killed") +
  scale_x_discrete(labels = c("Pre-Injection", "24hrPost", "72hrPost", "1wkPost", "2wkPost", "4wkPost"), name = "Time")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.05) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values=c("navy", "navy", "steelblue1","steelblue1"))

dev.off()




























