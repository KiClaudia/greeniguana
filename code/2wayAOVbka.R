# What was the effect of LPS across time-points? 
# Use a repeated measures two way anova to look at the effect within group of phys

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/BKA1LPSlong.csv")
View(gi)
install.packages("tidyverse")
install.packages("rstatix")
install.packages("ggpubr")
library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

gi$diet <- as.factor(gi$diet)
gi$lps <- as.factor(gi$lps)
gi$time <- as.factor(gi$time)
gi$iguanaID <- as.factor(gi$iguanaID)
str(gi)

GL <- gi %>%
  filter(tx == "G-L") #filter the treatment groups out so we can look at summary stats
GC <- gi %>%
  filter(tx == "G-C")
WL<- gi %>%
  filter(tx == "W-L")
WC<- gi %>%
  filter(tx == "W-C")

GL %>%
  group_by(time) %>%
  summarise(mean(bka, na.rm = TRUE)
  )
GC %>%
  group_by(time) %>%
  summarise(mean(bka, na.rm = TRUE)
  )
WL %>%
  group_by(time) %>%
  summarise(mean(bka, na.rm = TRUE)
  )
WC %>%
  group_by(time) %>%
  summarise(mean(bka, na.rm = TRUE)
  )

boxplot(GL$bka ~ GL$time) # visualize the data
boxplot(GC$bka ~ GC$time)
boxplot(WL$bka ~ WL$time)
boxplot(WC$bka ~ WC$time)

GLaov <- anova_test(data = GL, dv = bka, wid = iguanaID, within = time)
get_anova_table(GLaov) # difference across time but which pairs?

GLpwc <- GL %>% # pair-wise comparison
  pairwise_t_test(
    bka ~ time, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
GLpwc

#2 way aov (above is one way)
gi %>%
  group_by(tx, time) %>%
  get_summary_stats(bka, type = "mean_sd") %>%
  View()

bxp <- ggboxplot(
  gi, x = "time", y = "bka",
  color = "tx", palette = "jco"
)
bxp


