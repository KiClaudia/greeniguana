# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

# Use one way anova with different data sheet where there are 4 "treatments" gluoseday0 glucose day107, water day0 and water day107
original<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/GiWideDay0_107Data.csv")
View(original)

data <- original %>%
  select(iguanaID, tx, mass)
View(data)

hist((data$mass)) 

aov <- aov(data = data, mass~tx)
summary.aov(aov) #significant, need to do post hoc

tukey<-TukeyHSD(aov)
tukey

ggboxplot(data, x = "tx",  y = "mass")

data %>%
  group_by(tx) %>%
  get_summary_stats(mass, type = "mean_se")

