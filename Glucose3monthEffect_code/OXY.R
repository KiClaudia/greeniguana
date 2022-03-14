# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

#OXY

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

# Use one way anova with different data sheet where there are 4 "treatments" gluoseday0 glucose day107, water day0 and water day107
data<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/GiWideDay0_107Data.csv")
View(data)

hist((data$oxy)) # normal

ggboxplot(
  data, x = "tx",  y = "oxy")

aov <- aov(data = data, oxy~tx)
summary.aov(aov) #not significant

data %>%
  group_by(tx) %>%
  get_summary_stats(oxy, type = "mean_se")

