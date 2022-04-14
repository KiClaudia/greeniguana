# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

#true tri

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

# Use one way anova with different data sheet where there are 4 "treatments" gluoseday0 glucose day107, water day0 and water day107
original<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/GiWideDay0_107Data.csv")
View(original)

data <- original %>%
  select(iguanaID, tx, trutri) %>%
  na.omit()
View(data)

hist((data$trutri)) # not normal, do kruskal wallis

ggboxplot(
  data, x = "tx",  y = "glucose")
kruskal.test(data = data, trutri~tx)
posthoc <- pairwise.wilcox.test(data$trutri, data$tx,
                                p.adjust.method = "bonferroni")
posthoc

data %>%
  group_by(tx) %>%
  get_summary_stats(trutri, type = "mean_se")


# Day 107 both G and W higher than Day 0
# Water and Glucose at Day 0 not different
# Glucose group on 107 higher than Water group on 107

