# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

#agglutination

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

#----- Use one way anova with different data sheet where there are 4 "treatments" gluoseday0 glucose day107, water day0 and water day107-----
original<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/GiWideDay0_107Data.csv")
View(original)

hist((original$agg)) # not normal, do kruskal wallis

ggboxplot(
  original, x = "tx",  y = "agg")

kruskal.test(data = original, agg~tx)

posthoc <- pairwise.wilcox.test(original$agg, original$tx,
                                p.adjust.method = "bonferroni")
posthoc

original %>%
  group_by(tx) %>%
  get_summary_stats(agg, type = "mean_se")

#----day107-day0 t-test-----
original<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(original)

aggl <- original %>%
  select(iguanaID, diet, X0324agg, X0624agg) %>%
  na.omit()%>%
  mutate(aggdiff = X0624agg - X0324agg)
View(aggl)

aggl %>%
  group_by(diet) %>%
  identify_outliers(aggdiff) # no extreme outliers

hist((aggl$aggdiff))

aggl %>%
  group_by(diet) %>%
  shapiro_test(aggdiff) #normal!!

ggqqplot(aggl, "aggdiff", ggtheme = theme_bw()) 

ggboxplot(
  aggl, x = "diet", y = "aggdiff",
  palette = "jco")

aggl %>%
  group_by(diet) %>%
  get_summary_stats(aggdiff, type = "mean_se")

t.test(aggdiff~diet, data=aggl)
