# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

#lysis

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

#----- Use one way anova with different data sheet where there are 4 "treatments" gluoseday0 glucose day107, water day0 and water day107------
original<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/GiWideDay0_107Data.csv")
View(original)

hist((original$lysis)) # not normal, do kruskal wallis

ggboxplot(
  original, x = "tx",  y = "lysis")

kruskal.test(data = original, lysis~tx)

original %>%
  group_by(tx) %>%
  get_summary_stats(lysis, type = "mean_se")

#----day107-day0 t-test-----
original<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(original)

lys <- original %>%
  select(iguanaID, diet, X0324lys, X0624lys) %>%
  na.omit()%>%
  mutate(lysdiff = X0624lys - X0324lys)
View(lys)

lys %>%
  group_by(diet) %>%
  identify_outliers(lysdiff) # no extreme outliers

hist((lys$lysdiff))

lys %>%
  group_by(diet) %>%
  shapiro_test(lysdiff) #normal!!

ggqqplot(lys, "lysdiff", ggtheme = theme_bw()) 

ggboxplot(
  lys, x = "diet", y = "lysdiff",
  palette = "jco")

lys %>%
  group_by(diet) %>%
  get_summary_stats(lysdiff, type = "mean_se")

t.test(lysdiff~diet, data=lys)
