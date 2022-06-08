# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

#BKA

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library(betareg)
library(emmeans)

#------ Use one way anova with different data sheet where there are 4 "treatments" gluoseday0 glucose day107, water day0 and water day107--------
original<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/GiWideDay0_107Data.csv")
View(original)

hist((original$bka)) # not normal, do kruskal wallis
ggboxplot(
  original, x = "tx",  y = "bka")

kruskal.test(data = original, bka~tx)
posthoc <- pairwise.wilcox.test(original$bka, original$tx,
                                p.adjust.method = "bonferroni")
posthoc

original %>%
  group_by(tx) %>%
  get_summary_stats(bka, type = "mean_se")


#----day107-day0 t-test or kruskal/-----
original<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(original)

bka <- original %>%
  select(iguanaID, diet, X0324bka, X0624bka) %>%
  na.omit()%>%
  mutate(diff = X0624bka - X0324bka)
View(bka)

bka %>%
  group_by(diet) %>%
  identify_outliers(diff) # 10,16,20,33 outliers
bka <- bka %>%
  filter(iguanaID != "16")
bka <- bka %>%
  filter(iguanaID != "10")
bka <- bka %>%
  filter(iguanaID != "20")
bka <- bka %>%
  filter(iguanaID != "23")
View(bka)

hist((bka$diff))

bka %>%
  group_by(diet) %>%
  shapiro_test(diff) #not normal

ggqqplot(bka, "diff", ggtheme = theme_bw()) 

bartlett.test(diff~diet, bka) #test for homoskedasticity, this data is heteroskedastic meaning its variances is not homogeneous.
# we cannot use the kruskal wallis test if its not homoskedastic.


#--------beta-regression model----------
original<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/GiWideDay0_107Data.csv")
View(original)

#betareg needs to be between 0 and 1 but not 0 and 1, so the package suggests you
# do this (y * (n-1) + 0.5) / n where n is the sample size.

bkadata <- original %>%
  select(iguanaID, diet, tx, time, bka) %>%
  mutate(decimal = bka/100) %>%
  mutate(beta = (((decimal)*(72-1)+0.5)/72))
View(bkadata)

hist(bkadata$beta)

boxplot(bkadata$beta ~ bkadata$tx)

model <- betareg::betareg(bkadata$beta ~ bkadata$tx)
summary(model)

Post_Hoc <- emmeans (model, specs = pairwise ~ tx , type = "bka")
summary(Post_Hoc)
