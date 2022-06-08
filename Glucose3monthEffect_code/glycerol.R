# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

#glycerol
library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

#----- Use one way anova with different data sheet where there are 4 "treatments" gluoseday0 glucose day107, water day0 and water day107------
data<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/GiWideDay0_107Data.csv")
View(data)

hist((data$gly)) #can't really log to make it normal because lots of 0 and log(0) is infinity# can't inflate either because log of really small numbers are really big numbers
# use kruskal wallis which is one way aov for nonparametrirc

ggboxplot(
  data, x = "tx",  y = "gly")

kruskal.test(gly ~ tx, data = data)

posthoc <- pairwise.wilcox.test(data$gly, data$tx,
                     p.adjust.method = "bonferroni")
posthoc

data %>%
  group_by(tx) %>%
  get_summary_stats(gly, type = "mean_se")


#----day107-day0 t-test-----
original<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(original)

glydat <- original %>%
  select(iguanaID, diet, X0324gly, X0624gly) %>%
  na.omit()%>%
  mutate(glydiff = X0624gly - X0324gly)
View(glydat)

ggboxplot(
  glydat, x = "diet", y = "glydiff",
palette = "jco")

hist((glydat$glydiff))

glydat %>%
  group_by(diet) %>%
  identify_outliers(glydiff) # 16 is outlier
glydat <- glydat %>%
  filter(!c(iguanaID == "16"))

glydat %>%
  group_by(diet) %>%
  shapiro_test(glydiff) #normal!!
ggqqplot(glydat, "glydiff", ggtheme = theme_bw()) 
     

glydat %>%
  group_by(diet) %>%
  get_summary_stats(glydiff, type = "mean_se")

t.test(glydiff~diet, data=glydat)
