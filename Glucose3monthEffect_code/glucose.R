# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

#glucose

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

#----- Use one way anova with different data sheet where there are 4 "treatments" gluoseday0 glucose day107, water day0 and water day107-------------
data<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/GiWideDay0_107Data.csv")
View(data)

hist((data$glucose)) # normal

ggboxplot(
  data, x = "tx",  y = "glucose")

aov <- aov(data = data, glucose~tx)
summary.aov(aov) #significant, need to do post hoc

tukey<-TukeyHSD(aov)
tukey

data %>%
  group_by(tx) %>%
  get_summary_stats(glucose, type = "mean_se")



#----RM 2 way aov-----------
originalWide<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(originalWide)

dataWide <- originalWide %>%
  select(iguanaID, diet, X0324glu, X0624glu)
View(dataWide)
dataLong <- dataWide %>%
  gather(key = "time", value = "glu", X0324glu, X0624glu) %>%
  convert_as_factor(iguanaID, time)
View(dataLong)
str(dataLong)

hist((dataLong$glu))

dataLong %>%
  group_by(diet, time) %>%
  identify_outliers(glu) # outlier of 493
dataLong <- dataLong %>%
  filter(iguanaID != "5" )
View(dataLong)

dataLong %>%
  group_by(diet, time) %>%
  shapiro_test(glu) # normally distributed
ggqqplot(dataLong, "glu", ggtheme = theme_bw()) +
  facet_grid(time ~ diet, labeller = "label_both") # pretty much along the line

sat = anova_test(
  data = dataLong, 
  glu ~ diet * time,
  wid = iguanaID
)
get_anova_table(sat) # significant effect of time

dataLong %>%
  pairwise_t_test(
    glu ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

dataLong %>%
  group_by(diet, time) %>%
  get_summary_stats(glu, type = "mean_se")

ggboxplot(
  dataLong, x = "time", y = "glu",
  color = "diet", palette = "jco")
