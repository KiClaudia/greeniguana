# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

#OSI

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

#-- Use one way anova with different data sheet where there are 4 "treatments" gluoseday0 glucose day107, water day0 and water day107-----
data<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/GiWideDay0_107Data.csv")
View(data)

hist((data$osi)) # normal

ggboxplot(
  data, x = "tx",  y = "osi")

aov <- aov(data = data, osi~tx)
summary.aov(aov) #significant, need to do post hoc

data %>%
  group_by(tx) %>%
  get_summary_stats(osi, type = "mean_se")



#----2 way RM aov------
originalWide<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(originalWide)

dataWide <- originalWide %>%
  select(iguanaID, diet, X0324oi, X0624oi)
View(dataWide)
dataLong <- dataWide %>%
  gather(key = "time", value = "OSI", X0324oi, X0624oi) %>%
  convert_as_factor(iguanaID, time)
View(dataLong)
str(dataLong)

hist((dataLong$OSI))

dataLong %>%
  group_by(diet, time) %>%
  get_summary_stats(OSI, type = "mean_se")

ggboxplot(
  dataLong, x = "time", y = "OSI",
  color = "diet", palette = "jco")

dataLong %>%
  group_by(diet, time) %>%
  identify_outliers(OSI) # no extreme outliers
dataLong %>%
  group_by(diet, time) %>%
  shapiro_test(OSI) # normally distributed
ggqqplot(dataLong, "OSI", ggtheme = theme_bw()) +
  facet_grid(time ~ diet, labeller = "label_both") # pretty much along the line

sat = anova_test(
  data = dataLong, 
  OSI ~ diet * time,
  wid = iguanaID
)
get_anova_table(sat) # NOT significant 