# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

# -----Use one way anova with different data sheet where there are 4 "treatments" gluoseday0 glucose day107, water day0 and water day107-------------
original<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/GiWideDay0_107Data.csv")
View(original)

data <- original %>%
  select(iguanaID, tx, mass)
View(data)

hist((data$mass)) #normal

aov <- aov(data = data, mass~tx)
summary.aov(aov) #significant, need to do post hoc

tukey<-TukeyHSD(aov)
tukey

ggboxplot(data, x = "tx",  y = "mass")

data %>%
  group_by(tx) %>%
  get_summary_stats(mass, type = "mean_se")


#-------------RM 2way ANOVA-----------------------
originalWide<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(originalWide)

dataWide <- originalWide %>%
  select(iguanaID, diet, X0324mass, X0624mass)
View(dataWide)
dataLong <- dataWide %>%
  gather(key = "time", value = "mass", X0324mass, X0624mass) %>%
  convert_as_factor(iguanaID, time, iguanaID)
View(dataLong)
str(dataLong)

hist((dataLong$mass))

dataLong %>%
  group_by(diet, time) %>%
  get_summary_stats(mass, type = "mean_se")

ggboxplot(
  dataLong, x = "time", y = "mass",
  color = "diet", palette = "jco")

dataLong %>%
  group_by(diet, time) %>%
  identify_outliers(mass) # no extreme outliers
dataLong %>%
  group_by(diet, time) %>%
  shapiro_test(mass) # normally distributed
ggqqplot(dataLong, "mass", ggtheme = theme_bw()) +
  facet_grid(time ~ diet, labeller = "label_both") # pretty much along the line

sat = anova_test(
  data = dataLong, 
  mass ~ diet * time,
  wid = iguanaID
)
get_anova_table(sat) # significant effect of time

dataLong %>%
  pairwise_t_test(
    mass ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
# mass is different at time 0 and time 107 (regardless of diet treatment)


    