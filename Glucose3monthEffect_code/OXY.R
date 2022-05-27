# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

#OXY

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

# ------Use one way anova with different data sheet where there are 4 "treatments" gluoseday0 glucose day107, water day0 and water day107--------
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



#----RM 2way aov-------------
originalWide<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(originalWide)

dataWide <- originalWide %>%
  select(iguanaID, diet, X0324oxystd, X0624oxystd)
View(dataWide)
dataLong <- dataWide %>%
  gather(key = "time", value = "oxy", X0324oxystd, X0624oxystd) %>%
  convert_as_factor(iguanaID, time, iguanaID)
View(dataLong)
str(dataLong)

hist((dataLong$oxy))

dataLong %>%
  group_by(diet, time) %>%
  identify_outliers(oxy) # iguana 3,26,27,34 outliers
dataLong <- dataLong %>%
  filter(!iguanaID %in% c("3","26","27","34"))

dataLong %>%
  group_by(diet, time) %>%
  shapiro_test(oxy) # normally distributed
ggqqplot(dataLong, "oxy", ggtheme = theme_bw()) +
  facet_grid(time ~ diet, labeller = "label_both") # pretty much along the line

sat = anova_test(
  data = dataLong, 
  oxy ~ diet * time,
  wid = iguanaID
)
get_anova_table(sat) # significant effect of time

dataLong %>%
  group_by(diet, time) %>%
  get_summary_stats(oxy, type = "mean_se")

ggboxplot(
  dataLong, x = "time", y = "oxy",
  color = "diet", palette = "jco")


# Interaction: 
# simple main: effect of diet on OXY at every point of time
day0glucose <- dataLong %>% 
  filter(time == "X0324oxystd", diet == "g")
day0water <- dataLong %>% 
  filter(time == "X0324oxystd", diet == "w")
day107glucose <- dataLong %>% 
  filter(time == "X0624oxystd", diet == "g")
day107water <- dataLong %>% 
  filter(time == "X0624oxystd", diet == "w")

t.test(day0glucose$oxy, day0water$oxy, paired = FALSE, alternative = "two.sided") # effect of diet treatment at start
t.test(day107glucose$oxy, day107water$oxy, paired = FALSE, alternative = "two.sided") # no effect of diet treatment at end

# pairwise: between diet groups at each time point
pwc <- dataLong %>%
  group_by(time) %>%
  pairwise_t_test(
    oxy ~ diet, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc) 

#Interaction:
# simple main: effect of time on  oxy for every level of diet
t.test(day0glucose$oxy, day107glucose$oxy, paired = TRUE, alternative = "two.sided") # glucose did not differ in OXY from start to end
t.test(day0water$oxy, day107water$oxy, paired = TRUE, alternative = "two.sided") # water group had higher OXY at the end

# pairwise: between time points for each diet level
pwc2 <- dataLong %>%
  group_by(diet) %>%
  pairwise_t_test(
    oxy ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc2) 
