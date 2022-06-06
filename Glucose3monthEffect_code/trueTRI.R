# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

#true tri

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

#---- Use one way anova with different data sheet where there are 4 "treatments" gluoseday0 glucose day107, water day0 and water day107------
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

#-------------RM 2way ANOVA-----------------------
originalWide<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(originalWide)

dataWide <- originalWide %>%
  select(iguanaID, diet, X0324trtri, X0624trtri)
View(dataWide)
dataLong <- dataWide %>%
  gather(key = "time", value = "trtri", X0324trtri, X0624trtri) %>%
  convert_as_factor(iguanaID, time)
View(dataLong)
str(dataLong)

hist((dataLong$trtri))

dataLong %>%
  group_by(diet, time) %>%
  get_summary_stats(trtri, type = "mean_se")

ggboxplot(
  dataLong, x = "time", y = "trtri",
  color = "diet", palette = "jco")

dataLong %>%
  group_by(diet, time) %>%
  identify_outliers(trtri) # no extreme outliers
dataLong %>%
  group_by(diet, time) %>%
  shapiro_test(trtri) # normally distributed, only water 107 is a bit skewed

ggqqplot(dataLong, "trtri", ggtheme = theme_bw()) +
  facet_grid(time ~ diet, labeller = "label_both") # pretty much along the line

sat = anova_test(
  data = dataLong, 
  trtri ~ diet * time,
  wid = iguanaID
)
get_anova_table(sat) # significant effect of diet, time, diet:time

# Pairwise comparisons for main effect of diet 
dataLong %>%
  pairwise_t_test(
    trtri ~ diet, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) # glucose and water group is different

# Pairwise comparisons for main effect of time
dataLong %>%
  pairwise_t_test(
    trtri ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) #day 0 and day 107 are different

# Interaction: 
# simple main: effect of diet on true triglycerides at every point of time
day0glucose <- dataLong %>% 
  filter(time == "X0324trtri", diet == "g")
day0water <- dataLong %>% 
  filter(time == "X0324trtri", diet == "w")
day107glucose <- dataLong %>% 
  filter(time == "X0624trtri", diet == "g")
day107water <- dataLong %>% 
  filter(time == "X0624trtri", diet == "w")

t.test(day0glucose$trtri, day0water$trtri, paired = TRUE, alternative = "two.sided") # no effect of diet treatment at start
t.test(day107glucose$trtri, day107water$trtri, paired = TRUE, alternative = "two.sided") # effect of diet treatment at end

# pairwise: between diet groups at each time point
pwc <- dataLong %>%
  group_by(time) %>%
  pairwise_t_test(
    trtri ~ diet, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc) # difference between glucose and water group at end of study

#Interaction:
# simple main: effect of time on total tri for every level of diet
t.test(day0glucose$trtri, day107glucose$trtri, paired = TRUE, alternative = "two.sided") # diff in glucose group between start and end
t.test(day0water$trtri, day107water$trtri, paired = TRUE, alternative = "two.sided") # diff in water group between start and end

# pairwise: between time points for each diet level
pwc2 <- dataLong %>%
  group_by(diet) %>%
  pairwise_t_test(
    trtri ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc2) # totri different between time points for each level
