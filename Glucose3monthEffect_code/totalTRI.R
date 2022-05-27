# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

# Use one way anova with different data sheet where there are 4 "treatments" gluoseday0 glucose day107, water day0 and water day107-----------------
onewaydata<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/1wayaovtotri.csv")
View(onewaydata)

hist(log(onewaydata$totri))
onewaydata <- onewaydata %>%
  mutate(log = log(totri))
hist(onewaydata$log )
View(onewaydata
     )
aov <- aov(data = onewaydata, log~tx)
summary.aov(aov) #significant, need to do post hoc

tukey<-TukeyHSD(aov)
tukey

ggboxplot(
  onewaydata, x = "tx",  y = "log")

onewaydata %>%
  group_by(tx) %>%
  get_summary_stats(totri, type = "mean_se")

#-------------RM 2way ANOVA-----------------------
originalWide<- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(originalWide)

dataWide <- originalWide %>%
  select(iguanaID, diet, X0324totri, X0624totri)
View(dataWide)
dataLong <- dataWide %>%
  gather(key = "time", value = "totri", X0324totri, X0624totri) %>%
  convert_as_factor(iguanaID, time)
View(dataLong)
str(dataLong)

hist((dataLong$totri))

dataLong %>%
  group_by(diet, time) %>%
  get_summary_stats(totri, type = "mean_se")

ggboxplot(
  dataLong, x = "time", y = "totri",
  color = "diet", palette = "jco")

dataLong %>%
  group_by(diet, time) %>%
  identify_outliers(totri) # no extreme outliers
dataLong %>%
  group_by(diet, time) %>%
  shapiro_test(totri) # normally distributed
ggqqplot(dataLong, "totri", ggtheme = theme_bw()) +
  facet_grid(time ~ diet, labeller = "label_both") # pretty much along the line

sat = anova_test(
  data = dataLong, 
  totri ~ diet * time,
  wid = iguanaID
)
get_anova_table(sat) # significant effect of diet, time, diet:time

# Pairwise comparisons for main effect of diet 
dataLong %>%
  pairwise_t_test(
    totri ~ diet, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) # glucose and water group is different

# Pairwise comparisons for main effect of time
dataLong %>%
  pairwise_t_test(
    totri ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) #day 0 and day 107 are different

# Interaction: 
# simple main: effect of diet on total triglycerides at every point of time
day0glucose <- dataLong %>% 
  filter(time == "X0324totri", diet == "g")
day0water <- dataLong %>% 
  filter(time == "X0324totri", diet == "w")
day107glucose <- dataLong %>% 
  filter(time == "X0624totri", diet == "g")
day107water <- dataLong %>% 
  filter(time == "X0624totri", diet == "w")

t.test(day0glucose$totri, day0water$totri, paired = TRUE, alternative = "two.sided") # no effect of diet treatment at start
t.test(day107glucose$totri, day107water$totri, paired = TRUE, alternative = "two.sided") # effect of diet treatment at end

# pairwise: between diet groups at each time point
pwc <- dataLong %>%
  group_by(time) %>%
  pairwise_t_test(
    totri ~ diet, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc) # difference between glucose and water group at end of study

#Interaction:
# simple main: effect of time on total tri for every level of diet
t.test(day0glucose$totri, day107glucose$totri, paired = TRUE, alternative = "two.sided") # diff in glucose group between start and end
t.test(day0water$totri, day107water$totri, paired = TRUE, alternative = "two.sided") # diff in water group between start and end

# pairwise: between time points for each diet level
pwc2 <- dataLong %>%
  group_by(diet) %>%
  pairwise_t_test(
    totri ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc2) # totri different between time points for each level
