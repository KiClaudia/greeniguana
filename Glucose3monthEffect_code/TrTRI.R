# Is there a difference between the first and last time point of the experiment (3 months) 
# between groups and across groups? Look at glucose and water group only, this is to see effects of glucose
# 0324 is first day and 0624 is the last, 4 week after LPS but before wound healing.

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/TrueTriLong.csv")
View(gi) #note that all NA rows were omitted automatically during the transposal
install.packages("tidyverse")
install.packages("rstatix")
install.packages("ggpubr")
library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
str(gi)
gi$time <- as.factor(gi$time)
gi$iguanaID <- as.factor(gi$iguanaID)
gi$tx <- as.factor(gi$tx)
gi$diet <- as.factor(gi$diet)
gi$lps <- as.factor(gi$lps)
str(gi)

data <- gi %>%
  filter(time %in% c("0324trtri","0624trtri")) 
View(data)
hist((data$trtri)) #data is normal if logged
data <- data %>%
  mutate(log = log(trtri))
hist(data$log)

anova_test(
  data = data, dv = log, wid = iguanaID,
  between = c(diet), within = time) 

ggboxplot(
  data, x = "diet",  y = "trtri", facet.by = "time"
)

data %>%
  group_by(diet) %>%
  get_summary_stats(trtri, type = "mean_se")

data %>%
  group_by(time) %>%
  get_summary_stats(trtri, type = "mean_se")

# main effect of diet (glucose always higher regardless of time) 
# main effect of time (end of experiment higher for both groupw regardless of treatment)

