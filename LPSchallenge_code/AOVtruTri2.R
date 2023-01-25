# Look for differences within time and across groups for true Triglyceride data (secondary LPS)

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
  filter(time %in% c("0525trtri","0528trtri", "0530trtri", "0603trtri", "0610trtri","0628trtri")) 
View(data)
hist((data$trtri)) #data is normal

anova_test(
  data = data, dv = trtri, wid = iguanaID,
  between = c(diet, lps), within = time) 

data %>%
  group_by(diet) %>%
  get_summary_stats(trtri, type = "mean_se")

data %>%
  pairwise_t_test(
    trtri ~ time, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )
# main effect of diet (glucose higher) 
# and time (0603 and 0525, 0528, 0503) (0610 and 0525, 0528, 0530) (0528, 0628)

library("pwr")
pwr.anova.test(k = 3, n = 104 , f = 0.151, sig.level = 0.05, power = NULL)
