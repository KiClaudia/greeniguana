# Look for differences within time and across groups for true Triglyceride data (primary LPS)

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
  filter(time %in% c("0423trtri","0428trtri", "0430trtri", "0504trtri", "0511trtri","0525trtri")) 
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
# main effect of diet (glucose higher) and time (0423/0511 and 0428/0511)

