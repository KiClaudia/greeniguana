# Look for differences within time and across groups for glycerol data (primary LPS)

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/glycerolLong.csv")
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
  filter(time %in% c("0423gly","0428gly", "0430gly", "0504gly", "0511gly","0525gly")) 
View(data)
hist(log(data$glycerol)) #normal when logged
data$glycerol <- log(data$glycerol)
hist(data$glycerol)

anova_test(
  data = data, dv = glycerol, wid = iguanaID,
  between = c(diet, lps), within = time) 

data %>%
  group_by(diet) %>%
  get_summary_stats(glycerol, type = "mean_se")

data %>%
  pairwise_t_test(
    glycerol ~ time, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )

data %>%
  group_by(time) %>%
  anova_test(dv = glycerol, wid = iguanaID, between = lps) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

# main effect of diet and time 
# interaction of LPS:time (not significant upon post hoc)