# Look for differences within time and across groups for glycerol data (secondary LPS)

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

data2 <- gi %>%
  filter(time %in% c("0525gly","0528gly", "0530gly", "0603gly", "0610gly","0624gly")) 
View(data2)
hist(log(data2$glycerol)) #normal when logged

anova_test(
  data = data2, dv = glycerol, wid = iguanaID,
  between = c(diet, lps), within = time) 

data2 %>%
  group_by(diet) %>%
  get_summary_stats(glycerol, type = "mean_se")

data2 %>%
  pairwise_t_test(
    glycerol ~ time, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )


# main effect time and diet, interaction wasn't significant in post hoc
