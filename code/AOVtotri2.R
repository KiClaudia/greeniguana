# Look for differences within time and across groups for total Triglyceride data (secondary LPS)

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/triLong.csv")
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
  filter(time %in% c("0525totri","0528totri", "0530totri", "0603totri", "0610totri","0628totri")) 
View(data)
hist((data$totri)) #data is normal enough lol

anova_test(
  data = data, dv = totri, wid = iguanaID,
  between = c(diet, lps), within = time) 

data %>%
  group_by(diet) %>%
  get_summary_stats(totri, type = "mean_se")

data %>%
  pairwise_t_test(
    totri ~ time, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )

simpleMainEffect <- data %>%
  group_by(time) %>%
  anova_test(dv = totri, wid = iguanaID, between = lps) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
View(simpleMainEffect)

simpleMainEffect2 <- data %>%
  group_by(time) %>%
  anova_test(dv = totri, wid = iguanaID, between = diet) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
View(simpleMainEffect2)
data %>%
  group_by(diet, time) %>%
  get_summary_stats(totri, type = "mean_se")
# main effect of diet (glucose higher) and time (pwc table)
# interaction effect of diet:time (glucose always significant at every time point) and lps:time (actually none after post hoc)
