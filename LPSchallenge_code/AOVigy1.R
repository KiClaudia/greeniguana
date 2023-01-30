# Look for differences within time and across groups for IgY data (primary LPS)

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/igyLong.csv")
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
  filter(time %in% c("0423igy","0428igy", "0430igy", "0504igy", "0511igy","0525igy")) 
View(data)
hist((data$igy)) #normal

plot(data = data, igy~tx)
plot(data = data, igy~lps)
plot(data = data, igy~diet)
ggboxplot(
  data, x = "time",  y = "igy", color = "lps")
ggboxplot(
  data, x = "time",  y = "igy")


anova_test(
  data = data, dv = igy, wid = iguanaID,
  between = c(diet, lps), within = time) 

data %>%
  group_by(lps) %>%
  get_summary_stats(igy, type = "mean_se")

data %>%
  pairwise_t_test(
    igy ~ time, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )


simpleMainEffect <- data %>%
  group_by(time) %>%
  anova_test(dv = igy, wid = iguanaID, between = lps) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
View(simpleMainEffect)

simpleMainEffect <- data %>%
  group_by(lps) %>%
  anova_test(dv = igy, wid = iguanaID, between = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
View(simpleMainEffect)
# main effect of lps and time (PWC), interaction of lps*time (simple main)
