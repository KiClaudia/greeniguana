# Look for differences within time and across groups for total Triglyceride data (primary LPS)

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
  filter(time %in% c("0423totri","0428totri", "0430totri", "0504totri", "0511totri","0525totri")) 
View(data)
hist((data$totri)) #data is normal

anova_test(
  data = data, dv = totri, wid = iguanaID,
  between = c(diet, lps), within = time) 

data %>%
  group_by(diet) %>%
  get_summary_stats(totri, type = "mean_se")

simpleMainEffect <- data %>%
  group_by(time) %>%
  anova_test(dv = totri, wid = iguanaID, between = lps) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "BH")
View(simpleMainEffect)

data %>%
  pairwise_t_test(
    totri ~ time, paired = FALSE, 
    p.adjust.method = "BH"
  )
# main effect of diet (glucose higher) and time (refer to pwc)
# interaction effect of lps:time (at 0428)
