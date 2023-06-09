# Look for differences within time and across groups for svl data 

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/SVLlong.csv")
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

data1 <- gi %>%
  filter(time %in% c("0317svl","0525svl", "0625svl")) 
View(data1)

hist(log(data1$svl)) #logged data is normal
data1$svl <- log(data1$svl)
hist(data1$svl)

anova_test(
  data = data1, dv = svl, wid = iguanaID,
  between = c(diet, lps), within = time) 

simpleMainEffect <- data1 %>%
  group_by(time) %>%
  anova_test(dv = svl, wid = iguanaID, between = lps) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "BH")
View(simpleMainEffect) #no real interaction effect

data1 %>%
  pairwise_t_test(
    svl ~ time, paired = FALSE, 
    p.adjust.method = "BH"
  )

ggboxplot(
  data1, x = "time",  y = "svl")


data1 %>%
  group_by(time) %>%
  get_summary_stats(svl, type = "mean_se")
