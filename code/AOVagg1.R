# Look for differences within time and across groups for agglutination data (primary LPS)
# 1) mixed model 2) two-way anova

#----Mixed model------------
gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/AggLong.csv")
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
str(gi)

data <- gi %>%
  filter(time %in% c("0423agg","0428agg", "0430agg", "0504agg", "0511agg")) 
View(data)

data %>%
  group_by(time, tx) %>%
  get_summary_stats(agg, type = "mean_sd")

bxp <- ggboxplot(
  data, x = "time",  y = "agg",
  color = "tx", palette = "jco"
)
bxp

Aggaov <- anova_test(
  data = data, dv = agg, wid = iguanaID,
  between = tx, within = time
)
get_anova_table(Aggaov)
# kind of messy to look across the all 4 weeks...
# try again with baseline, 24hr, 72hr
datashort <- gi %>%
  filter(time %in% c("0423agg","0428agg", "0430agg")) 
View(datashort)

datashort %>%
  group_by(time, tx) %>%
  get_summary_stats(agg, type = "mean_sd")

bxp <- ggboxplot(
  datashort, x = "time",  y = "agg",
  color = "tx", palette = "jco"
)
bxp

anova_test(
  data = data, dv = agg, wid = iguanaID,
  between = tx, within = time)
