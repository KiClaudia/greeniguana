# Look for differences within time and across groups for agglutination data (secondary LPS)

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
gi$diet <- as.factor(gi$diet)
gi$lps <- as.factor(gi$lps)
str(gi)

data <- gi %>%
  filter(time %in% c("0525agg","0528agg", "0530agg", "0603agg", "0610agg","0624agg")) 
View(data)
hist(sqrt(data$agg)) #use sqrt to make data normal
data$agg<- sqrt(data$agg)
hist(data$agg)

data %>%
  group_by(diet, lps) %>%
  get_summary_stats(agg, type = "mean_sd")

bxp <- ggboxplot(
  data, x = "time",  y = "agg",
  color = "tx", palette = "jco"
)
bxp

aggaov <- anova_test(
  data = data, dv = agg, wid = iguanaID,
  between = c(diet, lps), within = time
)
get_anova_table(aggaov)
# main effect of lps and time (after transforming data), diet is 0.051 
# backtransformed to get averages
# diet - glucose is 3.03, water is 1.78
# lps - LPS is 3.26, control is 1.55
# time - not actually significant in pairwise

data %>%
  pairwise_t_test(
    agg ~ time, paired = FALSE, 
    p.adjust.method = "none"
  )


# Baseline-72hour and 1week-4week
datashort <- gi %>%
  filter(time %in% c("0603agg", "0610agg","0624agg"))
View(datashort)

datashort %>%
  group_by(time, tx) %>%
  get_summary_stats(agg, type = "mean_sd")

bxp <- ggboxplot(
  datashort, x = "time",  y = "agg",
  color = "tx", palette = "jco"
)
bxp

datashort$agg <- sqrt(datashort$agg)
hist(datashort$agg)
anova_test(
  data = datashort, dv = agg, wid = iguanaID,
  between = c(diet, lps), within = time)

# Same effect found for both as the whole model

