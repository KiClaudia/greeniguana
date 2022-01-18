# Look for differences within time and across groups for agglutination data (primary LPS)

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
gi$lps <- as.factor(gi$lps)
gi$diet <- as.factor(gi$diet)
str(gi)

data <- gi %>%
  filter(time %in% c("0423agg","0428agg", "0430agg", "0504agg", "0511agg", "0525agg")) 
View(data)
str(data)

# summary stats
data %>%
  group_by(time, diet, lps) %>%
  get_summary_stats(agg, type = "mean_se")
# data visualization
bxp <- ggboxplot(
  data, x = "lps",  y = "agg",
  color = "time", palette = "jco", facet.by = "diet"
)
bxp

# check assumptions
# outliers?
data %>%
  group_by(diet, lps, time) %>%
  identify_outliers(agg)
# normality?
data %>%
  group_by(diet, lps, time) %>%
  shapiro_test(agg) %>%
  View()

ggqqplot(data, "agg", ggtheme = theme_bw()) +
  facet_grid(diet + lps ~ time, labeller = "label_both")




hist((data$agg)) #sqrt data to make it normal
data$agg <- sqrt(data$agg)
View(data)



Aggaov <- anova_test(
  data = data, dv = agg, wid = iguanaID,
  within = c(time, lps, diet)
)
get_anova_table(Aggaov)





# come back to this code below for later more specific stuff
datashort <- gi %>%
  filter(time %in% c("0423agg","0430agg")) 
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
  data = datashort, dv = agg, wid = iguanaID,
  between = tx, within = time)

# Main time and treatment effect for whole course
# Only a time effect on baseline, 24, 72
# "" for baseline 24
# Main time and treatment effect for baseline and 72
# No effect for 24 and 72

