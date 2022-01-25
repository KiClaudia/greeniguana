# Look for differences within time and across groups for drom data (primary LPS)

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/dromLong.csv")
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
  filter(time %in% c("0423dromstd","0428dromstd", "0430dromstd", "0504dromstd", "0511dromstd","0525dromstd")) 
View(data)
hist((data$drom)) #data is normal

anova_test(
  data = data, dv = drom, wid = iguanaID,
  between = c(diet, lps), within = time) 

data %>%
  group_by(diet) %>%
  get_summary_stats(drom, type = "mean_sd")

bxp <- ggboxplot(
  data, x = "lps",  y = "drom", palette = "npg ")
bxp

data %>%
  group_by(lps) %>%
  pairwise_t_test(
    drom ~ time, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )  %>%
  View()
# main effect of diet (glucose is higher)
# interaction effect of lps:time
