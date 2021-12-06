# Anova to analyze BKA for the secondary immune challenge
gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/BKA2LPSlong.csv")
View(gi)
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
gi$lps <- as.factor(gi$lps)
gi$diet <- as.factor(gi$diet)
str(gi)

#------------RMANOVA---------------
bxp <- ggboxplot(
  gi, x = "time",  y = "bka",
  color = "tx", palette = "jco"
)
bxp
boxplot(data = gi, bka~diet*lps)

BKAaov <- anova_test(
  data = gi, dv = bka, wid = iguanaID,
  between = c(diet, lps), within = time
)
get_anova_table(BKAaov)

gi %>%
  group_by(diet) %>%
  get_summary_stats(bka, type = "mean_sd")

# main effect of diet, 49.1%bka for glucose and 68.7% for water
# can't actually run an anova because the data is bimodal, going to run GLM

#---------GLMM----------
#glucose and LPS are fixed effects, individual is random effect
hist(gi$bka)
#figure out which distribution to use
https://ase.tufts.edu/bugs/guide/assets/mixed_model_guide.html
