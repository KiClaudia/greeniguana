# Mixed model anova or between-within anova is used to compare across groups of different
# factors and within group (across time)

# assumptions: 1) no outliers 2) normality 3) homogeneity of variances 4) sphericity
# 5) homogenity of covariances
# https://www.datanovia.com/en/lessons/mixed-anova-in-r/

# we are going to look at interactions between treatment and time

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/BKA1LPSlong.csv")
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
str(gi)

gi %>%
  group_by(time, tx) %>%
  get_summary_stats(bka, type = "mean_sd")

bxp <- ggboxplot(
  gi, x = "time",  y = "bka",
  color = "tx", palette = "jco"
)
bxp

BKAaov <- anova_test(
  data = gi, dv = bka, wid = iguanaID,
  between = c(diet, lps), within = time
)
get_anova_table(BKAaov)

# treatment and time were significant for main effects and not interaction, do PWC post hoc
pwcTime <- gi %>%
  pairwise_t_test(
    bka ~ time, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )
pwcTX <- gi %>%
  pairwise_t_test(
    bka ~ tx, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )

# There were statistically significant main effects of treatment (F(3, 429) = 5.16, p < 0.05) 
# and time (F(3.84, 111.27) = 15.65 , p < 0.05) on the BKA.
# Considering the Bonferroni adjusted p-value, the simple main effect of treatment group was significant 
# for GC and WC (p = 0.0018); and GL and WC  (p = 0.034). This means that were was a different in response for BKA across time
# for groups given glucose and water when injected for saline. We can't really compare GL and WC group but there is a 
# difference there as well. 
# Regardless of treatment, there is a difference in BKA response across time, refer to table. Biologically, this means
# LPS injection leads to a significantly higher BKA 24 hour post injection than pre injection but at 72 hour, it goes
# back to about where BKA values were pre injection. Interestingly, at 1 week post, values are significantly lower than 
# baseline (almost like a refractory period). At 2 and 4 week it levels back out to pre-injection values


#----Do the mixed model aov for just the  baseline, 24 hour and 72 hour time point so nothing gets washed out--------------
# filter out dates so that only 0423, 0428 and 0430 are present
data <- gi %>%
  filter(time %in% c("0423bka", "0428bka", "0430bka"))
View(data)

data %>%
  group_by(time, tx) %>%
  get_summary_stats(bka, type = "mean_sd")

bxp <- ggboxplot(
  data, x = "time",  y = "bka",
  color = "tx", palette = "jco"
)
bxp

spikeaov <- anova_test(
  data = data, dv = bka, wid = iguanaID,
  between = tx, within = time
)
get_anova_table(spikeaov)

data2 <- gi %>%
  filter(time %in% c("0428bka", "0430bka"))
View(data2)

data2 %>%
  group_by(time, tx) %>%
  get_summary_stats(bka, type = "mean_sd")

bxp <- ggboxplot(
  data2, x = "time",  y = "bka",
  color = "tx", palette = "jco"
)
bxp

spike2aov <- anova_test(
  data = data2, dv = bka, wid = iguanaID,
  between = tx, within = time
)
get_anova_table(spike2aov)
