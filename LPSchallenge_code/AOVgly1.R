# Look for differences within time and across groups for glycerol data (primary LPS)

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/glycerolLong.csv")
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
  filter(time %in% c("0423gly","0428gly", "0430gly", "0504gly", "0511gly","0525gly")) 
View(data)
hist((data$glycerol)) #normal when logged

data <- data %>%
  mutate(gly_inflog =  log(glycerol+0.0001))            # data was inflatd by 0.0001 to avoid inf when logged
View(data)
hist(data$gly_inflog)             # huge outlier at -10

data <- data %>%
  filter(gly_inflog > -5)          #got rid of massive outlier (10x the avg)
hist(data$gly_inflog)


anova_test(
  data = data, dv = gly_inflog, wid = iguanaID,
  between = c(diet, lps), within = time) 

data %>%
  group_by(diet) %>%
  get_summary_stats(gly_inflog, type = "mean_se")

data %>%
  pairwise_t_test(
    gly_inflog ~ time, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )

# main effect of diet and time 
