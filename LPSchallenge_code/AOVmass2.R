# Look for differences within time and across groups for oxy data (primary LPS)

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/MassLong.csv")
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
  filter(time %in% c("0525mass","0528mass", "0530mass", "0603mass", "0610mass","0624mass")) 
View(data1)

hist(log(data1$mass)) #data is normal when logged
data1$mass <- log(data1$mass)
hist(data1$mass)

anova_test(
  data = data1, dv = mass, wid = iguanaID,
  between = c(diet, lps), within = time) 

ggboxplot(
  data1, x = "time",  y = "mass", color= "diet", palette = "jco"
)
ggboxplot(
  data1, x = "time",  y = "mass", palette = "jco"
)

data1 %>%
  pairwise_t_test(
    mass ~ time, paired = FALSE, 
    p.adjust.method = "BH"
  ) #pairwise for time

simpleMainEffect <- data1 %>%
  group_by(time) %>%
  anova_test(dv = mass, wid = iguanaID, between = diet) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "BH")
View(simpleMainEffect)

simpleMainEffect <- data1 %>%
  group_by(diet) %>%
  anova_test(dv = mass, wid = iguanaID, between = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "BH")
View(simpleMainEffect)
