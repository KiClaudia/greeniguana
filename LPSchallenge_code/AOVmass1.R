# Look for differences within time and across groups for mass data (primary LPS)

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
  filter(time %in% c("0423mass","0428mass", "0430mass", "0504mass", "0511mass","0525mass")) 
View(data1)

hist(log(data1$mass)) #data is normal when logged
data1$mass <- log(data1$mass)
hist(data1$mass)

anova_test(
  data = data1, dv = mass, wid = iguanaID,
  between = c(diet, lps), within = time) 

ggboxplot(
  data1, x = "time",  y = "mass", color= "lps", palette = "jco"
)
ggboxplot(
  data1, x = "time",  y = "mass", palette = "jco"
)
ggboxplot(
  data1, x = "time",  y = "mass", color= "lps", palette = "jco", facet.by = "diet"
)
data1 %>%
  pairwise_t_test(
    mass ~ time, paired = FALSE, 
    p.adjust.method = "BH"
  ) #pairwise for time

# main effect of time was significant in ANOVA but not in pairwise (I didn't check for LPS time since there is a 3way)
# 3 way interaction of diet: LPS: time

# Post hoc for 3 way, anova on diet*lps at each level of time
#0423
data0423 <- data1 %>%
  filter(time == "0423mass")
View(data0423)  

aov0423 <- anova_test(
  data = data1, dv = mass, wid = iguanaID,
  between = c(diet, lps)) 
aov0423

ggboxplot(
  data0423, x = "lps",  y = "mass", color = "diet"
)
data0423 %>%
  group_by(diet, lps) %>%
  get_summary_stats(type = c("mean_se"))
# at 0423, diet (water lower weight) and lps (lps lower weight) main effect

#0428
data0428 <- data1 %>%
  filter(time == "0428mass")
View(data0428)  

aov0428 <- anova_test(
  data = data0428, dv = mass, wid = iguanaID,
  between = c(diet, lps)) 
aov0428

ggboxplot(
  data0428, x = "lps",  y = "mass", color = "diet"
)
#0430
data0430 <- data1 %>%
  filter(time == "0430mass")
View(data0430)  

aov0430 <- anova_test(
  data = data0430, dv = mass, wid = iguanaID,
  between = c(diet, lps)) 
aov0430

#0504
data0504 <- data1 %>%
  filter(time == "0504mass")
View(data0504)  

aov0504 <- anova_test(
  data = data0504, dv = mass, wid = iguanaID,
  between = c(diet, lps)) 
aov0504

#0511
data0511 <- data1 %>%
  filter(time == "0511mass")
View(data0511)  

aov0511 <- anova_test(
  data = data0511, dv = mass, wid = iguanaID,
  between = c(diet, lps)) 
aov0511

#0525
data0525 <- data1 %>%
  filter(time == "0525mass")
View(data0525)  

aov0525 <- anova_test(
  data = data0525, dv = mass, wid = iguanaID,
  between = c(diet, lps)) 
aov0525
