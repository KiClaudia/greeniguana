# Look for differences within time and across groups for glucose data 

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/GlucoseLong.csv")
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
  filter(time %in% c("0423glu","0430glu", "0428glu", "0525glu")) 
View(data)
hist((data$glu)) #data is normal 

anova_test(
  data = data, dv = glu, wid = iguanaID,
  between = c(diet, lps), within = time) 

data %>%
  pairwise_t_test(
    glu ~ time, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )

# main effect of time (0423 and 0430)


data2 <- gi %>%
  filter(time %in% c("0528glu","0530glu", "0624glu", "0525glu")) 
View(data2)
hist((data2$glu)) #data is normal 

anova_test(
  data = data2, dv = glu, wid = iguanaID,
  between = c(diet, lps), within = time) 

# no effect 