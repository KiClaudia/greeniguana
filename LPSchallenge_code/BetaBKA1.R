# GLMM with a logit link to fit BKA data form 1LPS challenge

install.packages("tidyverse")
install.packages("rstatix")
install.packages("ggpubr")
install.packages("glmmTMB")
library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library("glmmTMB")

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/BKA1LPSlong.csv")
View(gi)
str(gi)
gi$iguanaID <- as.factor(gi$iguanaID)
gi$time <- as.factor(gi$time)
gi$tx <- as.factor(gi$tx)
str(gi)

# Beta is between 0 and 1 so I'm setting all 0 to  0.000000000001 and all 100 into 99.999999. Technically different but biologically the same
gi <- gi %>% 
  mutate(decimal_bka = bka/100)
gi<- gi %>%
  mutate(beta_bka = replace(decimal_bka, decimal_bka == 0, 0.00000000001))
gi <- gi %>%
  mutate(final_bka = replace(beta_bka, beta_bka == 1, .99))
View(gi)
str(gi)

# Response Var = final_bka, Explanatory Var = tx, Fixed Var = time, Random Var= iguanaID bc differences in response between iguanas

glmm <- glmmTMB(final_bka ~ tx + time + (1|iguanaID), data = gi, (family = beta_family(link = "logit")))
summary(glmm)

