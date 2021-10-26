# Graphically looking at basic phys difference before and after glucose treatment using repeated measures method. 

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(gi)

install.packages("tidyverse")
library("tidyverse")

# 03/24 is pre-study baseline, no treatment or diet
# 03/25 diet begins
# 04/23 blood sample after 30 days of glucose, pre-LPS etc. 

# The phys variables we care about are bka, agg, lysis, glucose, totri, oi, trtri, glu

colnames(gi)

sum <- gi %>%
  group_by(tx) %>%
  summarise(mean(X0324bka), sd(X0324bka), count=n()) %>%
  print()
  
sum <- gi %>%
  group_by(tx) %>%
  summarise(mean(X0423bka), sd(X0423bka), count=n()) %>%
  print()

boxplot(gi$X0324bka, gi$X0423bka)

library(ggplot2)
