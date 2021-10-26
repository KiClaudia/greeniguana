# Looking at basic phys difference before and after glucose treatment, summary table

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(gi)
install.packages("tidyverse")
library("tidyverse")
library("dplyr")

# 03/24 is pre-study baseline, no treatment or diet
# 03/25 diet begins
# 04/23 blood sample after 30 days of glucose, pre-LPS etc. 

# The phys variables we care about are bka, agg, lysis, glucose, totri, oi, trtri, glu, gly

summarydf <- data.frame(matrix(ncol = 18, nrow = 3)) 
#making an empty dataframe to put all the summary data in
str(summarydf)

summarydf %>% 
  mutate(pre-glucose_bka, mean(X0324bka))

sum <- gi %>%
  group_by(tx) %>%
  summarise(mean(X0324agg), sd(X0324bka), count=n()) %>%
  print()

sum <- gi %>%
  group_by(X0423agg) %>%
  summarise(mean(X0423agg), sd(X0423bka), count=n()) %>%
  print()
#not done, i'll come back to this
