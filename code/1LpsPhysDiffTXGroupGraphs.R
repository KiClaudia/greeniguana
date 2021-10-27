# Graphically looking at basic phys difference before and after 1st LPS treatment
# Comparing to post glucose pre LPS 0423to see effects of Glucose on LPS challenge
gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(gi)
library("tidyverse")
library("dplyr")

# 03/24 is pre-study baseline, no treatment or diet
# 04/23 post glucose, pre-lps
# 04/28 24hr post LPS
# 04/30 72hr post LPS
# 05/04 1wk post LPS
# 05/11 2wk post LPS
# 05/18 3wk post LPS
# 05/25 4wk post LPS

glucose <- gi %>%
  filter(diet == "g") 
water <- gi %>%
  filter(diet == "w")

boxplot(glucose$X0423bka, water$X0423bka,  xlab = "Treatment Groups", 
                             ylab = "Bacterial percent killed", 
                             main = "Change in BKA across glucose and water treatment group pre- and post- diet",
                             names = c("Pre-diet glucose group", "Post-diet glucose group", "Pre-diet water group", "Post-diet water group"))


