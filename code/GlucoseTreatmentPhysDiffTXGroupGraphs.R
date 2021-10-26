# Graphically looking at basic phys difference before and after glucose treatment using repeated measures method 

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(gi)
install.packages("tidyverse")
library("tidyverse")
library("dplyr")

# 03/24 is pre-study baseline, no treatment or diet
# 03/25 diet begins
# 04/23 blood sample after 30 days of glucose, pre-LPS etc. 

#------------ making boxplots of all the different phys variables before and after glucose SEPARATING OUT treatment groups
# so at this stage the two groups are glucose and non glucose

glucose <- gi %>%
  filter(diet == "g") 
  
water <- gi %>%
  filter(diet == "w") 
#----------------------------------------------------------------------------------
pdf('PrePostDietTrtGroupBKA.pdf',
    width = 10, height = 7)
PrePostGlucoseBKA <- boxplot(glucose$X0324bka, glucose$X0423bka, water$X0423bka, water$X0324bka, xlab = "Glucose treatment", 
                             ylab = "Bacterial percent killed", 
                             main = "Change in BKA across glucose and water treatment group pre- and post- diet",
                             names = c("Pre-diet glucose group", "Post-diet glucose group", "Pre-diet water group", "Post-diet water group"))

dev.off()


