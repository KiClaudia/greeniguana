# Look for differences within time and across groups for oxy data (primary LPS)

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/OXYlong.csv")
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
  filter(time %in% c("0423oxystd","0428oxystd", "0430oxystd", "0504oxystd", "0511oxystd","0525oxystd")) 
View(data)
hist((data$oxy)) #data is normal 

anova_test(
  data = data, dv = oxy, wid = iguanaID,
  between = c(diet, lps), within = time) 

ggboxplot(
  data, x = "time",  y = "oxy", color= "diet", palette = "jco"
)

# No effect of OXY over whole treatment

