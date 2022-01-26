gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/OsiLong.csv")
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
  filter(time %in% c("0423oi","0428oi", "0430oi", "0504oi", "0511oi","0525oi")) 
View(data)

hist((data$osi)) #data is normal

data %>%
  group_by(diet) %>%
  get_summary_stats(osi, type = "mean_se")

bxp <- ggboxplot(
  data, x = "diet",  y = "osi", color = "lps", 
  palette = "jco", facet.by = "time"
)
bxp

anova_test(
  data = data, dv = osi, wid = iguanaID,
  between = c(diet, lps), within = time
) 



