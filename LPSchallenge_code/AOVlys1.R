# look for differences amongst groups and time for lysis first challenge
gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/LysLong.csv")
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
  filter(time %in% c("0423lys", "0428lys", "0430lys", "0504lys", "0511lys","0525lys")) 
View(data)
hist(sqrt(data$lys)) #use sqrt to make data normal
data$lys<- sqrt(data$lys)
hist(data$lys)

data %>%
  group_by(time) %>%
  get_summary_stats(lys, type = "mean_sd")

bxp <- ggboxplot(
  data, x = "lps",  y = "lys", color = "diet",
  palette = "jco", facet.by = "time"
)
bxp

anova_test(
  data = data, dv = lys, wid = iguanaID,
  between = c(diet, lps), within = time
)
data %>%
  pairwise_t_test(
    lys ~ time, paired = FALSE, 
    p.adjust.method = "none"
  )
# sig effect of time only with baseline being different from all others
