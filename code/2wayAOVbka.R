# What was the effect of LPS across time-points? 
# Use a repeated measures two way anova to look at the effect within group of phys

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/BKA1LPSlong.csv")
View(gi)
install.packages("tidyverse")
install.packages("rstatix")
library("rstatix")
library("tidyverse")
library("dplyr")

gi$diet <- as.factor(gi$diet)
gi$lps <- as.factor(gi$lps)

GL <- gi %>%
  filter(tx == "G-L") 
gi %>%
  group_by(tx) %>%
  summarise(
    mean(X0430bka, na.rm = TRUE)
  )
boxplot(GL$X0423bka, GL$X0428bka, GL$X0430bka, GL$X0504bka, GL$X0511bka, GL$X0525bka,
        main = "Progression of BKA after first LPS challenge for Glucose-LPS group",
        ylab = "Bacterial Killing",
        xlab = "Time Progression",
        names = c("Pre-LPS", "24hr Post-LPS", "72hr", "1 week", "2 week", "4week"))
res.aov <- anova_test(data = GL, dv = score, wid = id, within = time)
get_anova_table(res.aov)



     