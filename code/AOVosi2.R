# Look for differences within time and across groups for OSI data (secondary LPS)

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
  filter(time %in% c("0525oi","0528oi", "0530oi", "0603oi", "0610oi","0624oi")) 
View(data)

#-----RMaov------------
hist((data$osi)) #data is normal

data %>%
  group_by(diet) %>%
  get_summary_stats(osi, type = "mean_sd")

bxp <- ggboxplot(
  data, x = "diet",  y = "osi", color = "lps", palette = "jco"
)
bxp

aov <- anova_test(
  data = data, dv = osi, wid = iguanaID,
  between = c(diet, lps), within = time
) 
lpstime <- data %>%
  group_by(lps) %>%
  pairwise_t_test(
    osi ~ time, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
View(lpstime)

# main effect of lps and diet (after transforming data)
# interaction effect of LPS:time
# backtransformed to get averages
# lps - LPS is 2.40, control is 1.38
# LPS:time --> control 0603, 0624 -->meh


#------Baseline-72hour and 24hr and 1wk 4 wk-------------
datashort <- gi %>%
  filter(time %in% c("0602oi","0610oi", "0624oi"))
View(datashort)

datashort %>%
  group_by(time) %>%
  get_summary_stats(osi, type = "mean_sd")

ggboxplot(
  datashort, x = "time",  y = "lys",
  color = "diet", palette = "jco"
)
hist((datashort$osi)) # normal

anova_test(
  data = datashort, dv = osi, wid = iguanaID,
  between = c(diet, lps), within = time)
# Same effect found for both as the whole model

#------ line plot-------------

df <- data.frame(data %>%
                   group_by(diet, time) %>%
                   get_summary_stats(osi, type = "mean_sd"))
View(df)
head(df)
diet <- ggplot(data=df, aes(x=time, y=mean, group = diet)) +
  geom_line(aes(color = diet))+
  geom_point(aes(color = diet))+
  scale_y_continuous(name = "Oxidative Stress Index", breaks = seq(-1,1,0.25)) +
  scale_x_discrete(name = "Time course", labels = c("Baseline", "24hr", "72hr","1 week", "2 week", "4 week")) +
  labs(title = "Main effect of diet on OSI")
diet

df2 <- data.frame(data %>%
                   group_by(lps, time) %>%
                   get_summary_stats(osi, type = "mean_sd"))
View(df2)
head(df2)
lps <- ggplot(data=df2, aes(x=time, y=mean, group = lps)) +
  geom_line(aes(color = lps))+
  geom_point(aes(color = lps))+
  scale_y_continuous(name = "Oxidative Stress Index", breaks = seq(-1,1,0.25)) +
  scale_x_discrete(name = "Time course", labels = c("Baseline", "24hr", "72hr","1 week", "2 week", "4 week")) +
  labs(title = "Main effect of LPS treatment on OSI")
lps
