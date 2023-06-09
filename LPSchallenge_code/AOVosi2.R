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

hist((data$oi)) #data is normal

data %>%
  group_by(lps) %>%
  get_summary_stats(oi, type = "mean_sd")

bxp <- ggboxplot(
  data, x = "diet",  y = "si", color = "lps", palette = "jco", facet.by = "time"
)
bxp

aov <- anova_test(
  data = data, dv = oi, wid = iguanaID,
  between = c(diet, lps), within = time
) 
aov
simpleMainEffect <- data %>%
  group_by(time) %>%
  anova_test(dv = oi, wid = iguanaID, between = lps) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "BH")
View(simpleMainEffect)

simplepairwise <- data %>%
  group_by(time) %>%
  pairwise_t_test(oi ~ lps, p.adjust.method = "BH")
View(simplepairwise)
# same as anova above because there is only two levels to LPS, this would be helpful if LPS had 2+ levels cuz it tells you 
# where the difference is between levels in LPS (or factor A) for every level of time (factor b)


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

#-------bar plot-----------
df <- data.frame(data %>%
                   group_by(tx) %>%
                   get_summary_stats(osi, type = "mean_sd"))
head(df)
ggplot(data=df, aes(x=tx, y=mean)) +
  geom_bar(stat="identity") +
  scale_y_continuous(name = "OSI",limits = c(-1,1)) +
  labs(title = "Main effect of diet and LPS on OSI")
