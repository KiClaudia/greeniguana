# Look for differences within time and across groups for drom data (secondary LPS)

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/dromLong.csv")
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
  filter(time %in% c("0525dromstd","0528dromstd", "0530dromstd", "0603dromstd", "0610dromstd","0624dromstd")) 
View(data)
hist((data$drom)) #data is normal

anova_test(
  data = data, dv = drom, wid = iguanaID,
  between = c(diet, lps), within = time) 

data %>%
  group_by(diet,time) %>%
  get_summary_stats(drom, type = "mean_sd")

bxp <- ggboxplot(
  data, x = "lps",  y = "drom", palette = "npg ")
bxp

diettime <- data %>%
  group_by(diet) %>%
  pairwise_t_test(
    drom ~ time, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
View(diettime)

# main effect of lps and diet (after transforming data)
# interaction effect of diet:time
# backtransformed to get averages

#-----------line plot main effect diet and lps----------
df <- data.frame(data %>%
                   group_by(diet, time) %>%
                   get_summary_stats(drom, type = "mean_sd"))
View(df)
head(df)
diet <- ggplot(data=df, aes(x=time, y=mean, group = diet)) +
  geom_line(aes(color = diet))+
  geom_point(aes(color = diet))+
  scale_y_continuous(name = "dROM") +
  scale_x_discrete(name = "Time course", labels = c("Baseline", "24hr", "72hr","1 week", "2 week", "4 week")) +
  labs(title = "Main effect of diet on dROM")
diet

df2 <- data.frame(data %>%
                    group_by(lps, time) %>%
                    get_summary_stats(drom, type = "mean_sd"))
View(df2)
head(df2)
lps <- ggplot(data=df2, aes(x=time, y=mean, group = lps)) +
  geom_line(aes(color = lps))+
  geom_point(aes(color = lps))+
  scale_y_continuous(name = "dROM") +
  scale_x_discrete(name = "Time course", labels = c("Baseline", "24hr", "72hr","1 week", "2 week", "4 week")) +
  labs(title = "Main effect of LPS treatment on dROM")
lps


#---------bar plot------------
df <- data.frame(data %>%
                   group_by(tx) %>%
                   get_summary_stats(drom, type = "mean_sd"))
head(df)

png('dROMaov2.png', res=300)
ggplot(data=df, aes(x=tx, y=mean, fill=tx)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = 'PuOr')+
  scale_y_continuous(name = "dROM",limits = c(-0.6,0.6)) +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_text(size = 3, label = c("ac", "ad", "bc", "bd"), y = c(.07,-.07,.07,.07))
                                                    
dev.off()
