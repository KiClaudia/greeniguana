# Anova to analyze BKA for the secondary immune challenge
gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/BKA2LPSlong.csv")
View(gi)
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
gi$lps <- as.factor(gi$lps)
gi$diet <- as.factor(gi$diet)
str(gi)

#------------RMANOVA---------------
boxplot(data = gi, bka~diet*lps)

BKAaov <- anova_test(
  data = gi, dv = bka, wid = iguanaID,
  between = c(diet, lps), within = time
)
get_anova_table(BKAaov)

gi %>%
  group_by(diet) %>%
  get_summary_stats(bka, type = "mean_sd")

# main effect of diet, 49.1%bka for glucose and 68.7% for water
# can't actually run an anova because the data is bimodal, going to run GLM
#------------bar plot of tx, main effect of diet---------
library(RColorBrewer)
df <- data.frame(gi %>%
                   group_by(tx) %>%
                   get_summary_stats(bka, type = "mean_sd"))
View(df)
head(df)
# save this in case
ggplot(data=df, aes(x=tx, y=mean, fill=tx)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  scale_fill_brewer(palette = 'PuOr')+
  scale_y_continuous(name = "Percent of Bacteria Killed",limits = c(0,100)) +
  scale_x_discrete(name = "Treatment Groups")+
  theme(legend.position = "none") +
  labs(caption="Figure 1. Main effect of diet on bacterial killing activity, Glucose group (49.1%) performed worst than water group (68.7%).") +
  theme(plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0))) +
  geom_text(label = c("a", "a", "b", "b"), aes(y =c(56,58,73,78), x = tx), size = 4)
#actually for the png, I'm going to save just the graph and do the labels separately
png('BKAaov2.png', res=300)
ggplot(data=df, aes(x=tx, y=mean, fill=tx)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  scale_fill_brewer(palette = 'PuOr')+
  scale_y_continuous(limits = c(0,100)) +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_text(label = c("a", "a", "b", "b"), aes(y =c(56,58,73,78), x = tx), size = 4)
dev.off()

#---------GLMM----------
#glucose and LPS are fixed effects, individual is random effect
hist(gi$bka)
#figure out which distribution to use
https://ase.tufts.edu/bugs/guide/assets/mixed_model_guide.html
