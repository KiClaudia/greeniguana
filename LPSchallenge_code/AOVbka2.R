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
library("betareg")
library("glmmTMB")
str(gi)
gi$time <- as.factor(gi$time)
gi$iguanaID <- as.factor(gi$iguanaID)
gi$lps <- as.factor(gi$lps)
gi$diet <- as.factor(gi$diet)
str(gi)

hist((gi$bka)) # need beta regression, not normal

ggboxplot(
  gi, x = "diet",  y = "bka", color = "lps", 
  palette = "jco", facet.by = "time")

gi %>%
  group_by(diet) %>%
  get_summary_stats(bka, type = "mean_se")

gi <- gi %>%
  mutate(data = bka/100) # change to decimal 
View(gi)

# betareg needs to be between 0 and 1 but not 0 and 1, so the package suggests you
# do this (y * (n-1) + 0.5) / n where n is the sample size.
# n = 36 

gi <- gi %>%
  mutate(beta = ((data*35)+0.5)/36)
View(gi)
range(gi$beta) # range is good

# which of the two beta models work better? they are the same
glmm <- glmmTMB(beta ~ lps* diet * time + (1|iguanaID), data = gi, (family = beta_family(link = "logit")))
summary(glmm)

betamodel <- betareg::betareg(gi$beta ~ gi$lps * gi$time * gi$diet)
summary(betamodel)

# Look at diet:time
dietmodel <- glmmTMB(beta ~ diet * time + (1|iguanaID), data = gi, (family = beta_family(link = "logit")))
summary(dietmodel)

# Look at LPS:time
lpsmodel <- glmmTMB(beta ~ lps * time + (1|iguanaID), data = gi, (family = beta_family(link = "logit")))
summary(lpsmodel)

# Look at diet
jdietmodel <- glmmTMB(beta ~ diet + (1|iguanaID), data = gi, (family = beta_family(link = "logit")))
summary(jdietmodel)

# Look at lps
jlpsmodel <- glmmTMB(beta ~ lps + (1|iguanaID), data = gi, (family = beta_family(link = "logit")))
summary(jlpsmodel)

# Look at time
timemodel <- jlpsmodel <- glmmTMB(beta ~ time + (1|iguanaID), data = gi, (family = beta_family(link = "logit")))
summary(timemodel)

# There may be too many time points, let's do just the first 3
gi2 <- gi %>%
  group_by(lps, diet) %>%
  filter(time == c("0525bka", "0528bka", "0530bka")) 

shortmod <- glmmTMB(beta ~ diet * lps * time + (1|iguanaID), data = gi2, (family = beta_family(link = "logit")))
summary(shortmod) #nada

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
ggplot(data=data, aes(x=tx, y=mean, fill=tx)) +
  geom_bar(stat="identity") +
  labs(fill = "Legend") +
  scale_fill_brewer(palette = 'PuOr', labels=c("Glucose/Control","Glucose/LPS","Water/Control","Water/LPS"))+
  scale_y_continuous(limits = c(0,100)) +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_text(size = 3, label = c("a", "a", "b", "b"), position = position_stack(vjust = 1.2))
dev.off()



#---look for time effects by doing change, NOTHING SIGNIFICNT-----------
#look for time effects by subtracting 24hr-baseline
widedata <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(widedata)
base24 <- widedata %>% 
  mutate(changeintime = X0528bka -X0525bka )
View(base24)
boxplot(data = base24, changeintime~tx)
kruskal.test(changeintime ~tx, data = base24) #p=0.2395 for 24hr-baseline

base72 <- widedata %>% 
  mutate(changeintime = X0530bka -X0525bka )
View(base72)
boxplot(data = base72, changeintime~tx)
kruskal.test(changeintime ~tx, data = base72) #p=0.5768 for 72hr-baseline

post2472 <- widedata %>% 
  mutate(changeintime = X0530bka -X0528bka )
View(post2472)
boxplot(data = post2472, changeintime~tx)
kruskal.test(changeintime ~tx, data = post2472) #p=0.07556 for 72hr-24hr

week1hr24 <- widedata %>% 
  mutate(changeintime = X0603bka -X0528bka )
View(week1hr24)
boxplot(data = week1hr24, changeintime~tx)
kruskal.test(changeintime ~tx, data = week1hr24) #p=0.5276 for 1 week-24hr

#nothing significant when looking at time 

