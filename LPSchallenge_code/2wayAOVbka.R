# What was the effect of LPS across time-points? 
# this documents purpose has since been updatd, check readme for latest updates

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/BKA1LPSlong.csv")
View(gi)
install.packages("tidyverse")
install.packages("rstatix")
install.packages("ggpubr")
library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")

gi$diet <- as.factor(gi$diet)
gi$lps <- as.factor(gi$lps)
gi$time <- as.factor(gi$time)
gi$iguanaID <- as.factor(gi$iguanaID)
str(gi)

GL <- gi %>%
  filter(tx == "G-L") #filter the treatment groups out so we can look at summary stats
GC <- gi %>%
  filter(tx == "G-C")
WL<- gi %>%
  filter(tx == "W-L")
WC<- gi %>%
  filter(tx == "W-C")

GL %>%
  group_by(time) %>%
  summarise(mean(bka, na.rm = TRUE)
  )
GC %>%
  group_by(time) %>%
  summarise(mean(bka, na.rm = TRUE)
  )
WL %>%
  group_by(time) %>%
  summarise(mean(bka, na.rm = TRUE)
  )
WC %>%
  group_by(time) %>%
  summarise(mean(bka, na.rm = TRUE)
  )

boxplot(GL$bka ~ GL$time) # visualize the data
boxplot(GC$bka ~ GC$time)
boxplot(WL$bka ~ WL$time)
boxplot(WC$bka ~ WC$time)

# ----Use a one way anova to check for main effect of time by comparing each group to itself across time----
GLaov <- anova_test(data = GL, dv = bka, wid = iguanaID, within = time)
get_anova_table(GLaov) 

GLpwc <- GL %>% # pair-wise comparison
  pairwise_t_test(
    bka ~ time, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
GLpwc #for GL, 24hr post increased after injection but then went back to baseline levels at 72 hr, 1 wk, 2 wk and 4 wk

GCaov <- anova_test(data = GC, dv = bka, wid = iguanaID, within = time)
get_anova_table(GCaov) 

GCpwc <- GC %>% # pair-wise comparison
  pairwise_t_test(
    bka ~ time, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
GCpwc # spike 24hr post injection but back to baseline at 72hr and BKA plummets at 1 week to below baseline and then back to baseline 2 wk and 4 wk

WLaov <- anova_test(data = WL, dv = bka, wid = iguanaID, within = time)
get_anova_table(WLaov) 

WLpwc <- WL %>% # pair-wise comparison
  pairwise_t_test(
    bka ~ time, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
WLpwc #spike 24hr post, back to baseline at 72, 1wk, 2 wk, 4 wk

WCaov <- anova_test(data = WC, dv = bka, wid = iguanaID, within = time)
get_anova_table(WCaov) 

WCpwc <- WC %>% # pair-wise comparison
  pairwise_t_test(
    bka ~ time, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
WCpwc #spike 24hr post, back to baseline at 72, 1wk, 2 wk, 4 wk but at 72 hr its not different from baseline or 24 hr
# basically at 72 hr its between baseline and spike and not different from either. 

# WC's spke is the most mild at ~33% change as opposed to the others which is about 40-50%



#----- Use a one way anova to check for main effect of treatment group by comparing across group at specified times or changes in two specified times----

#  look at change between 0423 and 0428, 0428 0430, 0423 0504

# make a new dataframe with BKA values for specific time frames or change in time frame and has all treatment groups

full <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
# note that we don't use long data for this because it is not repeated measures
change <- full %>% 
  group_by(iguanaID) %>%
  mutate(change = X0504bka -X0430bka )
View(change)

data <- change %>%
  group_by(iguanaID) %>%
  select(change, tx, iguanaID)
View(data)
head(data)
data$tx <- as.factor(data$tx)

data %>%
  group_by(tx) %>%
  summarise(
    count = n(),
    mean = mean(change, na.rm = TRUE),
    sd = sd(change, na.rm = TRUE)
  )

boxplot(change ~ tx, data)

aov <-aov(change ~ tx, data)
summary(aov)

# no difference across groups for change between pre LPS and 24 hour post (0428-0423)
# no difference across groups for change between pre LPS and 72 hour post (0430-0423)
# no difference across groups for change between 24 hour post and 72 hour post (0430-0428)
# no difference across groups for change between 24 hour post and 1 week post (0504-0423)
# no difference across groups for change between 72 hour post and 1 week post (0504-0430)

#----- what if we look at the values at each time without doing changes? ----
full <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")

dat <- full %>%
  select(X0423bka, X0428bka, X0430bka, X0504bka, tx, iguanaID)
View(dat)
head(dat)
dat$tx <- as.factor(dat$tx)

dat %>%
  group_by(tx) %>%
  select(X0504bka, tx, iguanaID) %>%
  summarise(
    count = n(),
    mean = mean(X0504bka, na.rm = TRUE),
    sd = sd(X0504bka, na.rm = TRUE)
  )
boxplot(X0504bka ~ tx, dat)
aov <-aov(X0504bka ~ tx, dat)
summary(aov)

# X0423bka no diff across groups
# X0428bka no diff across groups
# X0430bka no diff across groups
# X0504bka no diff across groups (but close p = 0.055)

#--------it's weird because it looks like there is a difference from summary tables...I'm going to remove outliers-----
baseline <- full %>%
  select(X0423bka, iguanaID, tx) 

baseline %>%
  group_by(tx) %>%
  identify_outliers(X0423bka) #I will remove extreme outliers which are above or below Q1 and Q3. 

baseline <- baseline %>%
  filter(X0423bka > 55.1)
View(baseline) #baseline has no outliers

baseline %>%
  group_by(tx) %>%
  summarise(
    count = n(),
    mean = mean(X0423bka, na.rm = TRUE),
    sd = sd(X0423bka, na.rm = TRUE)
  )
boxplot(X0423bka ~ tx, baseline)
aov <- aov(X0423bka ~ tx, baseline)
summary(aov) # not significant

post24 <- full %>%
  select(X0428bka, iguanaID, tx)
post24 %>%
  group_by(tx) %>%
  identify_outliers(X0428bka) # no outliers
boxplot(X0428bka ~ tx, post24)
aov <- aov(X0428bka ~ tx, post24)
summary(aov) # not significant

post72 <- full %>%
  select(X0430bka, iguanaID, tx)
post72 %>%
  group_by(tx) %>%
  identify_outliers(X0430bka) 
post72 <- post72 %>%
  filter(X0430bka > 7)
View(post72)
boxplot(X0430bka ~ tx, post72)
aov <- aov(X0430bka ~ tx, post72)
summary(aov) # not significant

# so taking out the outliers did not change anything. 

# -------2 way aov------------
# instead of doing a one way anova and looking at time and treatment separately,
# going to do it together using a two way anova rm
# the two variables will be diet (G or W) and LPS (LPS or control)

View(gi) #in long format with all my variables
longdata <- gi %>%
  filter(time %in% c("0423bka", "0428bka", "0430bka", "0504bka", "0511bka")) 

anova_test( # mixed model
  data = longdata, dv = bka, wid = iguanaID,
  between = c(diet, lps), within = time
) # effect of diet and LPS

widedata <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
wide <- widedata %>%
  mutate(change = X0430bka - X0428bka) %>%
  select(iguanaID, change, tx, diet, lps) %>%
  na.omit()
View(wide)

wide %>% anova_test(change ~ diet*lps) # two way, time account for by subtraction
boxplot(change ~ diet*lps, data = wide)

get_anova_table(res.aov)


