# agglutination LPS 2 has ordinal data, so must use Mann Whitney test

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/AggLong.csv")
View(gi) #note that all NA rows were omitted automatically during the transposal
install.packages("tidyverse")
install.packages("rstatix")
install.packages("ggpubr")
install.packages("dunn.test")
library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library("dunn.test")
str(gi)
gi$time <- as.factor(gi$time)
gi$iguanaID <- as.character(gi$iguanaID)
gi$tx <- as.factor(gi$tx)
gi$lps <- as.factor(gi$lps)
gi$diet <- as.factor(gi$diet)
str(gi)

data <- gi %>%
  filter(time %in% c("0525agg", "0528agg", "0530agg", "0603agg", "0610agg","0624agg")) 

hist(data$agg) # skewed

datapre <- gi %>%
  filter(time %in% c("0525agg"))

data24 <- gi %>%
  filter(time %in% c("0528agg"))

data72 <- gi %>%
  filter(time %in% c("0530agg"))

data1wk <- gi %>%
  filter(time %in% c("0603agg"))

data2wk <- gi %>%
  filter(time %in% c("0610agg"))

data4wk <- gi %>%
  filter(time %in% c("0624agg"))

# summary stats
data %>%
  group_by(time, diet, lps) %>%
  get_summary_stats(agg, type = "median_iqr") %>%
  View()


#-----pre-LPS 0525--------------------
preLPSdiet <- wilcox.test(agg ~ diet, data=datapre, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(preLPSdiet)

preLPSimmc <- wilcox.test(agg ~ lps, data=datapre, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(preLPSimmc) 

#-----24hr post --------------------
diet <- wilcox.test(agg ~ diet, data=data24, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) 

LPS <- wilcox.test(agg ~ lps, data=data24, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS) 

#-----72hr post --------------------
diet <- wilcox.test(agg ~ diet, data=data72, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) 

LPS <- wilcox.test(agg ~ lps, data=data72, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS) 

#-----1wk post -------------------
diet <- wilcox.test(agg ~ diet, data=data1wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) 

LPS <- wilcox.test(agg ~ lps, data=data1wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS) 

#-----2wk post --------------------
diet <- wilcox.test(agg ~ diet, data=data2wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet

LPS <- wilcox.test(agg ~ lps, data=data2wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS) 

#-----4wk post --------------------
diet <- wilcox.test(agg ~ diet, data=data4wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) 

LPS <- wilcox.test(agg ~ lps, data=data4wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS) 

#-----now look at differences between 0525 and 0528 (24hr)----------
df <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(df)

diff <- df %>%
  mutate(agg = X0525agg-X0528agg)
diff <- diff %>%
  select(iguanaID, tx, diet, lps, agg) %>%
  na.omit()
View(diff)  

diff %>%
  group_by(diet, lps) %>%
  get_summary_stats(agg, type = "mean_se") %>%
  View()

diet <- wilcox.test(agg ~ diet, data=diff, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet)

lps <- wilcox.test(agg ~ lps, data=diff, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(lps) 

#------ now look at differences between 0525 and 0530-------
View(df)
different <- df %>%
  mutate(agg2 = X0525agg-X0530agg)

different <- different %>%
  select(iguanaID, tx, diet, lps, agg2) %>%
  na.omit()
View(different)  


different %>%
  group_by(diet, lps) %>%
  get_summary_stats(agg2, type = "mean_se") %>%
  View()

diet <- wilcox.test(agg2 ~ diet, data=different, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) 

lps <- wilcox.test(agg2 ~ lps, data=different, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(lps) 

#------line plot to show diet and lps significance (independent no interaction) thru time-----
df <- data.frame(data %>%
                   group_by(diet,time) %>%
                   get_summary_stats(agg, type = "mean_se"))
View(df)
head(df)
p3 <- ggplot(data=df, aes(x=time, y=mean, group = diet)) +
  geom_line(aes(color = diet))+
  geom_point(aes(color = diet))+
  scale_y_continuous(limits = c(0,4)) +
  ylab("Agglutination Scores") +
  xlab("LPS 2 Timeline") +
  scale_x_discrete(labels = c("PreLPS", "24h", "72h","1w", "2w", "4w")) +
  annotate("text", x=1, y=3.25, label = "*") 


df2 <- data.frame(data %>%
                    group_by(lps,time) %>%
                    get_summary_stats(agg, type = "mean_se"))

p4 <- ggplot(data=df2, aes(x=time, y=mean, group = lps)) +
  geom_line(aes(color = lps))+
  geom_point(aes(color = lps))+
  scale_y_continuous(limits = c(0,5)) +
  scale_x_discrete(labels = c("B", "24h", "72h","1w", "2w", "4w")) +
  ylab("Agglutination Scores") +
  xlab("LPS 2 Timeline") +
  annotate("text", x=1, y=4.5, label = "p = 0.07")  +
  annotate("text", x=2, y=4.5, label = "**") +
  annotate("text", x=3, y=4.5, label = "***")  +
  annotate("text", x=4, y=4.5, label = "*") +
  annotate("text", x=5, y=4.5, label = "*")  +
  annotate("text", x=6, y=4.5, label = "*")

nested <- (((p2|p4)/(p1|p3)))+
  plot_annotation(tag_levels = 'A')
nested
library(patchwork) # p1 i got from mannagg1
