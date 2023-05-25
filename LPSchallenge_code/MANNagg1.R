# agglutination LPS 1 has ordinal data, so must use Mann Whitney test

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
  filter(time %in% c("0423agg","0428agg", "0430agg", "0504agg", "0511agg", "0525agg")) 

dataPreLPS <- gi %>%
  filter(time %in% c("0423agg")) #,"0428agg", "0430agg", "0504agg", "0511agg", "0525agg")) 
View(dataPreLPS)
str(dataPreLPS)
hist((dataPreLPS$agg)) #skewed

data24 <- gi %>%
  filter(time %in% c("0428agg"))

data72 <- gi %>%
  filter(time %in% c("0430agg"))

data1wk <- gi %>%
  filter(time %in% c("0504agg"))

data2wk <- gi %>%
  filter(time %in% c("0511agg"))

data4wk <- gi %>%
  filter(time %in% c("0525agg"))

# summary stats
data %>%
  group_by(time, diet, lps) %>%
  get_summary_stats(agg, type = "median_iqr") %>%
  View()


#-----pre-LPS 0423--------------------
preLPSdiet <- wilcox.test(agg ~ diet, data=dataPreLPS, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(preLPSdiet) # p = 0.03356

preLPSimmc <- wilcox.test(agg ~ lps, data=dataPreLPS, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(preLPSimmc) # p = 0.256

#-----24hr post 0428--------------------
diet <- wilcox.test(agg ~ diet, data=data24, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) # p = 0.93

LPS <- wilcox.test(agg ~ lps, data=data24, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS) # p =1097

#-----72hr post 0430--------------------
diet <- wilcox.test(agg ~ diet, data=data72, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) # p = 0.4687

LPS <- wilcox.test(agg ~ lps, data=data72, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS) # p =0.0059

#-----1wk post 0504--------------------
diet <- wilcox.test(agg ~ diet, data=data1wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) # p = 0.2287

LPS <- wilcox.test(agg ~ lps, data=data1wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS) # p =0.002159

#-----2wk post 0511--------------------
diet <- wilcox.test(agg ~ diet, data=data2wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) # p = 0.4425

LPS <- wilcox.test(agg ~ lps, data=data2wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS) # p =0.0191

#-----4wk post 0525--------------------
diet <- wilcox.test(agg ~ diet, data=data4wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) # p = 0.039

LPS <- wilcox.test(agg ~ lps, data=data4wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS) # p =0.07988

#-----now look at differences between 0423 and 0428 (24hr)----------
df <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(df)

diff <- df %>%
  mutate(agg = X0423agg-X0428agg)
diff <- diff %>%
  select(iguanaID, tx, diet, lps, agg) %>%
  na.omit()
View(diff)  

bxp <- ggboxplot(
  diff, x = "lps",  y = "agg",
  color = "diet", palette = "jco"
)
bxp

diff %>%
  group_by(diet, lps) %>%
  get_summary_stats(agg, type = "mean_se") %>%
  View()

diet <- wilcox.test(agg ~ diet, data=diff, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) # 0.0289

lps <- wilcox.test(agg ~ lps, data=diff, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(lps) # 0.8197

#------ now look at differences between 0423 and 0430-------
View(df)
different <- df %>%
  mutate(agg2 = X0423agg-X0430agg)

different <- different %>%
  select(iguanaID, tx, diet, lps, agg2) %>%
  na.omit()
View(different)  

bxp <- ggboxplot(
  different, x = "lps",  y = "agg2",
  color = "diet", palette = "jco"
)
bxp

different %>%
  group_by(diet, lps) %>%
  get_summary_stats(agg2, type = "mean_se") %>%
  View()

diet <- wilcox.test(agg2 ~ diet, data=different, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) # 0.0738

lps <- wilcox.test(agg2 ~ lps, data=different, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(lps) # 0.266

#------line plot to show diet and lps significance (independent no interaction) thru time-----
df <- data.frame(data %>%
                   group_by(diet,time) %>%
                   get_summary_stats(agg, type = "mean_se"))
View(df)
head(df)
ggplot(data=df, aes(x=time, y=mean, group = diet)) +
  geom_line(aes(color = diet))+
  geom_point(aes(color = diet))+
  scale_y_continuous(limits = c(0,4)) +
  ylab("Agglutination Scores") +
  xlab("LPS 1 Timeline") +
  labs(title = "Agglutination scores are doubled in sugar groups Pre-LPS and at 4wks \n post-LPS as compared to the control diet groups. The change between pre-LPS and 24 hr \n and pre-LPS and 72 hr is also different between glucose and control diet group. \n From the graph, we can see how the agglutination scores have a 2 fold reduction following \n the LPS treatment in the sugar group as compared to the contorl diet group.")+
  scale_x_discrete(labels = c("PreLPS", "24h", "72h","1w", "2w", "4w")) +
  annotate("text", x=1, y=3.25, label = "*")  +
  annotate("text", x=6, y=3.25, label = "*")


df2 <- data.frame(data %>%
                   group_by(lps,time) %>%
                   get_summary_stats(agg, type = "mean_se"))

ggplot(data=df2, aes(x=time, y=mean, group = lps)) +
  geom_line(aes(color = lps))+
  geom_point(aes(color = lps))+
  scale_y_continuous(limits = c(0,4)) +
  scale_x_discrete(labels = c("B", "24h", "72h","1w", "2w", "4w")) +
  ylab("Agglutination Scores") +
  xlab("LPS 1 Timeline") +
  labs(title = "Aggultination scores are higher in the LPS groups than the \n control groups starting at 72hr continuing through week 4")+
  annotate("text", x=3, y=3.25, label = "**")  +
  annotate("text", x=4, y=3.25, label = "**") +
  annotate("text", x=5, y=3.25, label = "*")  +
  annotate("text", x=6, y=3.25, label = "p = 0.07")


