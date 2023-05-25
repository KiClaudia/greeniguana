# Lysis LPS 1 has ordinal data, so must use Mann Whitney test

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/LysLong.csv")
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
  filter(time %in% c("0423lys", "0428lys", "0430lys", "0504lys", "0511lys","0525lys")) 

hist((data$lys)) #skewed

datapre <- gi %>%
  filter(time %in% c("0423lys"))

data24 <- gi %>%
  filter(time %in% c("0428lys"))

data72 <- gi %>%
  filter(time %in% c("0430lys"))

data1wk <- gi %>%
  filter(time %in% c("0504lys"))

data2wk <- gi %>%
  filter(time %in% c("0511lys"))

data4wk <- gi %>%
  filter(time %in% c("0525lys"))

# summary stats
data %>%
  group_by(time, diet, lps) %>%
  get_summary_stats(lys, type = "median_iqr") %>%
  View()


#-----pre-LPS 0423--------------------
preLPSdiet <- wilcox.test(lys ~ diet, data=datapre, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(preLPSdiet)

preLPSimmc <- wilcox.test(lys ~ lps, data=datapre, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(preLPSimmc) 

#-----24hr post 0428--------------------
diet <- wilcox.test(lys ~ diet, data=data24, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) 

LPS <- wilcox.test(lys ~ lps, data=data24, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS)   

#-----72hr post 0430--------------------
diet <- wilcox.test(lys ~ diet, data=data72, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet)    

LPS <- wilcox.test(lys ~ lps, data=data72, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS)  

#-----1wk post 0504--------------------
diet <- wilcox.test(lys ~ diet, data=data1wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet)  

LPS <- wilcox.test(lys ~ lps, data=data1wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS)  

#-----2wk post 0511--------------------
diet <- wilcox.test(lys ~ diet, data=data2wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) 

LPS <- wilcox.test(lys ~ lps, data=data2wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS) 

#-----4wk post 0525--------------------
diet <- wilcox.test(lys ~ diet, data=data4wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet)  

LPS <- wilcox.test(lys ~ lps, data=data4wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS)   

#-----now look at differences between 0423 and 0428 (24hr)----------
df <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(df)

diff <- df %>%
  mutate(lys = X0423lys-X0428lys)
diff <- diff %>%
  select(iguanaID, tx, diet, lps, lys) %>%
  na.omit()
View(diff)  


diet <- wilcox.test(lys ~ diet, data=diff, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) 

lps <- wilcox.test(lys ~ lps, data=diff, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(lps)  

#------ now look at differences between 0423 and 0430-------
View(df)
different <- df %>%
  mutate(lys2 = X0423lys-X0430lys)

different <- different %>%
  select(iguanaID, tx, diet, lps, lys2) %>%
  na.omit()
View(different)  

diet <- wilcox.test(lys2 ~ diet, data=different, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet)  

lps <- wilcox.test(lys2 ~ lps, data=different, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(lps)  

#------line plot to show diet significance change 24hr-----
df <- data.frame(data %>%
                   group_by(diet,time) %>%
                   get_summary_stats(lys, type = "mean_se"))
View(df)
head(df)
ggplot(data=df, aes(x=time, y=mean, group = diet)) +
  geom_line(aes(color = diet))+
  geom_point(aes(color = diet))+
  scale_y_continuous(limits = c(0,4)) +
  ylab("Lysis Scores") +
  xlab("LPS 1 Timeline") +
  scale_x_discrete(labels = c("PreLPS", "24h", "72h","1w", "2w", "4w")) +
  annotate("text", x=1.5, y=3.5, label = "*")




