# Lysis LPS 2 has ordinal data, so must use Mann Whitney test

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
  filter(time %in% c("0525lys","0528lys", "0530lys", "0603lys", "0610lys","0624lys")) 

hist((data$lys)) #skewed

datapre <- gi %>%
  filter(time %in% c("0525lys"))

data24 <- gi %>%
  filter(time %in% c("0528lys"))

data72 <- gi %>%
  filter(time %in% c("0530lys"))

data1wk <- gi %>%
  filter(time %in% c("0603lys"))

data2wk <- gi %>%
  filter(time %in% c("0610lys"))

data4wk <- gi %>%
  filter(time %in% c("0624lys"))

# summary stats
data %>%
  group_by(time, lps) %>%
  get_summary_stats(lys, type = "median_iqr") %>%
  View()


#-----pre-LPS --------------------
preLPSdiet <- wilcox.test(lys ~ diet, data=datapre, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(preLPSdiet)

preLPSimmc <- wilcox.test(lys ~ lps, data=datapre, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(preLPSimmc) 

#-----24hr post --------------------
diet <- wilcox.test(lys ~ diet, data=data24, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) 

LPS <- wilcox.test(lys ~ lps, data=data24, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS)   

#-----72hr post --------------------
diet <- wilcox.test(lys ~ diet, data=data72, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet)    

LPS <- wilcox.test(lys ~ lps, data=data72, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS)  

#-----1wk post --------------------
diet <- wilcox.test(lys ~ diet, data=data1wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet)  

LPS <- wilcox.test(lys ~ lps, data=data1wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS)  

#-----2wk post --------------------
diet <- wilcox.test(lys ~ diet, data=data2wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet) 

LPS <- wilcox.test(lys ~ lps, data=data2wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS) 

#-----4wk post --------------------
diet <- wilcox.test(lys ~ diet, data=data4wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(diet)  

LPS <- wilcox.test(lys ~ lps, data=data4wk, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(LPS)   

#-----now look at differences (24hr)----------
df <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(df)

diff <- df %>%
  mutate(lys = X0525lys-X0528lys)
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
  mutate(lys2 = X0525lys-X0530lys)

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
p2 <- ggplot(data=df2, aes(x=time, y=mean, group = diet)) +
  geom_line(aes(color = diet))+
  geom_point(aes(color = diet))+
  scale_y_continuous(limits = c(0,4)) +
  ylab("Lysis Scores") +
  xlab("LPS 2 Timeline") +
  scale_x_discrete(labels = c("PreLPS", "24h", "72h","1w", "2w", "4w")) +
  annotate("text", x=4, y=3.5, label = "**")


df4 <- data.frame(data2 %>%
                    group_by(lps,time) %>%
                    get_summary_stats(lys, type = "mean_se"))
p4 <- ggplot(data=df4, aes(x=time, y=mean, group = lps)) +
  geom_line(aes(color = lps))+
  geom_point(aes(color = lps))+
  scale_y_continuous(limits = c(0,4)) +
  ylab("Lysis Scores") +
  xlab("LPS 2 Timeline") +
  scale_x_discrete(labels = c("PreLPS", "24h", "72h","1w", "2w", "4w")) +
  annotate("text", x=2, y=3.5, label = "*") +
  annotate("text", x=3, y=3.5, label = "*") +
  annotate("text", x=4, y=3.5, label = "*")


nested <- (((p3|p4)/(p1|p2)))+
  plot_annotation(tag_levels = 'A')
nested
library(patchwork)




