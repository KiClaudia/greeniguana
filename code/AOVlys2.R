# Look for differences within time and across groups for lysis data (secondary LPS)

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/modData/LysLong.csv")
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
  filter(time %in% c("0525lys","0528lys", "0530lys", "0603lys", "0610lys","0624lys")) 
View(data)
hist(sqrt(data$lys)) #use sqrt to make data normal
data$lys<- sqrt(data$lys)
hist(data$lys)

data %>%
  group_by(time) %>%
  get_summary_stats(lys, type = "mean_sd")

bxp <- ggboxplot(
  data, x = "lps",  y = "lys",
   palette = "jco"
)
bxp

anova_test(
  data = data, dv = lys, wid = iguanaID,
  between = c(diet, lps), within = time
)

# main effect of lps and time (after transforming data)
# backtransformed to get averages
# lps - LPS is 2.40, control is 1.38
# time - not actually significant in pairwise

data %>%
  pairwise_t_test(
    lys ~ time, paired = FALSE, 
    p.adjust.method = "none"
  )

#------ Baseline-72hour and 1week-4week------------
datashort <- gi %>%
  filter(time %in% c("0525lys","0528lys", "0530lys"))
View(datashort)

datashort %>%
  group_by(time) %>%
  get_summary_stats(lys, type = "mean_sd")

ggboxplot(
  datashort, x = "time",  y = "lys",
  color = "diet", palette = "jco"
)
hist(sqrt(datashort$lys)) #sqrt for normality
datashort$lys <- sqrt(datashort$lys)
hist(datashort$lys)
anova_test(
  data = datashort, dv = lys, wid = iguanaID,
  between = c(diet, lps), within = time)
# Same effect found for both as the whole model

#---------line plot for main effect lps----------
df <- data.frame(data %>% group_by(time,lps) %>%
                   get_summary_stats(lys, type = "mean_sd"))
View(df)
head(df)

png('Lyslps2.png', res=300)
ggplot(data=df, aes(x=time, y=mean, group = lps)) +
  geom_line(aes(color = lps))+
  geom_point(aes(color = lps))+
  scale_color_manual(values=c("darkorchid4", "springgreen4")) +
  scale_y_continuous(limits = c(0,4)) +
  scale_x_discrete(labels = c("B", "24h", "72h","1w", "2w", "4w")) +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) 
dev.off()

#--------bar plot-----
df <- data.frame(data %>%
                   group_by(tx) %>%
                   get_summary_stats(lys, type = "mean_sd"))
head(df)
ggplot(data=df, aes(x=tx, y=mean)) +
  geom_bar(stat="identity") +
  scale_y_continuous(name = "Lysis",limits = c(0,5)) +
  labs(title = "Main effect of LPS on lysis")
