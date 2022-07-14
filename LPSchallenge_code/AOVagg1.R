# Look for differences within time and across groups for agglutination data (primary LPS)

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
gi$iguanaID <- as.factor(gi$iguanaID)
gi$tx <- as.factor(gi$tx)
gi$lps <- as.factor(gi$lps)
gi$diet <- as.factor(gi$diet)
str(gi)

data <- gi %>%
  filter(time %in% c("0423agg"))#,"0428agg"))#, "0430agg")) #"0504agg", "0511agg", "0525agg")) # going to use the first few days to simplify data
View(data)
str(data)
hist((data$agg))


# summary stats
data %>%
  group_by(time, diet, lps) %>%
  get_summary_stats(agg, type = "mean_se") %>%
  View()

# data visualization
bxp <- ggboxplot(
  data, x = "lps",  y = "agg",
  color = "diet", palette = "jco", facet.by = "time"
)
bxp

# because the data cannot be transformed to normality and it doesn't fit the gamma distribution, going to go with Kruskal

kruskal.test(data$agg, data$tx)
dunn.test(data$agg, data$tx)


#-----now look at differences between 0423 and 0428----------
df <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(df)

diff <- df %>%
  mutate(agg = X0423agg-X0428agg)
diff <- diff %>%
  select(iguanaID, tx, diet, lps, agg) %>%
  na.omit()
View(diff)  

kruskal.test(diff$agg, diff$diet)

bxp <- ggboxplot(
  diff, x = "lps",  y = "agg",
  color = "diet", palette = "jco"
)
bxp
diff %>%
  group_by(diet, lps) %>%
  get_summary_stats(agg, type = "mean_se") %>%
  View()
#------ now look at differences between 0423 and 0430-------
View(df)
different <- df %>%
  mutate(agg2 = X0423agg-X0430agg)

different <- different %>%
  select(iguanaID, tx, diet, lps, agg2) %>%
  na.omit()
View(different)  

kruskal.test(diet)
bxp <- ggboxplot(
  different, x = "lps",  y = "agg2",
  color = "diet", palette = "jco"
)
bxp
different %>%
  group_by(diet, lps) %>%
  get_summary_stats(agg2, type = "mean_se") %>%
  View()

