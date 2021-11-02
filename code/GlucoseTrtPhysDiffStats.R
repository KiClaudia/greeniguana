# 1) Was there an effect of glucose (techincally dexstrose) treatment within group?
# 2) Was there an effect of glucose across groups?
# I will use a paired t.test to compare within groups for treatment effects and then t.test to compare change and put it in a table 
# there will be a separate table for each question

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(gi)
install.packages("tidyverse")
library("tidyverse")
library("dplyr")

str(gi)
gi$tx <- as.factor(gi$tx) # anovas needs the groups to be factors
str(gi)


glucose <- gi %>%
  filter(diet == "g") 
water <- gi %>%
  filter(diet == "w") 

# Use histogram to check normality of data
hist(glucose$X0324oxystd)
hist(glucose$X0423oxystd)
hist(water$X0324oxystd)
hist(water$X0423oxystd)

# Normal: agg, lys, mass, glucose, glycerol, totri, trtri, oi, dromstd, oxystd
# Non-parametric: BKA
se <- function(x, na.rm = FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x) / length(x))
}

gi %>%
  group_by(diet) %>%
  summarise(
  predietmean = mean(X0324oi, na.rm = TRUE),
  predietse = se(X0324oi, na.rm = TRUE)
 )

length(na.omit(glucose$X0324oi))
length(na.omit(glucose$X0423oi))
length(na.omit(water$X0324oi))
length(na.omit(water$X0423oi))

gi %>%
  group_by(diet) %>%
  summarise(
    postdietmean = mean(X0423oi, na.rm = TRUE),
    postdietSE = se(X0423oi, na.rm = TRUE)
  )


boxplot(glucose$X0324gly, glucose$X0423gly,water$X0324gly, water$X0423gly) # visualize data

wilcox.test (glucose$X0324bka, glucose$X0423bka, paired = TRUE, alternative = "two.sided")
wilcox.test (water$X0324bka, water$X0423bka, paired = TRUE, alternative = "two.sided")

t.test (glucose$X0324agg, glucose$X0423agg, paired = TRUE, alternative = "two.sided")
t.test (water$X0324agg, water$X0423agg, paired = TRUE, alternative = "two.sided")

t.test (glucose$X0324lys, glucose$X0423lys, paired = TRUE, alternative = "two.sided")
t.test (water$X0324lys, water$X0423lys, paired = TRUE, alternative = "two.sided")

t.test (glucose$X0324glu, glucose$X0423glu, paired = TRUE, alternative = "two.sided")
t.test (water$X0324glu, water$X0423glu, paired = TRUE, alternative = "two.sided")

t.test (glucose$X0324gly, glucose$X0423gly, paired = TRUE, alternative = "two.sided")
t.test (water$X0324gly, water$X0423gly, paired = TRUE, alternative = "two.sided")

t.test (glucose$X0324totri, glucose$X0423totri, paired = TRUE, alternative = "two.sided")
t.test (water$X0324totri, water$X0423totri, paired = TRUE, alternative = "two.sided")

t.test (glucose$X0324trtri, glucose$X0423trtri, paired = TRUE, alternative = "two.sided")
t.test (water$X0324trtri, water$X0423trtri, paired = TRUE, alternative = "two.sided")

t.test (glucose$X0324oi, glucose$X0423oi, paired = TRUE, alternative = "two.sided")
t.test (water$X0324oi, water$X0423oi, paired = TRUE, alternative = "two.sided")

t.test (glucose$X0324oxystd, glucose$X0423oxystd, paired = TRUE, alternative = "two.sided")
t.test (water$X0324oxystd, water$X0423oxystd, paired = TRUE, alternative = "two.sided")

t.test (glucose$X0324dromstd, glucose$X0423dromstd, paired = TRUE, alternative = "two.sided")
t.test (water$X0324dromstd, water$X0423dromstd, paired = TRUE, alternative = "two.sided")

t.test (glucose$X0324mass, glucose$X0423mass, paired = TRUE, alternative = "two.sided")
t.test (water$X0324mass, water$X0423mass, paired = TRUE, alternative = "two.sided")
