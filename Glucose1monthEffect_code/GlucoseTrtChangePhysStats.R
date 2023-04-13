# Look at the change between diet treatment on phys variables
# 1) Create a new vector where it is 0423phys-0324phys of glucose and then of water. 
# 2) Check for normality with histogram
# 3) Compare with a t.test (not paired)
# 4) Summary statistics

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(gi)
install.packages("tidyverse")
library("tidyverse")
library("dplyr")
library("plotrix")

glucose <- gi %>%
  filter(diet == "g") 
water <- gi %>%
  filter(diet == "w") 
#----------bka----------

gluBKA <- glucose$X0423bka - glucose$X0324bka
watBKA <- water$X0423bka - water$X0324bka

hist(gluBKA)
hist(watBKA)

t.test (gluBKA, watBKA, alternative = "two.sided")

mean(gluBKA, na.rm = TRUE)
mean(watBKA, na.rm = TRUE)
std.error(gluBKA, na.rm = TRUE)
std.error(watBKA, na.rm = TRUE)

#---------agg----------
glu <- glucose$X0423agg - glucose$X0324agg
wat <- water$X0423agg - water$X0324agg

hist(glu)
hist(wat)

t.test (glu, wat, alternative = "two.sided")

mean(glu, na.rm = TRUE)
mean(wat, na.rm = TRUE)
std.error(glu, na.rm = TRUE)
std.error(wat, na.rm = TRUE)
length(na.omit(glu))
length(na.omit(wat))

#--------lys------------
glu <- glucose$X0423lys - glucose$X0324lys
wat <- water$X0423lys - water$X0324lys

hist(glu)
hist(wat)

t.test (glu, wat, alternative = "two.sided")

mean(glu, na.rm = TRUE)
mean(wat, na.rm = TRUE)
std.error(glu, na.rm = TRUE)
std.error(wat, na.rm = TRUE)
length(na.omit(glu))
length(na.omit(wat))

#-------glucose---------
glu <- glucose$X0423glu - glucose$X0324glu
wat <- water$X0423glu - water$X0324glu

hist(glu)
hist(wat)

t.test (glu, wat, alternative = "two.sided")

mean(glu, na.rm = TRUE)
mean(wat, na.rm = TRUE)
std.error(glu, na.rm = TRUE)
std.error(wat, na.rm = TRUE)
length(na.omit(glu))
length(na.omit(wat))

#---------glycerol---------
glu <- glucose$X0423gly - glucose$X0324gly
wat <- water$X0423gly - water$X0324gly

hist(glu)
hist(wat)

t.test (glu, wat, alternative = "two.sided")

mean(glu, na.rm = TRUE)
mean(wat, na.rm = TRUE)
std.error(glu, na.rm = TRUE)
std.error(wat, na.rm = TRUE)
length(na.omit(glu))
length(na.omit(wat))

#-------totri------
glu <- glucose$X0423totri - glucose$X0324totri
wat <- water$X0423totri - water$X0324totri

hist(glu)
hist(wat)

t.test (glu, wat, alternative = "two.sided")

mean(glu, na.rm = TRUE)
mean(wat, na.rm = TRUE)
std.error(glu, na.rm = TRUE)
std.error(wat, na.rm = TRUE)
length(na.omit(glu))
length(na.omit(wat))

#-------trtri----------
glu <- glucose$X0423trtri - glucose$X0324trtri
wat <- water$X0423trtri - water$X0324trtri

hist(glu)
hist(wat)

t.test (glu, wat, alternative = "two.sided")

mean(glu, na.rm = TRUE)
mean(wat, na.rm = TRUE)
se(glu, na.rm = TRUE)
se(wat, na.rm = TRUE)
length(na.omit(glu))
length(na.omit(wat))

#----------OSI----------
glu <- glucose$X0423oi - glucose$X0324oi
wat <- water$X0423oi - water$X0324oi

hist(glu)
hist(wat)

t.test (glu, wat, alternative = "two.sided")

mean(glu, na.rm = TRUE)
mean(wat, na.rm = TRUE)
std.error(glu, na.rm = TRUE)
std.error(wat, na.rm = TRUE)
length(na.omit(glu))
length(na.omit(wat))

#-----------OXYstd--------
glu <- glucose$X0423oxystd - glucose$X0324oxystd
wat <- water$X0423oxystd - water$X0324oxystd

hist(glu)
hist(wat)

t.test (glu, wat, alternative = "two.sided")

mean(glu, na.rm = TRUE)
mean(wat, na.rm = TRUE)
se(glu, na.rm = TRUE)
se(wat, na.rm = TRUE)
length(na.omit(glu))
length(na.omit(wat))


#------------dROMstd---------
glu <- glucose$X0423dromstd - glucose$X0324dromstd
wat <- water$X0423dromstd - water$X0324dromstd

hist(glu)
hist(wat)

t.test (glu, wat, alternative = "two.sided")

mean(glu, na.rm = TRUE)
mean(wat, na.rm = TRUE)
se(glu, na.rm = TRUE)
se(wat, na.rm = TRUE)
length(na.omit(glu))
length(na.omit(wat))

#-------mass---------
glu <- glucose$X0423mass - glucose$X0324mass
wat <- water$X0423mass - water$X0324mass

hist(glu)
hist(wat)

t.test (glu, wat, alternative = "two.sided")

mean(glu, na.rm = TRUE)
mean(wat, na.rm = TRUE)
std.error(glu, na.rm = TRUE)
std.error(wat, na.rm = TRUE)
length(na.omit(glu))
length(na.omit(wat))
