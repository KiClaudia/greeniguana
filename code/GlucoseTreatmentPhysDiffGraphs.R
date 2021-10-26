# Graphically looking at basic phys difference before and after glucose treatment using repeated measures method 

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(gi)
install.packages("tidyverse")
library("tidyverse")
library("dplyr")

# 03/24 is pre-study baseline, no treatment or diet
# 03/25 diet begins
# 04/23 blood sample after 30 days of glucose, pre-LPS etc. 

# The phys variables we care about are bka, agg, lysis, glucose, totri, oi, trtri, glu, gly

summarydf <- data.frame(matrix(ncol = 18, nrow = 3)) 
#making an empty dataframe to put all the summary data in
str(summarydf)

summarydf %>% 
  mutate(pre-glucose_bka, mean(X0324bka))

sum <- gi %>%
  group_by(sex) %>%
  summarise(mean(X0324agg), sd(X0324bka), count=n()) %>%
  print()
  
sum <- gi %>%
  group_by(sex) %>%
  summarise(mean(X0423agg), sd(X0423bka), count=n()) %>%
  print()
#not done, i'll come back to this

#----------------------------- making boxplots of all the different phys variables before and after glucose
pdf('PrePostGlucoseBKA.pdf')

PrePostGlucoseBKA <- boxplot(gi$X0324bka, gi$X0423bka, xlab = "Glucose treatment", 
        ylab = "Bacterial percent killed", 
        main = "Change in BKA pre and post glucose treatment",
        names = c("Pre-glucose", "Post-glucose"))
dev.off()

pdf('PrePostGlucoseLysis.pdf')

PrePostGlucoseLysis <- boxplot(gi$X0324lys, gi$X0423lys, xlab = "Glucose treatment", 
                             ylab = "Lysis Score", 
                             main = "Change in lysis pre and post glucose treatment",
                             names = c("Pre-glucose", "Post-glucose"))
dev.off()

pdf('PrePostGlucoseAgg.pdf')

PrePostGlucoseAgg <- boxplot(gi$X0324agg, gi$X0423agg, xlab = "Glucose treatment", 
                               ylab = "Agglutination Score", 
                               main = "Change in agglutination pre and post glucose treatment",
                               names = c("Pre-glucose", "Post-glucose"))
dev.off()

pdf('PrePostGlucoseGlu.pdf')

PrePostGlucoseGlu <- boxplot(gi$X0324glu, gi$X0423glu, xlab = "Glucose treatment", 
                             ylab = "Glucose mg/dL", 
                             main = "Change in blood glucose pre and post glucose treatment",
                             names = c("Pre-glucose", "Post-glucose"))
dev.off()

pdf('PrePostGlucoseMass.pdf')

PrePostGlucoseMass <- boxplot(gi$X0324mass, gi$X0423mass, xlab = "Glucose treatment", 
                             ylab = "Iguana Mass g", 
                             main = "Change in mass pre and post glucose treatment",
                             names = c("Pre-glucose", "Post-glucose"))
dev.off()

pdf('PrePostGlucoseGly.pdf')

PrePostGlucoseGly <- boxplot(gi$X0324gly, gi$X0423gly, xlab = "Glucose treatment", 
                              ylab = "Free Glycerol mg/mL", 
                              main = "Change in glycerol pre and post glucose treatment",
                              names = c("Pre-glucose", "Post-glucose"))
dev.off()

pdf('PrePostGlucoseTotri.pdf')

PrePostGlucoseTotri <- boxplot(gi$X0324totri, gi$X0423totri, xlab = "Glucose treatment", 
                              ylab = "Total triglycerides mg/mL", 
                              main = "Change in total triglycerides pre and post glucose treatment",
                              names = c("Pre-glucose", "Post-glucose"))
dev.off()

pdf('PrePostGlucoseTrtri.pdf')

PrePostGlucoseTrtri <- boxplot(gi$X0324trtri, gi$X0423trtri, xlab = "Glucose treatment", 
                              ylab = "True triglycerides mg/mL", 
                              main = "Change in true triglycerides pre and post glucose treatment",
                              names = c("Pre-glucose", "Post-glucose"))
dev.off()

pdf('PrePostGlucoseOI.pdf')

PrePostGlucoseOI <- boxplot(gi$X0324oi, gi$X0423oi, xlab = "Glucose treatment", 
                              ylab = "Oxidative Index", 
                              main = "Change in oxidative index pre and post glucose treatment",
                              names = c("Pre-glucose", "Post-glucose"))
dev.off()

pdf('PrePostGlucoseDrom.pdf')

PrePostGlucoseDrom <- boxplot(gi$X0324drom, gi$X0423drom, xlab = "Glucose treatment", 
                              ylab = "dROM in UCarr", 
                              main = "Change in dROM pre and post glucose treatment",
                              names = c("Pre-glucose", "Post-glucose"))
dev.off()

pdf('PrePostGlucoseOxy.pdf')

PrePostGlucoseOxy <- boxplot(gi$X0324oxy, gi$X0423oxy, xlab = "Glucose treatment", 
                              ylab = "OXY umol HClO/mL", 
                              main = "Change in OXY pre and post glucose treatment",
                              names = c("Pre-glucose", "Post-glucose"))
dev.off()

