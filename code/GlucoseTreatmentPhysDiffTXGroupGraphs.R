# Graphically looking at basic phys difference before and after glucose treatment using repeated measures method 

gi <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/ASU green iguana 2021/greeniguanaAnalysis/GreenIguanaMasterSpring2021.csv")
View(gi)
install.packages("tidyverse")
library("tidyverse")
library("dplyr")

# 03/24 is pre-study baseline, no treatment or diet
# 03/25 diet begins
# 04/23 blood sample after 30 days of glucose, pre-LPS etc. 

#------------ making boxplots of all the different phys variables before and after glucose SEPARATING OUT treatment groups
# so at this stage the two groups are glucose and non glucose

glucose <- gi %>%
  filter(diet == "g") 
  
water <- gi %>%
  filter(diet == "w") 
#----------------------------------------------------------------------------------
pdf('PrePostDietTrtGroupBKA.pdf',
    width = 10, height = 7)
boxplot(glucose$X0324bka, glucose$X0423bka, water$X0324bka, water$X0423bka, xlab = "Treatment Groups", 
                             ylab = "Bacterial percent killed", 
                             main = "Change in BKA across glucose and water treatment group pre- and post- diet",
                             names = c("Pre-diet glucose group", "Post-diet glucose group", "Pre-diet water group", "Post-diet water group"))
dev.off()

pdf('PrePostDietTrtGroupLys.pdf',
    width = 10, height = 7)
PrePostGlucoseLys <- boxplot(glucose$X0324lys, glucose$X0423lys, water$X0324lys, water$X0423lys, xlab = "Treatment Groups", 
                             ylab = "Lysis Score", 
                             main = "Change in lysis across glucose and water treatment group pre- and post- diet",
                             names = c("Pre-diet glucose group", "Post-diet glucose group", "Pre-diet water group", "Post-diet water group"))
dev.off()

pdf('PrePostDietTrtGroupAgg.pdf',
    width = 10, height = 7)
PrePostGlucoseAgg <- boxplot(glucose$X0324agg, glucose$X0423agg, water$X0324agg, water$X0423agg, xlab = "Treatment Groups", 
                             ylab = "Agglutination Score", 
                             main = "Change in agglutination across glucose and water treatment group pre- and post- diet",
                             names = c("Pre-diet glucose group", "Post-diet glucose group", "Pre-diet water group", "Post-diet water group"))
dev.off()

pdf('PrePostDietTrtGroupGlu.pdf',
    width = 10, height = 7)
PrePostGlucoseGlu <- boxplot(glucose$X0324glu, glucose$X0423glu, water$X0324glu, water$X0423glu, xlab = "Treatment Groups", 
                             ylab = "Blood Glucose mg/dL", 
                             main = "Change in blood glucose across glucose and water treatment group pre- and post- diet",
                             names = c("Pre-diet glucose group", "Post-diet glucose group", "Pre-diet water group", "Post-diet water group"))
dev.off()

pdf('PrePostDietTrtGroupMass.pdf',
    width = 10, height = 7)
PrePostGlucoseMass <- boxplot(glucose$X0324mass, glucose$X0423mass, water$X0324mass, water$X0423mass, xlab = "Treatment Groups", 
                             ylab = "Mass g", 
                             main = "Change in mass across glucose and water treatment group pre- and post- diet",
                             names = c("Pre-diet glucose group", "Post-diet glucose group", "Pre-diet water group", "Post-diet water group"))
dev.off()

pdf('PrePostDietTrtGroupGly.pdf',
    width = 10, height = 7)
PrePostGlucoseGly <- boxplot(glucose$X0324gly, glucose$X0423gly, water$X0324gly, water$X0423gly, xlab = "Treatment Groups", 
                             ylab = "Free glycerol mg/mL", 
                             main = "Change in glycerol across glucose and water treatment group pre- and post- diet",
                             names = c("Pre-diet glucose group", "Post-diet glucose group", "Pre-diet water group", "Post-diet water group"))
dev.off()

pdf('PrePostDietTrtGroupTotri.pdf',
    width = 10, height = 7)
PrePostGlucoseTotri <- boxplot(glucose$X0324totri, glucose$X0423totri, water$X0324totri, water$X0423totri, xlab = "Treatment Groups", 
                             ylab = "Total Triglycerides mg/mL", 
                             main = "Change in triglycerides across glucose and water treatment group pre- and post- diet",
                             names = c("Pre-diet glucose group", "Post-diet glucose group", "Pre-diet water group", "Post-diet water group"))
dev.off()

pdf('PrePostDietTrtGroupTrtri.pdf',
    width = 10, height = 7)
PrePostGlucoseTrtri <- boxplot(glucose$X0324trtri, glucose$X0423trtri, water$X0324trtri, water$X0423trtri, xlab = "Treatment Groups", 
                             ylab = "True triglycerides mg/mL", 
                             main = "Change in triglycerides across glucose and water treatment group pre- and post- diet",
                             names = c("Pre-diet glucose group", "Post-diet glucose group", "Pre-diet water group", "Post-diet water group"))
dev.off()

pdf('PrePostDietTrtGroupOSI.pdf',
    width = 10, height = 7)
PrePostGlucoseOSI <- boxplot(glucose$X0324oi, glucose$X0423oi, water$X0324oi, water$X0423oi, xlab = "Treatment Groups", 
                             ylab = "Oxidative Stress Index", 
                             main = "Change in oxidative stress index across glucose and water treatment group pre- and post- diet",
                             names = c("Pre-diet glucose group", "Post-diet glucose group", "Pre-diet water group", "Post-diet water group"))
dev.off()

pdf('PrePostDietTrtGroupDROM.pdf',
    width = 10, height = 7)
PrePostGlucoseDROM <- boxplot(glucose$X0324drom, glucose$X0423drom, water$X0324drom, water$X0423drom, xlab = "Treatment Groups", 
                             ylab = "dROM mg H2O2 per dL", 
                             main = "Change in dROM across glucose and water treatment group pre- and post- diet",
                             names = c("Pre-diet glucose group", "Post-diet glucose group", "Pre-diet water group", "Post-diet water group"))
dev.off()

pdf('PrePostDietTrtGroupOXY.pdf',
    width = 10, height = 7)
PrePostGlucoseOXY <- boxplot(glucose$X0324oxy, glucose$X0423oxy, water$X0324oxy, water$X0423oxy, xlab = "Treatment Groups", 
                              ylab = "OXY umol HClO per mL", 
                              main = "Change in OXY across glucose and water treatment group pre- and post- diet",
                              names = c("Pre-diet glucose group", "Post-diet glucose group", "Pre-diet water group", "Post-diet water group"))
dev.off()
