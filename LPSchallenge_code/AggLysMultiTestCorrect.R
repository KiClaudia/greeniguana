#Bonferroni adjusted p values for mannwhitney lysis and agglutination (needs correction due to multiple testing)


# lysis------------------
pval_diet_change <- c(0.047, 0.168)
adjusted <- p.adjust(pval_diet_change, method = "bonferroni")
alpha <- 0.05

# not significant anymore

pval_diet_LPS2 <- c(0.382, 0.363, 0.484, 0.0008, 0.738, 0.468)
adjusted2 <- p.adjust(pval_diet_LPS2, method = "bonferroni")
alpha <- 0.05
adjusted2

# still significant

pval_lps_lps2 <- c(0.102, 0.041, 0.028, 0.155, 0.079, 0.191)
adjusted3 <- p.adjust(pval_lps_lps2, method = "bonferroni")
alpha <- 0.05
adjusted3

# Agglutination-------------------

pval_diet_lp1 <- c(0.034, 0.93, 0.469, 0.229, 0.442, 0.039)
adjusted <- p.adjust(pval_diet_lp1, method = "bonferroni")
alpha <- 0.05
adjusted

pval_diet_lp2 <- c(0.48, 0.237, 0.125, 0.116, 0.703)
adjusted <- p.adjust(pval_diet_lp2, method = "bonferroni")
alpha <- 0.05
adjusted

pval_lps_lp1 <- c(.256, .11, .006, .002, 0.019, 0.079)
adjusted <- p.adjust(pval_lps_lp1, method = "bonferroni")
alpha <- 0.05
adjusted

pval_lps_lp2 <- c(0.002, 0.0001, 0.021, 0.034, 0.032 )
adjusted <- p.adjust(pval_lps_lp2, method = "bonferroni")
alpha <- 0.05
adjusted
