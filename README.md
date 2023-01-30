# greeniguana
spring 2021 green iguana data and analysis. 

Testing, this is a line from Rstudio

Testing, this is a line from github


# 10/21/2021
This is the Readme file for Spring 2021 green iguana project. This was a large study conducted from March through June at ASU with Dale DeNardo, Elizabeth Wu, and Susannah French. It is part of their NSF funded green iguana microbiome project.

For what is relevant to me, we tested the effects of a high sugar diet on immune function by having different groups (glucose and no glucose) and 2 LPS challeges spaced 4 weeks apart. A biopsy followed this and a GTT. We are also looking at telomere length before and after the study. 

There are two data files GreenIguanaMasterSpring2021, one is CSV and one is an excel workbook. I will never change the excel workbook. I also have Expt-LPS biopsy GTT spring 2021 study timeline revised workbook. This is the timeline for what treatment was given when throughout the whole study. 

There is a R markdown file here, not sure what will go in it yet but I will be using scripts for the analysis most likely. The code folder is where all my scripts for different questions will go into. I'm trying to comparmentalize as much as I can. The output folder is for any figures graphs, etc. from the code

There is a green iguana html file, probably won't use it, don't know how yet.

Authors: Claudia (lead), Dale (implemented the whole study) and Susannah (PIs), Elizabeth Wu (executed entirety of workshop), Erin Lewis (helped me on all of the assays)

Actually** I just made changes to the master file. removed all NAs so that it is just a blank so R doesn't think NA is text. resaved the CSV file over the old one so that its the same

# 10/26/2021
Created the GlucoseTreatmentPhysDiffGraphs script today where I am trying to create boxplots of relationships of the phys datas before and after glucose treatmet 03/24 and 04/23. Looking graphically at just pre and post glucose treatment and pre any other treatment. all graphs saved in PrePostGlucosePhysBoxplot which is in output

I did the OSI based on dROM UCARR but it shouldve been in 0.08mg/dL. so I redid the data sheet for the dROM data compilation to convert UCARR to mg/dL and then standardized it and in the master it recalculated OSI. Now I am re-entering the code for the graphs so they are up to date (mg/dL not UCARR). I also updated the master sheet in the other folder for the assays

Did a new script for the summary table for change in phys called GlucoseTreatmetPhysDiffTable, the one for graphs only has the graph code

# 10/27/2021
Created GlucoseTreatmentPhysDiffTXGroupGraphs script which is boxplots of difference pre and post diet across treatment groups (water and glucose) looking at phys data. This gives us a much better understanding of the difference across treatment groups after the treatment has been administered but before LPS etc. All those graphs are saved in output> PrePostDietPhysPlotsAcrossTrtGroups.

Took a look at graphs and in code I switched post and pre for water group so I switched it all, updated all the graphs and pushed it to github

Created 1LpsPhysDiffTXGroupGraphs script which is boxplots of difference pre and post (many time points) LPS across treatment groups looking at phys data. it is in the code folder

# 10/28/2021
Created output>24hrPost1LPSonPhys folder where I will be storing output from script 1LPSPhysDiffTXGroupGraphs. It is boxplots separated by each treatment group (GC, WC, GL, WL) for all the phys variables

made a change to the main data sheet because there is a "na" in WC$X0428agg that I actually typed in that I need to delete so R recognizes it as actually na. 

As part of the 1LPSPhysDiffTXGroupGraphs script, I have 24 and 72hr post lps into it. Graphs into Output>72hrpost1LPSonPhys

Adding 1week to the script but only doing bka, lys, agg, osi 
Adding 2week "" and 4week''

Created a script ProgressionGraphs1LPS for boxplots showing the progression of phys variables after LPS challenge, graphs are in ProgressionGraphsonPhys1LPS folder

# 11/1/2021
Created script GlucoseTrtPhysDiffStats which is the stats for looking at the difference between post and pre diet treatment. an excel sheet with the output was made GiDietPhys, which is in output. 
 
# 11/2/2021
Continued adding to the GiDietPhys output table for summary statistics.
Changed in the master spreadsheet = glycerol with negative values were changed to 0 since it can't be negative.
It is okay for dROMstd and OXYstd to be negative because it is a standardized value so it is centered around the mean, negative would just mean its less than mean. OI can also be negative and positive because 0 is balanecd, drom-oxy. Changed it for the csv and in the assay folder. 
Deleted GlucoseTreatmetPhysDiffTable script because I realized thats exactly what GlucoseTrtPhysDiff is
Deleted GlucoseTreatmentPhysDiffGraphs because it doesn't separate out treatment groups so its meaningless. 
Deleted PrePostGlucosePhysBoxplots because graphs in there doesn't separate out trt groups
Updated all the GLY graphs

Made script GlucoseTrtChangePhysStats to look at difference between groups after I calcualted change in phys measures, updated it in GiDietPhys excel spread sheet

# 11/23/2021
Made script 2wayAOVbka for repeated measure two way anova to look at how within each group (GL, GC, WL, WC) and NOT ACROSS, how the interactions of glucose and LPS affects BKA measures across time (4 weeks). Need to make data long and not wide, still figuring that out.
Made a new excel sheet BKA1LPSlong for the long format. 

# 11/24/2021
Originally I thought that a 2 way anova would be good for the LPS stuff but now I think it is better to do a mixed model anova because it looks at differences across time AND treatment (interaction). My idea of doing a RMaov and then another aov to compare between groups is essentially the same thing as this but not as well thought out. So for now I will keep the 2wayAOVbka script to use to write a new script for the mixed model called MMaovBKA.

# 11/30/2021
Worked on MMaovBKA, ran the aov and pwc on the BKA data. Made a new folder for my Rmd scripts. I will keep the original scripts for everything I run in code and Rmd is for the reporting of my findings. Made MMaovBKA.rmd 
***Actually, I am realizing that the mixed model anova tells me if there is an interaction between everytime and every treatment (there isn't for BKA) but for the main effects, it is taking the average across all the times for treatment and then average of all the treatment for time which doesn't work. SO. I need to use the mixed model to see if there is an interaction and if there is not, then I need to run a statistical test (one way aov?) comparing the change between each group and the time periods I am intersted in (two at a time i.e. 24hr - pre or 72hr - pre) to see the effect of treatment (main effect). Then for the main effect of time, I need to run a separate test for EACH group. 

renamed MMaovBKA to 1LBKA to signify these are the stats for BKA after primary immune challenge. this markdown file will be a combination of MMaovBKA.sript and 2wayaov. running the rest of the tests I need in 2wayaov script and then copying relevant parts into the Rmd file

Finished 1LBKA rmd file that summarizes my findings from script MMaovBKA and 2wayAOVbka. 

New rmd file to summarize results from diet treatment its called DietTrtPhys, results of diet on ALL phys

# 12/1/2021
Added to MMaovBKA where I tried shortening the time frame to just the spike, no difference found. Updated the Rmd for both BKA and Glucose treatment based on finding above and also to add the interpretation that Susannah and I talked about in our meeting today. 

#12/2/2021
As per Susannah's meeting, I did a two way anova on the change at specific time points for BKA and did not find any difference, this is in the 2wayAOVBKA script. Added those findings to my Rmd 1LPSBKA report

New script using mixed model and 2 way aov for agglutination, same as what I did for BKA. script is called AOVagg1 for anovas for agglutionation 1st LPS challenge. 

Created new folder called modData for modified data where I am storing excel spreadsheets for data I modified (long as opposed to wide). Some code might not work in the future because data is in a new file but that is easily fix-able

Made new long data for agglutination called AggLong
Made new long data for lysis called LysLong

Turns out I was doing the anovas wrong for BKA, redid it and updated the Rmd and the MMaov. Actually going to put it on pause for now because the data for the first BKA might be screwed up (methodology)

Going to put a pause on doing stuff for the first challenge and start on BKA for second challenge for sake of SICB 

New script BKA2aov to analysis BKA for the secondary challenge
New data BKA2LPSLong in modData

# 12/6/2021
New script AOVagg2 for agg LPS(2) analysis
New script AOVlys2 for lysis LPS(2) analysis
New script AOVosi2 for oxidative stress index LPS(2) analysis
New data OsiLong in modData
New data dromLong in modData
New data OXYlong in modData
New script AOVdrom2 for drom std LPS(2) analysis
New script AOVoxy2 for oxy std LPS(2) analysis

# 12/9/2021
Added code for line plots for secondary bka, osi, drom, agg, lys.

# 12/10/2021
Added code for bar plots and changed line plots for line plots for treatment groups (4 lines) which I really don't like. I like a combo of the bar and line.
Made poster quality graphics for bka
New SICB(2LPSplot) folder in output to put graphs

# 12/12/2021 
Messed around with making graphs pretty 

# 12/13/2021
Made nice SICB graphs for dROM, BKA, LYS, AGG

# 1/17/2022
New script called BetaBKA1 where I use a GLMM beta distribution to analyze the first BKA. Chaped 1LBKA.Rmd to explain what I am doing the in BetaBKA1 script since the MMaov doesn't work.

# 1/18/2022
Worked on AOVagg1, sqrt data to bc its not normal, redo anova. Need to take a step back and clean up the dta a bit, lots of outliers. 

# 1/19/2022
AOVagg1, 3way mixed anova (2btween 1 within) is what we are using for agg1. Also for BetaBKA1, we can't analyze data from first BKA because something wrong with the assay so we can't use it. 

# 1/20/2022
AOVagg1, ended up doing kruskalwallis on individual dates because it doesn't work to transform or do Gamma regresion. New script for agglutination r markdown. Done with analysis (for now lol). reporting on findings

# 1/21/2022
Agglutionation.Rmd completed as are analysis. working on Lysis 1 and 2, for consistency, AOVlys1 is the script for lysis primary challenge even if aov is not the test

# 1/24/2022
Updated 1LBKA.Rmd saying that there will be no analysis after looking through data with Susan.
dROM analysis for 1 and 2, made script AOVdrom1, ran anova, need to figure out how to run pairwise post hoc

# 1/25/2022
Did dROM 1 and 2, new script for dROM1. Ditto for OXY. did Rmd for both.

# 1/26/2022
https://www.datanovia.com/en/lessons/mixed-anova-in-r/#three-way-bbw-b this is the mixed anova site I used, for future reference
analyzed and Rmd for OSI
https://www.datanovia.com/en/lessons/mixed-anova-in-r/
Redid posthoc analysis for OSI based on post hoc from above, drom too
Tried to redo BKA2 but need to check with susannah to see if its even worth it to do for bka, fnished Rmd (used beta regression)

two way interactions post hoc -> simple main effect anova
main effect post hoc -> pairwise comparisons

# 2/17/2022
New script for Triglycerides, AOVtri1
Made new long data for triglycerides (total), glucose, and glycerol, all stored in modData

# 2/24/2022
worked on AOVtotri1 (changed AOVtri1 to this because I will do true tri too). 
Bonferroni has more power when the number of comparisons is small, whereas Tukey is more powerful when testing large numbers of means.so we should use bonferroni for the pairwise comparisons. 
ToTRI Rmd.

# 2/25/2022
Made true triglyceride Long data.
New trutri script AOVtruTRI1 and AOVtruTRI2
Rmd for trutri

# 2/28/2022
New script for AOVgly1 and 2

# 3/1/2022
New script for glucose and Rmd. Finished all analysis for 1st and 2nd challenges except for CORT
** Note keep transformation for a set of data for entire study, for example, I inflated and logged TrTRI1 so will do the same for TRTRI2

# 3/2/2022
script and new folder looking at effect of glucose over entire experiment. script for trtri. need to do this analysis for every variable. 
No rmd for this, just a table in excel


# 3/4/2022
will use one way anova to analyze glucose effects from day 0 and day 107. Need to also make new data table that is NOT long data and with treatment groups as G0 G107 W0 W107. This data is called GiWideDay0_107Data, it is in the main folder
Made new scripts for one way ANOVA for total TRI and true TRI and mass

# 3/14/2022
GiWide data is in modData. More scripts for this analysis + results in output folder excel sheet.
Finished coding all scripts for day0 vs day 107. Entered all outputs into excel file GlucoseEffectDay0_Day107. sent to SF for comments

# 4/14/2022
Instead of one way anova, SF thinks I should do a matched T test. Will redo using same scripts from GLucose3monthEffect but add it under the existing ANOVA. If not normal, use wilcoxin paired rank signed test

But wait, as I am doing this now, matched T only works for two, but if I want to compare g107 and w107... I should use ANOVA, let me check with SF

# 4/25/2022
For beginning and end effects (0 vs 107) we will use a two-way RM ANOVA (check utah state notebook for notes on why). It is more comprehensive than a one way ANOVA. using same scripts from Glucose3monthEffect_code. 
Added a second tab to excel sheet GlucoseEffectDay0_Day107Results where I am putting results from two way anova RM. highlighted in red is what is not dont, black is good to go. Adjusting the summary  statistics table as well.
Did mass.

# 5/1/2022
just as a note to my future self. When doing an ANOVA with interaction, even if two-way interaction is significant, still interpret the main effects. 

# 5/3/2022
analyzed main effects of total triglycerides, still need to do interaction effects (simple main and then pairwise, then do it again with the variables reversed)

# 5/27/2022
doing interaction post hoc for totri. Did a simple main, was significant, did pairwise, since I only have two groups for diet (g and W), idk if doing pairwise is neccessary because "where" the difference is has to be bewteen the only two groups. I can see this pairwise step being nececssary if there were 3 groups and so on. 

NOTE TO SELF, difference between post hoc for main effect and interaction effect is that it is not group_by for main but is for interaction

glucose 2way rm aov and post hoc, updated GlucoseEffectDay0_Day107Results, took outlier out (iguana #5)
OXY 2way rm aov and post hoc. updated excel. took out some outliers. some post hoc had to change paired to FALSE because of stuff taken out

# 6/6/2022
redid dROM with 2 way rm aov, did OSI, updated the table as well. ditto for true triglycerides

# 6/7/2022
for glycerol, previously we did kruskal wallis because because data wasn't normal. However, if we subtract day0 from 107, it is normal so we can run a t test between glucose and water group.
ditto for agglutination
For BKA, it is not normal so can't do anova or t-test. It is also not homoskedastic so we can't use kruskal wallis. I will use beta regression as it allows for data bounded between 0 and 1, it's curve is not normal and can match the left skew, and heteroscedastic data is okay. updated result table as well

# 7/14/2022
made new folder sugarLPS-figures for graphs I make for the sugar LPS microbiome paper. Code for BKA graph is in aovBKA2 in LPSchallenge_Code.
Made new script for AggFigures, so many figures that I just made a new script so I don't add to the other scripts
Made agglutination line graph  for both challenges, LPS higher than control
Made graph in AggFigures for agglutination effect on treatment groups

# 7/20/2022
Added significane ** to agglutination line graphs.

New script LysFigures in sugarLPS_figures. Line graph made for main effect of LPS on second challenge for Lysis. line graph made for main time effect second challenge lysis

New  script and figures for OSI 

New script and figure for total tri

Double checked results manuscript, sent new updated version to SF and DD (made sure numbesr were according to bonferroni adjusted values)

# 1/30/2023
New script for IgY analysis called AOVigy1 in LPSchallenge_code folder. finished analysis for it.
made new csv for IgY Long data
wrote up the results quickly to send to SF for perusal. i wrote it separate from results because i want to verify this is the method I should use before merging with the rest of the results that is already valid. 