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
