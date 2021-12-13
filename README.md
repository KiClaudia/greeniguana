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

#12/9/2021
Added code for line plots for secondary bka, osi, drom, agg, lys.

#12/10/2021
Added code for bar plots and changed line plots for line plots for treatment groups (4 lines) which I really don't like. I like a combo of the bar and line.
Made poster quality graphics for bka
New SICB(2LPSplot) folder in output to put graphs

#12/12/2021 
Messed around with making graphs pretty 
