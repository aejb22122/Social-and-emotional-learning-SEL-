# Title:    Devereux Student Strengths Assessment (DESSA) - Spring 2019
# Author:   Annick Eudes JB Research Manager
# Date:     July 24, 2019
# Version:  1.0

# ---- Preliminaries ----
# Loading the packages that we might need :

# library(tidyverse)
# library(ggplot2)
# library(plyr)
# library(dplyr)


# ---- 1. Importing the practionner's data -----
# Importing the dataset

library(readxl)
prt_data <- read_excel("participant_data_2019.xlsx")
View(prt_data)

# Cleaning ...
# prt_data[c("Organization type DON'T USE", "Director full name", "Director Email", "Quality Advisors")] <- NULL


# Renaming the columns
names(prt_data)
names(prt_data) <- c("Your_ID", "Program_Name", "Full_name")
#names(prt_data) <- c("Program_Name", "Your_ID", "Full_name")

# ---- 2. Merging the practionner's data with the DESSA data -----
# Let's make a copy of the sel_data_imputed to preserve the original data
dessa_data <- sel_data_imputed

# We will be using a "full" joint to keep all the observation in both datasets 
# this will permit updates as the survey continues ...

# This process is case sensitive, thus we have to convert all the ID to uppercase string characters - toupper()
# dessa_data[ ,6] = toupper(dessa_data[, 6])


# Will be Join by = "Your_ID"

dessa.complete <- full_join(prt_data, dessa_data, by = "Your_ID")
View(dessa.complete)


# Let's export the entire results
openxlsx::write.xlsx(dessa.complete, file = "P:/RE/Private/SEL Assessment Results/DESSA Scoring/results/Post_Test/DESSA_Post_Test_Scoring_and_programs.xlsx")
file.exists("P:/RE/Private/SEL Assessment Results/DESSA Scoring/results/Post_Test/DESSA_Post_Test_Scoring_and_programs.xlsx")



# ---- Conclusion ~ Results summary for the DESSA template ----

# We will export the results for the dessa template.
# Results_for_template <- select(dessa.complete, Child_ID, Your_ID, Date_interview_start, Program_Name, Full_name, 
#                                Personal_Responsibility, PRTScore, PRPercentile, PRDescription,
#                                Optimistic_Thinking, OTTScore, Percentile_OT, OTDescription,
#                                Goal_Directed_Behavior, GDBTScore, Percentile_GDB, GBDescription,
#                                Social_Awareness, SocAW_TScore, Percentile_SocAw, SODescription,
#                                Decision_Making, DMTScore, Percentile_DM, DMDescription,
#                                Relationship_Skills, RSkill_TScore, Percentile_RS, RSDescription,
#                                Self_Awareness, SATScore, Percentile_Self_Aw, SADescription,
#                                Self_Management, Self_M_TScore, Percentile_Self_M, SMDescription,
#                                raw_SEC, SEC_Tcore, SEC_percentile, SECDescription)

Results_for_template <- select(dessa.complete, Child_ID, Your_ID, Program_Name, Full_name, 
                               Personal_Responsibility, PRTScore, PRPercentile, PRDescription,
                               Optimistic_Thinking, OTTScore, Percentile_OT, OTDescription,
                               Goal_Directed_Behavior, GDBTScore, Percentile_GDB, GBDescription,
                               Social_Awareness, SocAW_TScore, Percentile_SocAw, SODescription,
                               Decision_Making, DMTScore, Percentile_DM, DMDescription,
                               Relationship_Skills, RSkill_TScore, Percentile_RS, RSDescription,
                               Self_Awareness, SATScore, Percentile_Self_Aw, SADescription,
                               Self_Management, Self_M_TScore, Percentile_Self_M, SMDescription,
                               raw_SEC, SEC_Tcore, SEC_percentile, SECDescription)


# 1) We will rename the variables so they correspond to the names in the template...
# names(Results_for_template) <- c("ChildID", "Your ID", "RatingDate", "GroupName", "RaterName",
#                                  "PRScaleRawScore", "PRTScore", "PRPercentile", "PRDescription",
#                                  "OTScaleRawScore", "OTTScore", "OTPercentile", "OTDescription",
#                                  "GBScaleRawScore", "GBTScore", "GBPercentile", "GBDescription",
#                                  "SOScaleRawScore", "SOTScore", "SOPercentile", "SODescription",
#                                  "DMScaleRawScore", "DMTScore", "DMPercentile", "DMDescription",
#                                  "RSScaleRawScore", "RSTScore", "RSPercentile", "RSDescription",
#                                  "SAScaleRawScore", "SATScore", "SAPercentile", "SADescription",
#                                  "SMScaleRawScore", "SMTScore", "SMPercentile", "SMDescription",
#                                  "SECScaleRawScore", "SECTScore", "SECPercentile", "SECDescription")

names(Results_for_template) <- c("ChildID", "Your ID", "GroupName", "RaterName",
                                 "PRScaleRawScore", "PRTScore", "PRPercentile", "PRDescription",
                                 "OTScaleRawScore", "OTTScore", "OTPercentile", "OTDescription",
                                 "GBScaleRawScore", "GBTScore", "GBPercentile", "GBDescription",
                                 "SOScaleRawScore", "SOTScore", "SOPercentile", "SODescription",
                                 "DMScaleRawScore", "DMTScore", "DMPercentile", "DMDescription",
                                 "RSScaleRawScore", "RSTScore", "RSPercentile", "RSDescription",
                                 "SAScaleRawScore", "SATScore", "SAPercentile", "SADescription",
                                 "SMScaleRawScore", "SMTScore", "SMPercentile", "SMDescription",
                                 "SECScaleRawScore", "SECTScore", "SECPercentile", "SECDescription")



# This is part of the tidyverse package type help(readxl) for more info...


openxlsx::write.xlsx(Results_for_template, file = "P:/RE/Private/SEL Assessment Results/DESSA Scoring/results/Post_Test/DESSA_Post_Test_Results_for_template.xlsx")
file.exists("P:/RE/Private/SEL Assessment Results/DESSA Scoring/results/Post_Test/DESSA_Post_Test_Results_for_template.xlsx")


