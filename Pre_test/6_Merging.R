# Title:    Devereux Student Strengths Assessment (DESSA) - Fall 2018
# Author:   Annick Eudes JB Research Manager
# Date:     December 5, 2018
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
prt_data <- read_excel("practitioner_data_2018.xlsx")
View(prt_data)

# Cleaning
prt_data[c("Organization type DON'T USE", "Director full name", "Director Email", "Quality Advisors")] <- NULL


# Renaming the columns
names(prt_data)
names(prt_data) <- c("ORGID", "Program_Name", "Program_Type", "Salesforce_ID", "Your_ID", "Registry_ID",
                     "Your_ID2", "Full_name", "First_Name", "Last_Name", "Email")

# ---- 2. Merging the practionner's data with the DESSA data -----
# Let's make a copy of the sel_data_imputed to preserve the original data
dessa_data <- sel_data_imputed

# We will be using a "full" joint to keep all the observation in both datasets 
# this will permit updates as the survey continues ...

# This process is case sensitive, thus we have to convert all the ID to uppercase string characters - toupper()
dessa_data[ ,2] = toupper(dessa_data[,2])       # Will be Join by = "Your_ID"

dessa.complete <- full_join(dessa_data, prt_data)
View(dessa.complete)


# Let's export the entire results
openxlsx::write.xlsx(dessa.complete, file = "P:/RE/Private/Ten Year Report/Data and Analysis/SEL/Reports to Programs/2018-2019 DESSA/DESSA_2018_2019/results/DESSA_Scoring_and_programs.xlsx")
file.exists("P:/RE/Private/Ten Year Report/Data and Analysis/SEL/Reports to Programs/2018-2019 DESSA/DESSA_2018_2019/results/DESSA_Scoring_and_programs.xlsx")


# We will export the results for the dessa template.
Results_template <- select(dessa.complete, Child_ID, Program_Name, Your_ID, Full_name, 
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
names(Results_template) <-c("ChildID", "GroupName", "Your ID", "RaterName", 
                            "PRScaleRawScore", "PRTScore", "PRPercentile", "PRDescription", 
                            "OTScaleRawScore", "OTTScore", "OTPercentile", "OTDescription",
                            "GBScaleRawScore", "GBTScore", "GBPercentile", "GBDescription",
                            "SOScaleRawScore", "SOTScore", "SOPercentile", "SODescription",
                            "DMScaleRawScore", "DMTScore", "DMPercentile", "DMDescription",
                            "RSScaleRawScore", "RSTScore", "RSPercentile", "RSDescription",
                            "SAScaleRawScore", "SATScore", "SAPercentile", "SADescription",
                            "SMScaleRawScore", "SMTScore", "SMPercentile", "SMDescription",
                            "SECScaleRawScore", "SECTScore", "SECPercentile", "SECDescription")


# Exporting the Results that will be used in the template in excel format :
openxlsx::write.xlsx(sel_data_imputed, file = "P:/RE/Private/Ten Year Report/Data and Analysis/SEL/Reports to Programs/2018-2019 DESSA/DESSA_2018_2019/results/Results_template.xlsx")
file.exists("P:/RE/Private/Ten Year Report/Data and Analysis/SEL/Reports to Programs/2018-2019 DESSA/DESSA_2018_2019/results/Results_template.xlsx")
