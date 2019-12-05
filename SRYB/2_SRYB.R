# Title:    SRYB - Short Form Measurement - Fall 2019
# Author:   Annick Eudes JB Research Manager
# Date:     December 4, 2019


# Reviewd by :
# Date :

# ---- Preliminaries ----

# ---- 1. Importing the practionner's data -----
# Importing the dataset


library(readxl)
practitionerID <- read_excel("P:/RE/Private/SEL Assessment Results/2019_2020_SRYB_Scoring/data/practitionerID.xlsx")
View(practitionerID)


# Cleaning ...
# prt_data[c("Organization type DON'T USE", "Director full name", "Director Email", "Quality Advisors")] <- NULL

# Renaming the columns
names(practitionerID)
names(practitionerID) <- c("Program_Name", "Your_ID", "Full_name")

# ---- 2. Merging the practionner's data with the SRYB data -----

# Will be Join by = "Your_ID"

sryb <- full_join(df, practitionerID, by = "Your_ID")
View(sryb)

# ---- Conclusion ~ Results summary for the SRYB template ----

# Let's export the results
openxlsx::write.xlsx(sryb, file = "P:/RE/Private/SEL Assessment Results/2019_2020_SRYB_Scoring/results/SRYB_all_programs.xlsx")
file.exists("P:/RE/Private/SEL Assessment Results/2019_2020_SRYB_Scoring/results/SRYB_all_programs.xlsx")


# We will export the results for the SRYB template.
Results_for_template <- select(sryb, ChildID, Your_ID, Program_Name, Full_name, Question1,
                               Question2, Question3, Question4, Question5, Question6, Question7,
                               Question8, Question9, Question10, Question11, Question12,
                               Question13, Question14)

"P:\RE\Private\SEL Assessment Results\2019_2020_SRYB_Scoring\results"

# This is part of the tidyverse package type help(readxl) for more info...
openxlsx::write.xlsx(Results_for_template, file = "P:/RE/Private/SEL Assessment Results/2019_2020_SRYB_Scoring/results/SRYB_results_for_template.xlsx")
file.exists("P:/RE/Private/SEL Assessment Results/2019_2020_SRYB_Scoring/results/SRYB_results_for_template.xlsx")
