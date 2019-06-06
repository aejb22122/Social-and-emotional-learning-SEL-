# Title:    Devereux Student Strengths Assessment (DESSA) - Fall 2018
# Author:   Annick Eudes JB Research Manager
# Date:     December 5, 2018
# Version:  1.0

# Reviewd by :
# Date :

# ---- Preliminaries ----
# Loading the packages that we might need :

# library(tidyverse)
# library(ggplot2)
# library(plyr)
# library(dplyr)

# ---- Step 6: Determining the Description for Each Scale ----
# We will prepare descriptions for each T-Scores

# Personnal Responsibility descriptions
PRDescription <- 
  ifelse(PRTScore >= 60, "S",
         ifelse(PRTScore <= 42 | PRTScore <= 59, "T",
                ifelse(PRTScore <= 40, "N",
                       ifelse(is.na(PRTScore),0,0))))

# We will had this new description variable to the completed dessa dataset
sel_data_imputed$PRDescription <- PRDescription

# Optimistic Thinking descriptions
OTDescription <-
  ifelse(OTTScore >= 60, "S",
         ifelse(OTTScore <= 42 | OTTScore <= 59, "T",
                ifelse(OTTScore <= 40, "N",
                       ifelse(is.na(OTTScore),0,0))))

sel_data_imputed$OTDescription <- OTDescription


# Goal Direted Behavior description
GBDescription <-
  ifelse(GDBTScore >= 60, "S",
         ifelse(GDBTScore <= 42 | GDBTScore <= 59, "T",
                ifelse(GDBTScore <= 40, "N",
                       ifelse(is.na(GDBTScore), 0,0))))

sel_data_imputed$GBDescription <- GBDescription
  
# Child's Social Awareness description
SODescription <-
  ifelse(SocAW_TScore >= 60, "S",
         ifelse(SocAW_TScore <= 42 | SocAW_TScore <= 59, "T",
                ifelse(is.na(SocAW_TScore), 0, 0)))

sel_data_imputed$SODescription <- SODescription

# Decision Making Description
DMDescription <- 
  ifelse(DMTScore >= 60, "S",
         ifelse(DMTScore <= 42 | DMTScore <= 59, "T",
                ifelse(DMTScore <= 40, "N",
                       ifelse(is.na(DMTScore), 0, 0))))

sel_data_imputed$DMDescription <- DMDescription

# Child's Relationship Skills 
RSDescription <-
  ifelse(RSkill_TScore >= 60, "S",
         ifelse(RSkill_TScore <= 42 | RSkill_TScore <= 59, "T",
                ifelse(RSkill_TScore <= 40, "N",
                       ifelse(is.na(RSkill_TScore), 0, 0))))

sel_data_imputed$RSDescription <- RSDescription

# Child's Self Awareness description
SADescription <- 
  ifelse(SATScore >= 60, "S",
         ifelse(SATScore <= 42 | SATScore <= 59, "T",
                ifelse(SATScore <= 40, "N",
                       ifelse(is.na(SATScore), 0,0))))

sel_data_imputed$SADescription <- SADescription

# Child's Self_Management description
SMDescription <-
  ifelse(Self_M_TScore >= 60, "S",
         ifelse(Self_M_TScore <= 42 | Self_M_TScore <= 59, "T",
                ifelse(Self_M_TScore <= 40, "N",
                       ifelse(is.na(Self_M_TScore), 0,0))))

sel_data_imputed$SMDescription <- SMDescription

# Social Emotional Composite description
SECDescription <-
  ifelse(SEC_Tcore >= 60, "S",
         ifelse(SEC_Tcore <= 42 | SEC_Tcore <= 59, "T",
                ifelse(SEC_Tcore <= 40, "N",
                       ifelse(is.na(SEC_Tcore), 0, 0))))

sel_data_imputed$SECDescription <- SECDescription

# Verifications
names(sel_data_imputed)
View(sel_data_imputed)

openxlsx::write.xlsx(sel_data_imputed, file = "P:/RE/Private/Ten Year Report/Data and Analysis/SEL/Reports to Programs/2018-2019 DESSA/DESSA_2018_2019/results/DESSA_Scoring_and_descriptions.xlsx")

file.exists("P:/RE/Private/Ten Year Report/Data and Analysis/SEL/Reports to Programs/2018-2019 DESSA/DESSA_2018_2019/results/DESSA_Scoring_and_descriptions.xlsx")
