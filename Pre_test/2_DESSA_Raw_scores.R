# Title:    Devereux Student Strengths Assessment (DESSA) - Fall 2018
# Author:   Annick Eudes JB Research Manager
# Date:     November 09, 2018
# Version:  2.0

# Reviewd by : Annick Eudes 

# This revision of the stript replaces the original cleaned dataset with the 
# sel_data_imputed data that incoporates the missing values.

# Date : December 5, 2018


# ---- Preliminaries ----
# Loading the packages that we might need :

# library(tidyverse)
# library(ggplot2)
# library(plyr)
# library(dplyr)

# ---- Step 2: Calculating the DESSA Scale Raw Scores ----

# The Scale Raw Scores for the eight scales (Personal
# Responsibility, Optimistic Thinking, Goal-Directed Behavior,
# Social-Awareness, Decision Making, Relationship Skills, Self-
# Awareness, and Self-Management) are obtained by adding the raw
# scores for all of the items that comprise each scale."

# ---- 2.1. Computing the PR, OT, GB, and SO scores for each child ----

# attach(sel_data)  # so objects in the database can be accessed
                  # by simply giving their names... 
                  # i.e Q1 in lieu of sel_data$Q1 ...

attach(sel_data_imputed)

# We will be using the mutate() function from the dplyr package to compute
# The Raw Scrores for each child.
# and add the new column "Personal_Responsibility" to the dataset:

sel_data_imputed <- mutate(sel_data_imputed, Personal_Responsibility = Q1+Q4+Q6+Q20+Q21+Q23+Q24+Q28+Q32+Q35)

# We will do the same for :

# Optimistic Thinking scale
sel_data_imputed <- mutate(sel_data_imputed, Optimistic_Thinking = Q2+Q5+Q7+Q10+Q16+Q30+Q36)

# Goal-Directed Behavior :
sel_data_imputed <- mutate(sel_data_imputed, Goal_Directed_Behavior = Q3+Q9+Q12+Q13+Q14+Q15+Q18+Q26+Q29+Q33)

# Social Awareness :
sel_data_imputed <- mutate(sel_data_imputed, Social_Awareness = Q8+Q11+Q17+Q19+Q22+Q25+Q27+Q31+Q34)

# Decision Making :
sel_data_imputed <- mutate(sel_data_imputed, Decision_Making = Q37+Q39+Q42+Q52+Q65+Q66+Q68+Q69)

# Relationship Skills :
sel_data_imputed <- mutate(sel_data_imputed, Relationship_Skills = Q38+Q40+Q45+Q47+Q50+Q55+Q61+Q64+Q70+Q71)

# Self- Awareness :
sel_data_imputed <- mutate(sel_data_imputed, Self_Awareness = Q41+Q49+Q57+Q58+Q59+Q62+Q63)

# Self-Management :
sel_data_imputed <- mutate(sel_data_imputed, Self_Management = Q43+Q44+Q46+Q48+Q51+Q53+Q54+Q56+Q60+Q67+Q72)

# The new columns should have been added at the end of the dataset :

View(sel_data_imputed)

# ---- 2.3 Calculations of the DESSA Raw Scores of each Child ID ---- 

table_PR_Child <- select(sel_data_imputed, Child_ID, Personal_Responsibility)
table_OT_Child <- select(sel_data_imputed, Child_ID, Optimistic_Thinking)
table_GDB_Child <- select(sel_data_imputed, Child_ID, Goal_Directed_Behavior)
table_SA_Child <- select(sel_data_imputed, Child_ID, Social_Awareness)
table_DM_Child <- select(sel_data_imputed, Child_ID, Decision_Making)
table_RS_Child <- select(sel_data_imputed, Child_ID, Relationship_Skills)
table_SelfAwar_Child <- select(sel_data_imputed, Child_ID, Self_Awareness)
table_SM_Child <- select(sel_data_imputed, Child_ID, Self_Management)

# You can view these table if necessary or export them to excell

# View(table_PR_Child)
# View(table_OT_Child)
# View(table_SA_Child)
# View(table_GDB_Child)
# View(table_DM_Child)
# View(table_RS_Child)
# View(table_SelfAwar_Child)
# View(table_SM_Child)
