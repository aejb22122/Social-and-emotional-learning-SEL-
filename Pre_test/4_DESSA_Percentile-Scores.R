# Title:    Devereux Student Strengths Assessment (DESSA) - Fall 2018
# Author:   Annick Eudes JB Research Manager
# Date:     November 16, 2018
# Version:  1.0

# ---- Preliminaries ----
# Loading the packages that we might need :
# Don't forget to load these at each session....

# library(tidyverse)
# library(ggplot2)
# library(plyr)
# library(dplyr)


# ---- Step 3 (B) : Determining DESSA Percentile Scores ----
# attach(sel_data_imputed) # Use this if you are starting a new R session.

# ---- 3.1 Percentile-Scores for of each Child's Personal Responsibility ---- 

PRPercentile <- ifelse(Personal_Responsibility <= 8, 1,
                    ifelse(Personal_Responsibility == 9, 2,
                           ifelse(Personal_Responsibility == 10, 3,
                                  ifelse(Personal_Responsibility == 11, 4,
                                         ifelse(Personal_Responsibility == 12, 4,
                                                ifelse(Personal_Responsibility == 13, 7,
                                                       ifelse(Personal_Responsibility == 14, 8,
                                                              ifelse(Personal_Responsibility == 15, 10,
                                                                     ifelse(Personal_Responsibility == 16, 12,
                                                                            ifelse(Personal_Responsibility == 17, 14,
                                                                                   ifelse(Personal_Responsibility == 18, 16,
                                                                                          ifelse(Personal_Responsibility == 19, 18,
                                                                                                 ifelse(Personal_Responsibility == 20, 21,
                                                                                                        ifelse(Personal_Responsibility == 21, 27,
                                                                                                               ifelse(Personal_Responsibility == 22, 31,
                                                                                                                      ifelse(Personal_Responsibility == 23, 34,
                                                                                                                             ifelse(Personal_Responsibility == 24, 38,
                                                                                                                                    ifelse(Personal_Responsibility == 25, 42,
                                                                                                                                           ifelse(Personal_Responsibility == 26, 46,
                                                                                                                                                  ifelse(Personal_Responsibility == 27, 50,
                                                                                                                                                         ifelse(Personal_Responsibility == 28, 54,
                                                                                                                                                                ifelse(Personal_Responsibility == 29, 62,
                                                                                                                                                                              ifelse(Personal_Responsibility == 30, 66,
                                                                                                                                                                                     ifelse(Personal_Responsibility == 31, 73,
                                                                                                                                                                                            ifelse(Personal_Responsibility == 32, 79,
                                                                                                                                                                                                   ifelse(Personal_Responsibility == 33, 82,
                                                                                                                                                                                                          ifelse(Personal_Responsibility == 34, 86,
                                                                                                                                                                                                                 ifelse(Personal_Responsibility == 35, 88,
                                                                                                                                                                                                                        ifelse(Personal_Responsibility == 36, 90,
                                                                                                                                                                                                                               ifelse(Personal_Responsibility == 37, 93,
                                                                                                                                                                                                                                      ifelse(Personal_Responsibility == 38, 96,
                                                                                                                                                                                                                                             ifelse(Personal_Responsibility == 39, 97,
                                                                                                                                                                                                                                                    ifelse(Personal_Responsibility == 40, 99,
                                                                                                                                                                                                                                                           ifelse(is.na(Personal_Responsibility),0, 0))))))))))))))))))))))))))))))))))
                                                                                                                                                                                                                                             
# We can now add the Percentile-Scores for Personal Responsibility onto the dataset :
sel_data_imputed$PRPercentile <- PRPercentile
# View(sel_data_impute)

# Verifications :
table_PRPercentile_PR_Child <- select(sel_data_imputed, Child_ID, Personal_Responsibility, PRPercentile)
# View(table_PRPercentile_PR_Child)


# ---- 3.2 Percentile Scores for of each Child's Optimistic Thinking ---- 

Percentile_OT = ifelse(Optimistic_Thinking <=5, 1,
                    ifelse(Optimistic_Thinking == 6, 2,
                           ifelse(Optimistic_Thinking == 7, 3,
                                  ifelse(Optimistic_Thinking == 8, 4,
                                         ifelse(Optimistic_Thinking == 9, 5,
                                                ifelse(Optimistic_Thinking == 10, 7,
                                                       ifelse(Optimistic_Thinking == 11, 10,
                                                              ifelse(Optimistic_Thinking == 12, 14,
                                                                     ifelse(Optimistic_Thinking == 13, 16,
                                                                            ifelse(Optimistic_Thinking == 14, 21,
                                                                                   ifelse(Optimistic_Thinking == 15, 24,
                                                                                          ifelse(Optimistic_Thinking == 16, 31,
                                                                                                 ifelse(Optimistic_Thinking == 17, 38,
                                                                                                        ifelse(Optimistic_Thinking == 18, 46,
                                                                                                               ifelse(Optimistic_Thinking == 19, 54,
                                                                                                                      ifelse(Optimistic_Thinking == 20, 62,
                                                                                                                             ifelse(Optimistic_Thinking == 21, 69,
                                                                                                                                    ifelse(Optimistic_Thinking == 22, 76,
                                                                                                                                           ifelse(Optimistic_Thinking == 23, 84,
                                                                                                                                                  ifelse(Optimistic_Thinking == 24, 88,
                                                                                                                                                         ifelse(Optimistic_Thinking == 25, 92,
                                                                                                                                                                ifelse(Optimistic_Thinking == 26, 95,
                                                                                                                                                                       ifelse(Optimistic_Thinking == 27, 97,
                                                                                                                                                                              ifelse(Optimistic_Thinking == 28,99,
                                                                                                                                                                                            ifelse(is.na(Personal_Responsibility),0, 0)))))))))))))))))))))))))
# We can now add the Percentile Scores for Optimistic Thinking to the dataset :
sel_data_imputed$Percentile_OT <- Percentile_OT


# Verification :

table_Percentile_OT_Child <- select(sel_data_imputed, Child_ID, Optimistic_Thinking, Percentile_OT)
# View(table_Percentile_OT_Child)


# ---- 3.3 Percentile for of each Child's Goal_Directed_Behavior ---- 

Percentile_GDB = ifelse(Goal_Directed_Behavior <= 6, 1,
                     ifelse(Goal_Directed_Behavior == 7,2,
                            ifelse(Goal_Directed_Behavior == 8, 2,
                                   ifelse(Goal_Directed_Behavior == 9,3,
                                          ifelse(Goal_Directed_Behavior == 10,4,
                                                 ifelse(Goal_Directed_Behavior == 11, 4,
                                                        ifelse(Goal_Directed_Behavior == 12, 5,
                                                               ifelse(Goal_Directed_Behavior == 13, 7,
                                                                      ifelse(Goal_Directed_Behavior == 14, 8,
                                                                             ifelse(Goal_Directed_Behavior == 15, 10,
                                                                                    ifelse(Goal_Directed_Behavior == 16, 12,
                                                                                           ifelse(Goal_Directed_Behavior == 17, 16,
                                                                                                  ifelse(Goal_Directed_Behavior == 18, 18,
                                                                                                         ifelse(Goal_Directed_Behavior == 19, 21,
                                                                                                                ifelse(Goal_Directed_Behavior == 20, 24,
                                                                                                                       ifelse(Goal_Directed_Behavior == 21, 27,
                                                                                                                              ifelse(Goal_Directed_Behavior == 22, 31,
                                                                                                                                     ifelse(Goal_Directed_Behavior == 23, 34,
                                                                                                                                            ifelse(Goal_Directed_Behavior == 24,38,
                                                                                                                                                   ifelse(Goal_Directed_Behavior == 25,42,
                                                                                                                                                          ifelse(Goal_Directed_Behavior == 26,46,
                                                                                                                                                                 ifelse(Goal_Directed_Behavior == 27,54,
                                                                                                                                                                        ifelse(Goal_Directed_Behavior == 28,58,
                                                                                                                                                                               ifelse(Goal_Directed_Behavior == 29,62,
                                                                                                                                                                                      ifelse(Goal_Directed_Behavior == 30,69,
                                                                                                                                                                                             ifelse(Goal_Directed_Behavior == 31,76,
                                                                                                                                                                                                    ifelse(Goal_Directed_Behavior == 32, 79,
                                                                                                                                                                                                           ifelse(Goal_Directed_Behavior == 33,82,
                                                                                                                                                                                                                  ifelse(Goal_Directed_Behavior == 34, 86,
                                                                                                                                                                                                                         ifelse(Goal_Directed_Behavior == 35, 88,
                                                                                                                                                                                                                                ifelse(Goal_Directed_Behavior == 36, 92,
                                                                                                                                                                                                                                       ifelse(Goal_Directed_Behavior == 37, 93,
                                                                                                                                                                                                                                              ifelse(Goal_Directed_Behavior == 38, 96,
                                                                                                                                                                                                                                                     ifelse(Goal_Directed_Behavior == 39, 97,
                                                                                                                                                                                                                                                            ifelse(Goal_Directed_Behavior == 40, 99,
                                                                                                                                                                                                                                                                   ifelse(is.na(Goal_Directed_Behavior),0, 0))))))))))))))))))))))))))))))))))))


# We can now add the Percentile Scores for Goal Directed Behavior to the dataset :
sel_data_imputed$Percentile_GDB <- Percentile_GDB


# Verification :
table_Percentile_GDB_Child <- select(sel_data_imputed, Child_ID, Goal_Directed_Behavior, Percentile_GDB)
# View(table_Percentile_GDB_Child)


# ---- 3.4 Percentile for of each Child's Social Awareness ---- 

Percentile_SocAw = ifelse(Social_Awareness <= 6,1,
                       ifelse(Social_Awareness == 7,2,
                              ifelse(Social_Awareness == 8,2,
                                     ifelse(Social_Awareness == 9,3,
                                            ifelse(Social_Awareness == 10,4,
                                                   ifelse(Social_Awareness == 11,4,
                                                          ifelse(Social_Awareness == 12,5,
                                                                 ifelse(Social_Awareness == 13,7,
                                                                        ifelse(Social_Awareness == 14,10,
                                                                               ifelse(Social_Awareness == 15,12,
                                                                                      ifelse(Social_Awareness == 16,14,
                                                                                             ifelse(Social_Awareness == 17, 16,
                                                                                                    ifelse(Social_Awareness == 18, 18,
                                                                                                           ifelse(Social_Awareness == 19, 24,
                                                                                                                  ifelse(Social_Awareness == 20, 27,
                                                                                                                         ifelse(Social_Awareness == 21, 31,
                                                                                                                                ifelse(Social_Awareness == 22, 38,
                                                                                                                                       ifelse(Social_Awareness == 23, 46,
                                                                                                                                              ifelse(Social_Awareness == 24,54,
                                                                                                                                                     ifelse(Social_Awareness == 25,58,
                                                                                                                                                            ifelse(Social_Awareness == 26, 66,
                                                                                                                                                                   ifelse(Social_Awareness == 27, 73,
                                                                                                                                                                          ifelse(Social_Awareness == 28, 79,
                                                                                                                                                                                 ifelse(Social_Awareness == 29, 84,
                                                                                                                                                                                        ifelse(Social_Awareness == 30, 88,
                                                                                                                                                                                               ifelse(Social_Awareness == 31, 92,
                                                                                                                                                                                                      ifelse(Social_Awareness == 32, 95,
                                                                                                                                                                                                             ifelse(Social_Awareness == 33, 96,
                                                                                                                                                                                                                    ifelse(Social_Awareness == 34, 97,
                                                                                                                                                                                                                           ifelse(Social_Awareness == 35, 98,
                                                                                                                                                                                                                                  ifelse(Social_Awareness == 36, 99,
                                                                                                                                                                                                                                         ifelse(is.na(Social_Awareness),0, 0))))))))))))))))))))))))))))))))


# We can now add the Percentile Scores for Social Awareness to the dataset :
sel_data_imputed$Percentile_SocAw <- Percentile_SocAw
# View(sel_data_imputed)

# Verification :
Social_Awareness_Percentile <- select(sel_data_imputed, Child_ID, Social_Awareness, Percentile_SocAw)
# View(Social_Awareness_Percentile)

# ---- 3.5 Percentile for of each Child's Decision Making ---- 

Percentile_DM = ifelse(Decision_Making <= 6, 1,
                    ifelse(Decision_Making == 7, 2,
                           ifelse(Decision_Making == 8, 3,
                                  ifelse(Decision_Making == 9, 4,
                                         ifelse(Decision_Making == 10, 5,
                                                ifelse(Decision_Making == 11, 7,
                                                       ifelse(Decision_Making == 12, 10,
                                                              ifelse(Decision_Making == 13, 12,
                                                                     ifelse(Decision_Making == 14, 14,
                                                                            ifelse(Decision_Making == 15, 18,
                                                                                   ifelse(Decision_Making == 16, 21,
                                                                                          ifelse(Decision_Making == 17, 24,
                                                                                                 ifelse(Decision_Making == 18, 31,
                                                                                                        ifelse(Decision_Making == 19, 34,
                                                                                                               ifelse(Decision_Making == 20, 38,
                                                                                                                      ifelse(Decision_Making == 21, 42,
                                                                                                                             ifelse(Decision_Making == 22,50,
                                                                                                                                    ifelse(Decision_Making == 23, 58,
                                                                                                                                           ifelse(Decision_Making == 24, 66,
                                                                                                                                                  ifelse(Decision_Making == 25, 76,
                                                                                                                                                         ifelse(Decision_Making == 26, 82,
                                                                                                                                                                ifelse(Decision_Making == 27, 86,
                                                                                                                                                                       ifelse(Decision_Making == 28, 90,
                                                                                                                                                                              ifelse(Decision_Making == 29,93,
                                                                                                                                                                                     ifelse(Decision_Making == 30, 96,
                                                                                                                                                                                            ifelse(Decision_Making == 31, 97,
                                                                                                                                                                                                   ifelse(Decision_Making == 32,99,
                                                                                                                                                                                                          ifelse(is.na(Decision_Making),0, 0))))))))))))))))))))))))))))


# We can now add the Percentiles for Decision Making to the dataset :
sel_data_imputed$Percentile_DM <- Percentile_DM
# View(sel_data_imputed)

# Verification :
Decision_Making_Percentile <- select(sel_data_imputed, Child_ID, Decision_Making, Percentile_DM)
# View(Decision_Making_Percentile)


# ---- 3.6 Percentile for of each Child's Relationship Skills ---- 

Percentile_RS = ifelse(Relationship_Skills <= 7, 1,
                    ifelse(Relationship_Skills == 8,2,
                           ifelse(Relationship_Skills == 9, 2,
                                  ifelse(Relationship_Skills == 10, 3,
                                         ifelse(Relationship_Skills == 11, 4,
                                                ifelse(Relationship_Skills == 12, 4,
                                                       ifelse(Relationship_Skills == 13, 5,
                                                              ifelse(Relationship_Skills == 14, 7,
                                                                     ifelse(Relationship_Skills == 15, 8,
                                                                            ifelse(Relationship_Skills == 16, 10,
                                                                                   ifelse(Relationship_Skills == 17, 12,
                                                                                          ifelse(Relationship_Skills == 18, 14,
                                                                                                 ifelse(Relationship_Skills == 19, 16,
                                                                                                        ifelse(Relationship_Skills == 20, 21,
                                                                                                               ifelse(Relationship_Skills == 21, 24,
                                                                                                                      ifelse(Relationship_Skills == 22, 27,
                                                                                                                             ifelse(Relationship_Skills == 23, 31,
                                                                                                                                    ifelse(Relationship_Skills == 24, 34,
                                                                                                                                           ifelse(Relationship_Skills == 25, 38,
                                                                                                                                                  ifelse(Relationship_Skills == 26, 42,
                                                                                                                                                         ifelse(Relationship_Skills == 27,50,
                                                                                                                                                                ifelse(Relationship_Skills == 28,58,
                                                                                                                                                                       ifelse(Relationship_Skills == 29, 62,
                                                                                                                                                                              ifelse(Relationship_Skills == 30, 66,
                                                                                                                                                                                     ifelse(Relationship_Skills == 31, 73,
                                                                                                                                                                                            ifelse(Relationship_Skills == 32, 79,
                                                                                                                                                                                                   ifelse(Relationship_Skills == 33, 82,
                                                                                                                                                                                                          ifelse(Relationship_Skills == 34, 84,
                                                                                                                                                                                                                 ifelse(Relationship_Skills == 35, 88,
                                                                                                                                                                                                                        ifelse(Relationship_Skills == 36, 90,
                                                                                                                                                                                                                               ifelse(Relationship_Skills == 37, 92,
                                                                                                                                                                                                                                      ifelse(Relationship_Skills == 38, 95,
                                                                                                                                                                                                                                             ifelse(Relationship_Skills == 39, 97,
                                                                                                                                                                                                                                                    ifelse(Relationship_Skills == 40, 99,
                                                                                                                                                                                                                                                           ifelse(is.na(Relationship_Skills),0,0)))))))))))))))))))))))))))))))))))


# We can now add the Percentile for Relationship Skills to the dataset :
sel_data_imputed$Percentile_RS <- Percentile_RS
# View(sel_data_imputed)

# Verification :
Relationship_Skills_Percentile <- select(sel_data_imputed, Child_ID, Relationship_Skills, Percentile_RS)
# View(Relationship_Skills_Percentile)

# ---- 3.7 Percentile for of each Child's Self Awareness ---- 

Percentile_Self_Aw = ifelse(Self_Awareness <= 3, 1,
                         ifelse(Self_Awareness == 4, 2,
                                ifelse(Self_Awareness == 5, 2,
                                       ifelse(Self_Awareness == 6,3,
                                              ifelse(Self_Awareness == 7, 4,
                                                     ifelse(Self_Awareness == 8, 5,
                                                            ifelse(Self_Awareness == 9, 7,
                                                                   ifelse(Self_Awareness == 10, 8,
                                                                          ifelse(Self_Awareness == 11, 12,
                                                                                 ifelse(Self_Awareness == 12, 16,
                                                                                        ifelse(Self_Awareness == 13, 18,
                                                                                               ifelse(Self_Awareness == 14, 21,
                                                                                                      ifelse(Self_Awareness == 15, 27,
                                                                                                             ifelse(Self_Awareness == 16, 34,
                                                                                                                    ifelse(Self_Awareness == 17, 42,
                                                                                                                           ifelse(Self_Awareness == 18,50,
                                                                                                                                  ifelse(Self_Awareness == 19,58,
                                                                                                                                         ifelse(Self_Awareness == 20,69,
                                                                                                                                                ifelse(Self_Awareness == 21,76,
                                                                                                                                                       ifelse(Self_Awareness == 22,82,
                                                                                                                                                              ifelse(Self_Awareness == 23,86,
                                                                                                                                                                     ifelse(Self_Awareness == 24, 90,
                                                                                                                                                                            ifelse(Self_Awareness == 25, 93,
                                                                                                                                                                                   ifelse(Self_Awareness == 26, 96,
                                                                                                                                                                                          ifelse(Self_Awareness == 27, 98,
                                                                                                                                                                                                 ifelse(Self_Awareness == 28,99,
                                                                                                                                                                                                        ifelse(is.na(Self_Awareness),0,0)))))))))))))))))))))))))))


# We can now add the Percentile for Self Awareness to the dataset :
sel_data_imputed$Percentile_Self_Aw <- Percentile_Self_Aw
# View(sel_data_imputed)

# Verification : by viewing the raw scores and the percentiles side by side
Self_Awareness_Percentile <- select(sel_data_imputed, Child_ID, Self_Awareness, Percentile_Self_Aw)
# View(Self_Awareness_Percentile)

# ---- 3.8 Percentile for of each Child's Self_Management ---- 

Percentile_Self_M = ifelse(Self_Management <= 8, 1,
                        ifelse(Self_Management == 9,2,
                               ifelse(Self_Management == 10, 2,
                                      ifelse(Self_Management == 11, 3,
                                             ifelse(Self_Management == 12, 4,
                                                    ifelse(Self_Management == 13, 4,
                                                           ifelse(Self_Management == 14, 5,
                                                                  ifelse(Self_Management == 15, 7,
                                                                         ifelse(Self_Management == 16, 8,
                                                                                ifelse(Self_Management == 17, 10,
                                                                                       ifelse(Self_Management == 18, 12,
                                                                                              ifelse(Self_Management == 19, 14,
                                                                                                     ifelse(Self_Management == 20, 16,
                                                                                                            ifelse(Self_Management == 21, 18,
                                                                                                                   ifelse(Self_Management == 22, 21,
                                                                                                                          ifelse(Self_Management == 23, 24,
                                                                                                                                 ifelse(Self_Management == 24, 27,
                                                                                                                                        ifelse(Self_Management == 25, 31,
                                                                                                                                               ifelse(Self_Management == 26, 34,
                                                                                                                                                      ifelse(Self_Management == 27, 42,
                                                                                                                                                             ifelse(Self_Management == 28, 46,
                                                                                                                                                                    ifelse(Self_Management == 29, 50,
                                                                                                                                                                           ifelse(Self_Management == 30, 58,
                                                                                                                                                                                  ifelse(Self_Management == 31, 62,
                                                                                                                                                                                         ifelse(Self_Management == 32, 66,
                                                                                                                                                                                                ifelse(Self_Management == 33, 73,
                                                                                                                                                                                                       ifelse(Self_Management == 34, 79,
                                                                                                                                                                                                              ifelse(Self_Management == 35, 82,
                                                                                                                                                                                                                     ifelse(Self_Management == 36, 84,
                                                                                                                                                                                                                            ifelse(Self_Management == 37, 86,
                                                                                                                                                                                                                                   ifelse(Self_Management == 38, 90,
                                                                                                                                                                                                                                          ifelse(Self_Management == 39, 92,
                                                                                                                                                                                                                                                 ifelse(Self_Management == 40, 95,
                                                                                                                                                                                                                                                        ifelse(Self_Management == 41, 96,
                                                                                                                                                                                                                                                               ifelse(Self_Management == 42, 97,
                                                                                                                                                                                                                                                                      ifelse(Self_Management == 43, 98,
                                                                                                                                                                                                                                                                             ifelse(Self_Management == 44, 99,
                                                                                                                                                                                                                                                                                    ifelse(is.na(Self_Awareness),0,0))))))))))))))))))))))))))))))))))))))
                                                                                                                                                                                                                                                                             



# We can now add the Percentile Score for Self_Management to the dataset :
sel_data_imputed$Percentile_Self_M <- Percentile_Self_M
# View(sel_data_imputed)


# Verification :
# Remember that the selection function uses the columns of the dataset and not the names of the R objects!
Self_Management_Percentile <- select(sel_data_imputed, Child_ID, Self_Management, Percentile_Self_M)
# View(Self_Management_Percentile)

# ---- Percentiles Score Results summary ----

# Exporting the results of the Percentiles in a excel spreadsheet

# Creating a dataframe with all the Percentiles of the 8 dimentions and exporting it to excel spreadsheet :
all_Percentile_Scores_table <- select(sel_data_imputed, Child_ID, PRPercentile, Percentile_OT, Percentile_GDB, Percentile_SocAw, Percentile_DM, Percentile_RS, Percentile_Self_Aw, Percentile_Self_M)
# View(all_Percentile_Scores_table)

# We can now export the results into a excel file :
openxlsx::write.xlsx(all_Percentile_Scores_table, file = "P:/RE/Private/Ten Year Report/Data and Analysis/SEL/Reports to Programs/2018-2019 DESSA/DESSA_2018_2019/results/percentile_scores.xlsx")
file.exists("P:/RE/Private/Ten Year Report/Data and Analysis/SEL/Reports to Programs/2018-2019 DESSA/DESSA_2018_2019/results/percentile_scores.xlsx")
