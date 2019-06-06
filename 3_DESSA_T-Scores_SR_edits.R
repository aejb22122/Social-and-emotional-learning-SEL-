# Title:    Devereux Student Strengths Assessment (DESSA) - Fall 2018
# Author:   Annick Eudes JB Research Manager
# Date:     November 16, 2018
# Version:  1.0

# Reviewd by : Lisa Lindeman, Director of Research
# Date : January 19, 2019

# ---- Preliminaries ----
# Loading the packages that we might need :

# library(tidyverse)
# library(ggplot2)
# library(plyr)
# library(dplyr)


# ---- Step 3 (A): Determining DESSA T-Scores ----

attach(sel_data_imputed)

# ---- 3.1 T-Scores for of each Child's Personal Responsibility ---- 


# This is just ugly!!!  .... but it works :
# Researching a cleanner way to do this ... 

PRTScore = ifelse(Personal_Responsibility <= 8, 28,
                  ifelse(Personal_Responsibility == 9, 30,
                         ifelse(Personal_Responsibility == 10, 31,
                                ifelse(Personal_Responsibility == 11, 32,
                                       ifelse(Personal_Responsibility == 12, 33,
                                              ifelse(Personal_Responsibility == 13, 35,
                                                     ifelse(Personal_Responsibility == 14, 36,
                                                            ifelse(Personal_Responsibility == 15, 37,
                                                                   ifelse(Personal_Responsibility == 16, 38,
                                                                          ifelse(Personal_Responsibility == 17, 39,
                                                                                 ifelse(Personal_Responsibility == 18, 40,
                                                                                        ifelse(Personal_Responsibility == 19, 41,
                                                                                               ifelse(Personal_Responsibility == 20, 42,
                                                                                                      ifelse(Personal_Responsibility == 21, 44,
                                                                                                             ifelse(Personal_Responsibility == 22, 45,
                                                                                                                    ifelse(Personal_Responsibility == 23, 46,
                                                                                                                           ifelse(Personal_Responsibility == 24, 47,
                                                                                                                                  ifelse(Personal_Responsibility == 25, 48,
                                                                                                                                         ifelse(Personal_Responsibility == 26, 49,
                                                                                                                                                ifelse(Personal_Responsibility == 27, 50,
                                                                                                                                                       ifelse(Personal_Responsibility == 28, 51,
                                                                                                                                                              ifelse(Personal_Responsibility == 29, 53,
                                                                                                                                                                     ifelse(Personal_Responsibility == 30, 54,
                                                                                                                                                                            ifelse(Personal_Responsibility == 31, 56,
                                                                                                                                                                                   ifelse(Personal_Responsibility == 32, 58,
                                                                                                                                                                                          ifelse(Personal_Responsibility == 33, 59,
                                                                                                                                                                                                 ifelse(Personal_Responsibility == 34, 61,
                                                                                                                                                                                                        ifelse(Personal_Responsibility == 35, 62,
                                                                                                                                                                                                               ifelse(Personal_Responsibility == 36, 63,
                                                                                                                                                                                                                      ifelse(Personal_Responsibility == 37, 65,
                                                                                                                                                                                                                             ifelse(Personal_Responsibility == 38, 67,
                                                                                                                                                                                                                                    ifelse(Personal_Responsibility == 39, 69,
                                                                                                                                                                                                                                           ifelse(Personal_Responsibility == 40, 72,
                                                                                                                                                                                                                                                  ifelse(is.na(Personal_Responsibility),0, 0))))))))))))))))))))))))))))))))))
# We can now add the T-Scores for Personal Responsibility to the dataset :
sel_data_imputed$PRTScore <- PRTScore

# To verify that all is correct :

PRTScore_PR_Child <- select(sel_data_imputed, Child_ID, Personal_Responsibility, PRTScore)
# View(PRTScore_PR_Child)

# ---- 3.2 T-Scores for of each Child's Optimistic Thinking ---- 

OTTScore = ifelse(Optimistic_Thinking <=5, 28,
                    ifelse(Optimistic_Thinking == 6, 30,
                           ifelse(Optimistic_Thinking == 7, 31,
                                  ifelse(Optimistic_Thinking == 8, 32,
                                         ifelse(Optimistic_Thinking == 9, 34,
                                                ifelse(Optimistic_Thinking == 10, 35,
                                                     ifelse(Optimistic_Thinking == 11, 37,
                                                          ifelse(Optimistic_Thinking == 12, 39,
                                                               ifelse(Optimistic_Thinking == 13, 40,
                                                                    ifelse(Optimistic_Thinking == 14, 42,
                                                                         ifelse(Optimistic_Thinking == 15, 43,
                                                                                ifelse(Optimistic_Thinking == 16, 45,
                                                                                        ifelse(Optimistic_Thinking == 17, 47,
                                                                                               ifelse(Optimistic_Thinking == 18, 49,
                                                                                                       ifelse(Optimistic_Thinking == 19, 51,
                                                                                                               ifelse(Optimistic_Thinking == 20, 53,
                                                                                                                       ifelse(Optimistic_Thinking == 21, 55,
                                                                                                                              ifelse(Optimistic_Thinking == 22, 57,
                                                                                                                                      ifelse(Optimistic_Thinking == 23, 60,
                                                                                                                                             ifelse(Optimistic_Thinking == 24, 62,
                                                                                                                                                    ifelse(Optimistic_Thinking == 25,64,
                                                                                                                                                            ifelse(Optimistic_Thinking == 26,66,
                                                                                                                                                                    ifelse(Optimistic_Thinking == 27,69,
                                                                                                                                                                           ifelse(Optimistic_Thinking == 28,72,
                                                                                                                                                                                  ifelse(is.na(Personal_Responsibility),0, 0)))))))))))))))))))))))))
# We can now add the T-Scores for Optimistic Thinking to the dataset :
sel_data_imputed$OTTScore <- OTTScore

# To verify that all is correct, we can view the OT, OTTScore side by side :

OTTScore_Child <- select(sel_data_imputed, Child_ID, Optimistic_Thinking, OTTScore)
# View(OTTScore_Child)


# ---- 3.3 T-Scores for of each Child's Goal_Directed_Behavior ---- 

GDBTScore = ifelse(Goal_Directed_Behavior <=6,28,
                    ifelse(Goal_Directed_Behavior == 7,29,
                           ifelse(Goal_Directed_Behavior == 8,30,
                                  ifelse(Goal_Directed_Behavior == 9,31,
                                         ifelse(Goal_Directed_Behavior == 10,32,
                                                ifelse(Goal_Directed_Behavior == 11,33,
                                                       ifelse(Goal_Directed_Behavior == 12,34,
                                                              ifelse(Goal_Directed_Behavior == 13,35,
                                                                     ifelse(Goal_Directed_Behavior == 14,36,
                                                                            ifelse(Goal_Directed_Behavior == 15,37,
                                                                                   ifelse(Goal_Directed_Behavior == 16,38,
                                                                                          ifelse(Goal_Directed_Behavior == 17,40,
                                                                                                 ifelse(Goal_Directed_Behavior == 18,41,
                                                                                                        ifelse(Goal_Directed_Behavior == 19,42,
                                                                                                               ifelse(Goal_Directed_Behavior == 20,43,
                                                                                                                      ifelse(Goal_Directed_Behavior == 21,44,
                                                                                                                             ifelse(Goal_Directed_Behavior == 22,45,
                                                                                                                                    ifelse(Goal_Directed_Behavior == 23,46,
                                                                                                                                           ifelse(Goal_Directed_Behavior == 24,47,
                                                                                                                                                  ifelse(Goal_Directed_Behavior == 25,48,
                                                                                                                                                         ifelse(Goal_Directed_Behavior == 26,49,
                                                                                                                                                                ifelse(Goal_Directed_Behavior == 27,51,
                                                                                                                                                                       ifelse(Goal_Directed_Behavior == 28,52,
                                                                                                                                                                              ifelse(Goal_Directed_Behavior == 29,53,
                                                                                                                                                                                     ifelse(Goal_Directed_Behavior == 30,55,
                                                                                                                                                                                            ifelse(Goal_Directed_Behavior == 31,57,
                                                                                                                                                                                                   ifelse(Goal_Directed_Behavior == 32,58,
                                                                                                                                                                                                          ifelse(Goal_Directed_Behavior == 33,59,
                                                                                                                                                                                                                 ifelse(Goal_Directed_Behavior == 34,61,
                                                                                                                                                                                                                        ifelse(Goal_Directed_Behavior == 35,62,
                                                                                                                                                                                                                               ifelse(Goal_Directed_Behavior == 36,64,
                                                                                                                                                                                                                                      ifelse(Goal_Directed_Behavior == 37,65,
                                                                                                                                                                                                                                             ifelse(Goal_Directed_Behavior == 38,67,
                                                                                                                                                                                                                                                    ifelse(Goal_Directed_Behavior == 39, 69,
                                                                                                                                                                                                                                                           ifelse(Goal_Directed_Behavior == 40, 72,
                                                                                                                                                                                                                                                                  ifelse(is.na(Goal_Directed_Behavior),0, 0))))))))))))))))))))))))))))))))))))
                                                                                                                                                                                                                               
                                                                                                                                                                                                                               
# We can now add the T-Scores for Goal Directed Behavior to the dataset :
sel_data_imputed$GDBTScore <- GDBTScore


# Verifications :
GBTScore_Child <- select(sel_data_imputed, Child_ID, Goal_Directed_Behavior, GDBTScore)

# View(GBTScore_Child)

# ---- 3.4 T-Scores for of each Child's Social Awareness ---- 

SocAW_TScore = ifelse(Social_Awareness <= 6,28,
                     ifelse(Social_Awareness == 7,29,
                            ifelse(Social_Awareness == 8,30,
                                   ifelse(Social_Awareness == 9,31,
                                          ifelse(Social_Awareness == 10,32,
                                                 ifelse(Social_Awareness == 11,33,
                                                        ifelse(Social_Awareness == 12,34,
                                                               ifelse(Social_Awareness == 13,35,
                                                                      ifelse(Social_Awareness == 14,37,
                                                                             ifelse(Social_Awareness == 15,38,
                                                                                    ifelse(Social_Awareness == 16,39,
                                                                                           ifelse(Social_Awareness == 17,40,
                                                                                                  ifelse(Social_Awareness == 18,41,
                                                                                                         ifelse(Social_Awareness == 19,43,
                                                                                                                ifelse(Social_Awareness == 20,44,
                                                                                                                       ifelse(Social_Awareness == 21,45,
                                                                                                                              ifelse(Social_Awareness == 22,47,
                                                                                                                                     ifelse(Social_Awareness == 23,49,
                                                                                                                                            ifelse(Social_Awareness == 24,51,
                                                                                                                                                   ifelse(Social_Awareness == 25,52,
                                                                                                                                                          ifelse(Social_Awareness == 26,54,
                                                                                                                                                                 ifelse(Social_Awareness == 27,56,
                                                                                                                                                                        ifelse(Social_Awareness == 28,58,
                                                                                                                                                                               ifelse(Social_Awareness == 29,60,
                                                                                                                                                                                      ifelse(Social_Awareness == 30,62,
                                                                                                                                                                                             ifelse(Social_Awareness == 31,64,
                                                                                                                                                                                                    ifelse(Social_Awareness == 32,66,
                                                                                                                                                                                                           ifelse(Social_Awareness == 33,68,
                                                                                                                                                                                                                  ifelse(Social_Awareness == 34,69,
                                                                                                                                                                                                                         ifelse(Social_Awareness == 35,71,
                                                                                                                                                                                                                                ifelse(Social_Awareness == 36, 72,
                                                                                                                                                                                                                                       ifelse(is.na(Social_Awareness),0, 0))))))))))))))))))))))))))))))))


# We can now add the T-Scores for Social Awareness to the dataset :
sel_data_imputed$SocAW_TScore <- SocAW_TScore


# To verify that all is correct :
Social_Awareness_tscore <- select(sel_data_imputed, Child_ID, Social_Awareness, SocAW_TScore)
# View(Social_Awareness_tscore)


# ---- 3.5 T-Scores for of each Child's Decision Making ---- 

DMTScore = ifelse(Decision_Making <= 6,28,
                    ifelse(Decision_Making == 7,29,
                              ifelse(Decision_Making == 8,31,
                                     ifelse(Decision_Making == 9,32,
                                            ifelse(Decision_Making == 10,34,
                                                   ifelse(Decision_Making == 11,35,
                                                          ifelse(Decision_Making == 12,37,
                                                                 ifelse(Decision_Making == 13,38,
                                                                        ifelse(Decision_Making == 14,39,
                                                                               ifelse(Decision_Making == 15,41,
                                                                                      ifelse(Decision_Making == 16,42,
                                                                                             ifelse(Decision_Making == 17,43,
                                                                                                    ifelse(Decision_Making == 18,45,
                                                                                                           ifelse(Decision_Making == 19,46,
                                                                                                                  ifelse(Decision_Making == 20,47,
                                                                                                                         ifelse(Decision_Making == 21, 48,
                                                                                                                                ifelse(Decision_Making == 22,50,
                                                                                                                                       ifelse(Decision_Making == 23,52,
                                                                                                                                              ifelse(Decision_Making == 24,54,
                                                                                                                                                     ifelse(Decision_Making == 25,57,
                                                                                                                                                            ifelse(Decision_Making == 26,59,
                                                                                                                                                                  ifelse(Decision_Making == 27,61,
                                                                                                                                                                                 ifelse(Decision_Making == 28,63,
                                                                                                                                                                                        ifelse(Decision_Making == 29,65,
                                                                                                                                                                                               ifelse(Decision_Making == 30,67,
                                                                                                                                                                                                      ifelse(Decision_Making == 31,69,
                                                                                                                                                                                                             ifelse(Decision_Making == 32, 72,
                                                                                                                                                                                                                    ifelse(is.na(Decision_Making),0, 0))))))))))))))))))))))))))))


# We can now add the T-Scores for Decision_Making to the dataset :
sel_data_imputed$DMTScore <- DMTScore


# To verify :
Decision_Making_tscore <- select(sel_data_imputed, Child_ID, Decision_Making, DMTScore)
# View(Decision_Making_tscore)

# ---- 3.6 T-Scores for of each Child's Relationship Skills ---- 

RSkill_TScore = ifelse(Relationship_Skills <= 7,28,
                    ifelse(Relationship_Skills == 8,29,
                           ifelse(Relationship_Skills == 9,30,
                                  ifelse(Relationship_Skills == 10,31,
                                         ifelse(Relationship_Skills == 11,32,
                                                ifelse(Relationship_Skills == 12,33,
                                                       ifelse(Relationship_Skills == 13,34,
                                                              ifelse(Relationship_Skills == 14,35,
                                                                     ifelse(Relationship_Skills == 15,36,
                                                                            ifelse(Relationship_Skills == 16,37,
                                                                                   ifelse(Relationship_Skills == 17,38,
                                                                                          ifelse(Relationship_Skills == 18,39,
                                                                                                 ifelse(Relationship_Skills == 19,40,
                                                                                                        ifelse(Relationship_Skills == 20,42,
                                                                                                               ifelse(Relationship_Skills == 21,43,
                                                                                                                      ifelse(Relationship_Skills == 22,44,
                                                                                                                             ifelse(Relationship_Skills == 23,45,
                                                                                                                                    ifelse(Relationship_Skills == 24,46,
                                                                                                                                           ifelse(Relationship_Skills == 25,47,
                                                                                                                                                  ifelse(Relationship_Skills == 26,48,
                                                                                                                                                         ifelse(Relationship_Skills == 27,50,
                                                                                                                                                                ifelse(Relationship_Skills == 28,52,
                                                                                                                                                                       ifelse(Relationship_Skills == 29,53,
                                                                                                                                                                              ifelse(Relationship_Skills == 30,54,
                                                                                                                                                                                     ifelse(Relationship_Skills == 31,56,
                                                                                                                                                                                            ifelse(Relationship_Skills == 32,58,
                                                                                                                                                                                                   ifelse(Relationship_Skills == 33,59,
                                                                                                                                                                                                          ifelse(Relationship_Skills == 34,60,
                                                                                                                                                                                                                 ifelse(Relationship_Skills == 35,62,
                                                                                                                                                                                                                        ifelse(Relationship_Skills == 36,63,
                                                                                                                                                                                                                               ifelse(Relationship_Skills == 37,64,
                                                                                                                                                                                                                                      ifelse(Relationship_Skills == 38,66,
                                                                                                                                                                                                                                             ifelse(Relationship_Skills == 39,69,
                                                                                                                                                                                                                                                    ifelse(Relationship_Skills == 40,72,
                                                                                                                                                                                                                                                           ifelse(is.na(Relationship_Skills),0,0)))))))))))))))))))))))))))))))))))


# We can now add the T-Scores for Relationship Skills to the dataset :
sel_data_imputed$RSkill_TScore <- RSkill_TScore


# To verify that all is correct :
Relationship_Skills_tscore <- select(sel_data_imputed, Child_ID, Relationship_Skills, RSkill_TScore)
# View(Relationship_Skills_tscore)

# ---- 3.7 T-Scores for of each Child's Self Awareness ---- 

SATScore = ifelse(Self_Awareness <= 3,28,
                    ifelse(Self_Awareness == 4,29,
                           ifelse(Self_Awareness == 5,30,
                                  ifelse(Self_Awareness == 6,31,
                                         ifelse(Self_Awareness == 7,33,
                                                ifelse(Self_Awareness == 8, 34,
                                                       ifelse(Self_Awareness == 9,35,
                                                              ifelse(Self_Awareness == 10,36,
                                                                     ifelse(Self_Awareness == 11,38,
                                                                            ifelse(Self_Awareness == 12,40,
                                                                                   ifelse(Self_Awareness == 13,41,
                                                                                          ifelse(Self_Awareness == 14,42,
                                                                                                 ifelse(Self_Awareness == 15,44,
                                                                                                        ifelse(Self_Awareness == 16,46,
                                                                                                               ifelse(Self_Awareness == 17,48,
                                                                                                                      ifelse(Self_Awareness == 18,50,
                                                                                                                             ifelse(Self_Awareness == 19,52,
                                                                                                                                    ifelse(Self_Awareness == 20,55,
                                                                                                                                           ifelse(Self_Awareness == 21,57,
                                                                                                                                                  ifelse(Self_Awareness == 22,59,
                                                                                                                                                         ifelse(Self_Awareness == 23,61,
                                                                                                                                                                ifelse(Self_Awareness == 24,63,
                                                                                                                                                                       ifelse(Self_Awareness == 25,65,
                                                                                                                                                                              ifelse(Self_Awareness == 26,67,
                                                                                                                                                                                     ifelse(Self_Awareness == 27,70,
                                                                                                                                                                                            ifelse(Self_Awareness == 28,72,
                                                                                                                                                                                                   ifelse(is.na(Self_Awareness),0,0)))))))))))))))))))))))))))


# We can now add the T-Scores for Self_Awareness to the dataset :
sel_data_imputed$SATScore <- SATScore


# To verify :
Self_Awareness_tscore <- select(sel_data_imputed, Child_ID, Self_Awareness, SATScore)
# View(Self_Awareness_tscore)

# ---- 3.8 T-Scores for of each Child's Self_Management ---- 
Self_M_TScore = ifelse(Self_Management <= 8, 28,
                        ifelse(Self_Management == 9,29,
                               ifelse(Self_Management == 10, 30,
                                      ifelse(Self_Management == 11, 31,
                                             ifelse(Self_Management == 12, 32,
                                                    ifelse(Self_Management == 13, 33,
                                                           ifelse(Self_Management == 14, 34,
                                                                  ifelse(Self_Management == 15, 35,
                                                                         ifelse(Self_Management == 16, 36,
                                                                                ifelse(Self_Management == 17, 37,
                                                                                       ifelse(Self_Management == 18, 38,
                                                                                              ifelse(Self_Management == 19, 39,
                                                                                                     ifelse(Self_Management == 20, 40,
                                                                                                            ifelse(Self_Management == 21, 41,
                                                                                                                   ifelse(Self_Management == 22, 42,
                                                                                                                          ifelse(Self_Management == 23, 43,
                                                                                                                                 ifelse(Self_Management == 24, 44,
                                                                                                                                        ifelse(Self_Management == 25, 45,
                                                                                                                                               ifelse(Self_Management == 26, 46,
                                                                                                                                                      ifelse(Self_Management == 27, 48,
                                                                                                                                                             ifelse(Self_Management == 28, 49,
                                                                                                                                                                    ifelse(Self_Management == 29, 50,
                                                                                                                                                                           ifelse(Self_Management == 30, 52,
                                                                                                                                                                                  ifelse(Self_Management == 31, 53,
                                                                                                                                                                                         ifelse(Self_Management == 32, 54,
                                                                                                                                                                                                ifelse(Self_Management == 33, 56,
                                                                                                                                                                                                       ifelse(Self_Management == 34, 58,
                                                                                                                                                                                                              ifelse(Self_Management == 35, 59,
                                                                                                                                                                                                                     ifelse(Self_Management == 36, 60,
                                                                                                                                                                                                                            ifelse(Self_Management == 37, 61,
                                                                                                                                                                                                                                   ifelse(Self_Management == 38, 63,
                                                                                                                                                                                                                                          ifelse(Self_Management == 39, 64,
                                                                                                                                                                                                                                          ifelse(Self_Management == 40, 66,
                                                                                                                                                                                                                                                 ifelse(Self_Management == 41, 68,
                                                                                                                                                                                                                                                        ifelse(Self_Management == 42, 69,
                                                                                                                                                                                                                                                               ifelse(Self_Management == 43, 71,
                                                                                                                                                                                                                                                                      ifelse(Self_Management == 44, 72,
                                                                                                                                                                                                                                                                             ifelse(is.na(Self_Awareness),0,0))))))))))))))))))))))))))))))))))))))



# We can now add the T-Scores for Self_Management to the dataset :
sel_data_imputed$Self_M_TScore <- Self_M_TScore 

 

# To verify that all is correct :
Self_Management_tscore <- select(sel_data_imputed, Child_ID, Self_Management, Self_M_TScore)
# View(Self_Management_tscore)

# ---- T-Scores Results summary ----
# We will export the results in a excel file:
# Creating a dataframe with all the T-Scores and exporting it to excel file :
table_all_TScores <- select(sel_data_imputed, Child_ID, PRTScore, OTTScore, GDBTScore, SocAW_TScore, DMTScore, SATScore, Self_M_TScore, RSkill_TScore)
# View(table_all_TScores)   # Viewing the child ID with all the TScores


# This is part of the tidyverse package type help(readxl) for more info...
# openxlsx::write.xlsx(table_all_TScores, file = "P:/RE/Private/Ten Year Report/Data and Analysis/SEL/Reports to Programs/2018-2019 DESSA/DESSA_2018_2019/results/tscores.xlsx")
# file.exists("P:/RE/Private/Ten Year Report/Data and Analysis/SEL/Reports to Programs/2018-2019 DESSA/DESSA_2018_2019/results/tscores.xlsx")





