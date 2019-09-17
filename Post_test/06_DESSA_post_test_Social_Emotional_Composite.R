# Title:    Devereux Student Strengths Assessment (DESSA) - Spring 2019
# Author:   Annick Eudes JB Research Manager
# Date:     July 23, 2019
# Version:  1.0

# ---- Preliminaries ----
# Loading the packages that we might need :

library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)

# ---- Step 4 : Determining the T-Score and Percentile Score for the Social-Emotional Composite ----
# Computing the Raw Social Emotional Composite 
# The sum of the eight T-scores is treated as the Scale Raw Score for the Social-Emotional Composite.
# Thus, 
raw_SEC <- PRTScore + OTTScore + GDBTScore + SocAW_TScore + DMTScore + SATScore + Self_M_TScore + RSkill_TScore

sel_data_imputed$raw_SEC <- raw_SEC

# View(sel_data_imputed)

# Verifications (Do we have any abnormal numbers?)
max(sel_data_imputed$raw_SEC, na.rm = TRUE)
min(sel_data_imputed$raw_SEC, na.rm = TRUE)


# ---- Computing the T-Scores of the Social Emotional Composite ----
# The SEC T-Scores are computed the same same way as described for the eight scales
# We will use the "|" witch is the "OR" operator in R.
# Thus, 

# This has been verified ; there are no typos!
SEC_Tcore <- 
        ifelse(raw_SEC <= 0 | raw_SEC <= 246, 28,
               ifelse(raw_SEC <= 247 | raw_SEC <= 258, 29,
                      ifelse(raw_SEC <= 259 | raw_SEC <= 266, 30,
                             ifelse(raw_SEC < 267 | raw_SEC <= 270, 31,
                                    ifelse(raw_SEC <= 271 | raw_SEC <= 275, 32,
                                           ifelse(raw_SEC <= 276 | raw_SEC <= 280, 33,
                                                  ifelse(raw_SEC <= 281 | raw_SEC <= 289, 34,
                                                         ifelse(raw_SEC <= 290 | raw_SEC <= 296, 35,
                                                                ifelse(raw_SEC <= 297 | raw_SEC <= 302, 36,
                                                                       ifelse(raw_SEC <= 303 | raw_SEC <= 309, 37,
                                                                              ifelse(raw_SEC <= 310 | raw_SEC <= 315, 38,
                                                                                     ifelse(raw_SEC <= 316 | raw_SEC <= 324, 39,
                                                                                            ifelse(raw_SEC <= 325 | raw_SEC <= 331, 40,
                                                                                                   ifelse(raw_SEC <= 332 | raw_SEC <= 338, 41,
                                                                                                          ifelse(raw_SEC <= 339 | raw_SEC <= 346, 42,
                                                                                                                 ifelse(raw_SEC <= 347 | raw_SEC <= 352, 43,
                                                                                                                        ifelse(raw_SEC <= 353 | raw_SEC <= 360, 44,
                                                                                                                               ifelse(raw_SEC <= 361 | raw_SEC <= 366, 45,
                                                                                                                                      ifelse(raw_SEC <= 367 | raw_SEC <= 373, 46,
                                                                                                                                             ifelse(raw_SEC <= 374 | raw_SEC <= 378, 47,
                                                                                                                                                    ifelse(raw_SEC <= 379 | raw_SEC <= 386, 48,
                                                                                                                                                           ifelse(raw_SEC <= 387 | raw_SEC <= 393, 49,
                                                                                                                                                                  ifelse(raw_SEC <= 394 | raw_SEC <= 401, 50,
                                                                                                                                                                         ifelse(raw_SEC <= 402 | raw_SEC <= 407, 51,
                                                                                                                                                                                ifelse(raw_SEC <= 408 | raw_SEC <= 415, 52,
                                                                                                                                                                                       ifelse(raw_SEC <= 416 | raw_SEC <= 424, 53,
                                                                                                                                                                                              ifelse(raw_SEC <= 425 | raw_SEC <= 432, 54,
                                                                                                                                                                                                     ifelse(raw_SEC <= 433 | raw_SEC <= 438, 55,
                                                                                                                                                                                                            ifelse(raw_SEC <= 439 | raw_SEC <= 447, 56,
                                                                                                                                                                                                                   ifelse(raw_SEC <= 448 | raw_SEC <= 455, 57,
                                                                                                                                                                                                                          ifelse(raw_SEC <= 456 | raw_SEC <= 465, 58,
                                                                                                                                                                                                                                 ifelse(raw_SEC <= 466 | raw_SEC <= 475, 59,
                                                                                                                                                                                                                                        ifelse(raw_SEC <= 476 | raw_SEC <= 485, 60,
                                                                                                                                                                                                                                               ifelse(raw_SEC <= 486 | raw_SEC <= 492, 61,
                                                                                                                                                                                                                                                      ifelse(raw_SEC <= 493 | raw_SEC <= 500, 62,
                                                                                                                                                                                                                                                             ifelse(raw_SEC <= 501 | raw_SEC <= 510, 63,
                                                                                                                                                                                                                                                                    ifelse(raw_SEC <= 511 | raw_SEC <= 518, 64,
                                                                                                                                                                                                                                                                           ifelse(raw_SEC <= 519 | raw_SEC <= 527, 65,
                                                                                                                                                                                                                                                                                  ifelse(raw_SEC <= 528 | raw_SEC <= 532, 66,
                                                                                                                                                                                                                                                                                         ifelse(raw_SEC <= 533 | raw_SEC <= 539, 67,
                                                                                                                                                                                                                                                                                                ifelse(raw_SEC <= 540 | raw_SEC <= 544, 68,
                                                                                                                                                                                                                                                                                                       ifelse(raw_SEC <= 545 | raw_SEC <= 553, 69,
                                                                                                                                                                                                                                                                                                              ifelse(raw_SEC <= 554 | raw_SEC <= 558, 70,
                                                                                                                                                                                                                                                                                                                     ifelse(raw_SEC <= 559 | raw_SEC <= 560, 71,
                                                                                                                                                                                                                                                                                                                            ifelse(raw_SEC <= 561 | raw_SEC <= 576, 72,
                                                                                                                                                                                                                                                                                                                                   ifelse(is.na(raw_SEC),0))))))))))))))))))))))))))))))))))))))))))))))


# We can now add the T-Scores for the Social-Emotional Composite to the dataset :
sel_data_imputed$SEC_Tcore <- SEC_Tcore
# View(sel_data_imputed)

# Verification : by viewing the raw scores and the T-Scores side by side
View(select(sel_data_imputed, Child_ID, raw_SEC, SEC_Tcore))

# ---- Computing the Percentile Score of the Social Emotional Composite ----
# This has been verified ; there are no typos!
SEC_percentile <- 
        ifelse(raw_SEC <= 0 | raw_SEC <= 246, 1,
               ifelse(raw_SEC <= 247 | raw_SEC <= 258, 2,
                      ifelse(raw_SEC <= 259 | raw_SEC <= 266, 2,
                             ifelse(raw_SEC < 267 | raw_SEC <= 270, 3,
                                    ifelse(raw_SEC <= 271 | raw_SEC <= 275, 4,
                                           ifelse(raw_SEC <= 276 | raw_SEC <= 280, 4,
                                                  ifelse(raw_SEC <= 281 | raw_SEC <= 289, 5,
                                                         ifelse(raw_SEC <= 290 | raw_SEC <= 296, 7,
                                                                ifelse(raw_SEC <= 297 | raw_SEC <= 302, 8,
                                                                       ifelse(raw_SEC <= 303 | raw_SEC <= 309, 10,
                                                                              ifelse(raw_SEC <= 310 | raw_SEC <= 315, 12,
                                                                                     ifelse(raw_SEC <= 316 | raw_SEC <= 324, 14,
                                                                                            ifelse(raw_SEC <= 325 | raw_SEC <= 331, 16,
                                                                                                   ifelse(raw_SEC <= 332 | raw_SEC <= 338, 18,
                                                                                                          ifelse(raw_SEC <= 339 | raw_SEC <= 346, 21,
                                                                                                                 ifelse(raw_SEC <= 347 | raw_SEC <= 352, 24,
                                                                                                                        ifelse(raw_SEC <= 353 | raw_SEC <= 360, 27,
                                                                                                                               ifelse(raw_SEC <= 361 | raw_SEC <= 366, 31,
                                                                                                                                      ifelse(raw_SEC <= 367 | raw_SEC <= 373, 34,
                                                                                                                                             ifelse(raw_SEC <= 374 | raw_SEC <= 378, 38,
                                                                                                                                                    ifelse(raw_SEC <= 379 | raw_SEC <= 386, 42,
                                                                                                                                                           ifelse(raw_SEC <= 387 | raw_SEC <= 393, 46,
                                                                                                                                                                  ifelse(raw_SEC <= 394 | raw_SEC <= 401, 50,
                                                                                                                                                                         ifelse(raw_SEC <= 402 | raw_SEC <= 407, 54,
                                                                                                                                                                                ifelse(raw_SEC <= 408 | raw_SEC <= 415, 58,
                                                                                                                                                                                       ifelse(raw_SEC <= 416 | raw_SEC <= 424, 62,
                                                                                                                                                                                              ifelse(raw_SEC <= 425 | raw_SEC <= 432, 66,
                                                                                                                                                                                                     ifelse(raw_SEC <= 433 | raw_SEC <= 438, 69,
                                                                                                                                                                                                            ifelse(raw_SEC <= 439 | raw_SEC <= 447, 73,
                                                                                                                                                                                                                   ifelse(raw_SEC <= 448 | raw_SEC <= 455, 76,
                                                                                                                                                                                                                          ifelse(raw_SEC <= 456 | raw_SEC <= 465, 79,
                                                                                                                                                                                                                                 ifelse(raw_SEC <= 466 | raw_SEC <= 475, 82,
                                                                                                                                                                                                                                        ifelse(raw_SEC <= 476 | raw_SEC <= 485, 84,
                                                                                                                                                                                                                                               ifelse(raw_SEC <= 486 | raw_SEC <= 492, 86,
                                                                                                                                                                                                                                                      ifelse(raw_SEC <= 493 | raw_SEC <= 500, 88,
                                                                                                                                                                                                                                                             ifelse(raw_SEC <= 501 | raw_SEC <= 510, 90,
                                                                                                                                                                                                                                                                    ifelse(raw_SEC <= 511 | raw_SEC <= 518, 92,
                                                                                                                                                                                                                                                                           ifelse(raw_SEC <= 519 | raw_SEC <= 527, 93,
                                                                                                                                                                                                                                                                                  ifelse(raw_SEC <= 528 | raw_SEC <= 532, 95,
                                                                                                                                                                                                                                                                                         ifelse(raw_SEC <= 533 | raw_SEC <= 539, 96,
                                                                                                                                                                                                                                                                                                ifelse(raw_SEC <= 540 | raw_SEC <= 544, 96,
                                                                                                                                                                                                                                                                                                       ifelse(raw_SEC <= 545 | raw_SEC <= 553, 97,
                                                                                                                                                                                                                                                                                                              ifelse(raw_SEC <= 554 | raw_SEC <= 558, 98,
                                                                                                                                                                                                                                                                                                                     ifelse(raw_SEC <= 559 | raw_SEC <= 560, 98,
                                                                                                                                                                                                                                                                                                                            ifelse(raw_SEC <= 561 | raw_SEC <= 576, 99,
                                                                                                                                                                                                                                                                                                                                   ifelse(is.na(raw_SEC),0))))))))))))))))))))))))))))))))))))))))))))))

# We can now include the Percentile Score for the Social-Emotional Composite to the dataset :
sel_data_imputed$SEC_percentile <- SEC_percentile


# Verification : by viewing the raw Social-Emotional Composite and the percentiles side by side
View(select(sel_data_imputed, Child_ID, raw_SEC, SEC_percentile))

# We can also export the new sel dataset with the Questions, Raw scores, T-Scores, Percentiles scores
# and the Raw Social-Emotional Composite, Tscores and Percentiles
openxlsx::write.xlsx(all_Percentile_Scores_table, file = "P:/RE/Private/SEL Assessment Results/DESSA Scoring/results/Post_Test/DESSA_Post_Test_SEC_Scoring.xlsx")
file.exists("P:/RE/Private/SEL Assessment Results/DESSA Scoring/results/Post_Test/DESSA_Post_Test_SEC_Scoring.xlsx")
