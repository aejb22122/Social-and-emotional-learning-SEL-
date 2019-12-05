# Title:    Devereux Student Strengths Assessment (DESSA) - Fall 2018
# Author:   Annick Eudes JB Research Manager
# Date:     November 09, 2018
# Version:  1.0

# ---- Preliminaries ----
# If you don't have these packages installed, use :
#install.packages("tidyverse") # Data cleaning and alot more
#install.packages("ggplot2")   # Graphs
#install.packages("plyr")


# Loading the packages that we might need :

library(tidyverse)
library(ggplot2)
library(plyr)


# Setting the working directory for this project in the "data" folder of this project:
# Last download from https://www.snapsurveys.com/:
# date()
# today <- date()

setwd("P:/RE/Private/Ten Year Report/Data and Analysis/SEL/Reports to Programs/2018-2019 DESSA/DESSA_2018_2019/data")

# ---- Step 1. Data management (re-coding the variables) -----
# Importing the dataset
# I've transformed the data from .*csv format to an excel format *.xlsx 
# We used the importing option form R-studio to convert the ID.date column to a date format :


library(readxl)
sel_data <- read_excel("sel_youth_assessment_fall_2018.xlsx",
col_types = c("text", "text", "date",
"text", "date", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text", "text", "text",
"text", "text", "text"))
View(sel_data)


# First looks :

dim(sel_data)
str(sel_data)


# ---- 1.1 Cleaning the dataset ----
# we will remove some of the columns that we might not use:
# sel_data$ID.format <- NULL
# sel_data$ID.completed <- NULL
# sel_data$ID.end <- NULL
# sel_data$ID.start <- NULL
# sel_data$ID.time <- NULL
# sel_data$ID.endDate <- NULL
# sel_data$Your_ID2 <- NULL   # This column is the same has the Your_ID column. 
                              # It is used to compare the ID entered by the rators.


# ---- 1.2 Recoding of the variables ----

# From the dplyr package, the mutate_at() requires you to specify columns inside a vars() argument 
# for which the mutation will be executed :

# df <- df %>%
#   mutate_at(c("arg1", "arg2", "arg3"), funs(recode(., "1" = -1, "2" = 1, .default = NaN)))
# 
# df <- df %>% mutate_at(c("Q3a", "Q3bt"), 
#                        funs(recode(., "Strongly Disagree" = 1,
#                                    "Disagree" = 2, 
#                                    "Neither agree or disagree" = 3, 
#                                    "Agree" = 4, 
#                                    "Strongly agree" = 5,
#                                    .default = 0)))

# sel_data <- sel_data %>% mutate_at(vars(Q3a:Q3bt), # The variables from Q3a to Q3bt.
#             funs(recode(Q3a:Q3bt, "Strongly Disagree" = "1",
#                         "Disagree"  = "2",
#                         "Neither agree or disagree" = "3",
#                         "Agree" = "4",
#                         "Strongly agree" = "5")))


sel_data$Q2[sel_data$Q2 == "Strongly Disagree"] <- 1
sel_data$Q2[sel_data$Q2 == "Disagree"] <- 2
sel_data$Q2[sel_data$Q2 == "Neither agree or disagree"] <- 3
sel_data$Q2[sel_data$Q2 == "Agree"] <- 4
sel_data$Q2[sel_data$Q2 == "Strongly agree"] <- 5

# From this point and moving foward, the recoding of the variables are similar...
# From question Q3a to question Q3bt and Q4 we will use the following codes :
# 0 = Never, 1 = Rarely, 2 = Occasionally, 3 = Frequently, 4 = Very Frequently
# NA = Unable to Answer

# Recoding of Q3a	1 "remember important information?"

sel_data$Q3a[sel_data$Q3a == "Never"] <- 0
sel_data$Q3a[sel_data$Q3a == "Rarely"] <- 1
sel_data$Q3a[sel_data$Q3a == "Occasionally"] <- 2
sel_data$Q3a[sel_data$Q3a == "Frequently"] <- 3
sel_data$Q3a[sel_data$Q3a == "Very Frequently"] <- 4
sel_data$Q3a[sel_data$Q3a == "Unable to Answer"] <- NA

# Recoding of Q3b	(Q2) carry himself or herself with confidence?

sel_data$Q3b[sel_data$Q3b == "Never"] <- 0
sel_data$Q3b[sel_data$Q3b == "Rarely"] <- 1
sel_data$Q3b[sel_data$Q3b == "Occasionally"] <- 2
sel_data$Q3b[sel_data$Q3b == "Frequently"] <- 3
sel_data$Q3b[sel_data$Q3b == "Very Frequently"] <- 4
sel_data$Q3b[sel_data$Q3b == "Unable to Answer"] <- NA

# Recoding of Q3c	(Q3) keep trying when unsuccessful?

sel_data$Q3c[sel_data$Q3c == "Never"] <- 0
sel_data$Q3c[sel_data$Q3c == "Rarely"] <- 1
sel_data$Q3c[sel_data$Q3c == "Occasionally"] <- 2
sel_data$Q3c[sel_data$Q3c == "Frequently"] <- 3
sel_data$Q3c[sel_data$Q3c == "Very Frequently"] <- 4
sel_data$Q3c[sel_data$Q3c == "Unable to Answer"] <- NA

# Recoding of Q3d	(Q4) handle his/her belongings with care?

sel_data$Q3d[sel_data$Q3d == "Never"] <- 0
sel_data$Q3d[sel_data$Q3d == "Rarely"] <- 1
sel_data$Q3d[sel_data$Q3d == "Occasionally"] <- 2
sel_data$Q3d[sel_data$Q3d == "Frequently"] <- 3
sel_data$Q3d[sel_data$Q3d == "Very Frequently"] <- 4
sel_data$Q3d[sel_data$Q3d == "Unable to Answer"] <- NA

# Recoding of Q3e	(Q5) say good things about himself/herself?

sel_data$Q3e[sel_data$Q3e == "Never"] <- 0
sel_data$Q3e[sel_data$Q3e == "Rarely"] <- 1
sel_data$Q3e[sel_data$Q3e == "Occasionally"] <- 2
sel_data$Q3e[sel_data$Q3e == "Frequently"] <- 3
sel_data$Q3e[sel_data$Q3e == "Very Frequently"] <- 4
sel_data$Q3e[sel_data$Q3e == "Unable to Answer"] <- NA

# View(sel_data) # So far .... so good

# Recoding of Q3f	(Q6) serve an important role at home or school?

sel_data$Q3f[sel_data$Q3f == "Never"] <- 0
sel_data$Q3f[sel_data$Q3f == "Rarely"] <- 1
sel_data$Q3f[sel_data$Q3f == "Occasionally"] <- 2
sel_data$Q3f[sel_data$Q3f == "Frequently"] <- 3
sel_data$Q3f[sel_data$Q3f == "Very Frequently"] <- 4
sel_data$Q3f[sel_data$Q3f == "Unable to Answer"] <- NA

# Recoding of Q3g	(Q7) speak about positive things?

sel_data$Q3g[sel_data$Q3g == "Never"] <- 0
sel_data$Q3g[sel_data$Q3g == "Rarely"] <- 1
sel_data$Q3g[sel_data$Q3g == "Occasionally"] <- 2
sel_data$Q3g[sel_data$Q3g == "Frequently"] <- 3
sel_data$Q3g[sel_data$Q3g == "Very Frequently"] <- 4
sel_data$Q3g[sel_data$Q3g == "Unable to Answer"] <- NA

# Recoding of Q3h	(Q8) cope well with insults and mean comments?

sel_data$Q3h[sel_data$Q3h == "Never"] <- 0
sel_data$Q3h[sel_data$Q3h == "Rarely"] <- 1
sel_data$Q3h[sel_data$Q3h == "Occasionally"] <- 2
sel_data$Q3h[sel_data$Q3h == "Frequently"] <- 3
sel_data$Q3h[sel_data$Q3h == "Very Frequently"] <- 4
sel_data$Q3h[sel_data$Q3h == "Unable to Answer"] <- NA

# Recoding of Q3i	(Q9) take steps to achieve goals?

sel_data$Q3i[sel_data$Q3i == "Never"] <- 0
sel_data$Q3i[sel_data$Q3i == "Rarely"] <- 1
sel_data$Q3i[sel_data$Q3i == "Occasionally"] <- 2
sel_data$Q3i[sel_data$Q3i == "Frequently"] <- 3
sel_data$Q3i[sel_data$Q3i == "Very Frequently"] <- 4
sel_data$Q3i[sel_data$Q3i == "Unable to Answer"] <- NA

# Recoding of Q3j	(Q10) look forward to classes or activities at school?

sel_data$Q3j[sel_data$Q3j == "Never"] <- 0
sel_data$Q3j[sel_data$Q3j == "Rarely"] <- 1
sel_data$Q3j[sel_data$Q3j == "Occasionally"] <- 2
sel_data$Q3j[sel_data$Q3j == "Frequently"] <- 3
sel_data$Q3j[sel_data$Q3j == "Very Frequently"] <- 4
sel_data$Q3j[sel_data$Q3j == "Unable to Answer"] <- NA

# Recoding of Q3k	(Q11) get along with different types of people?

sel_data$Q3k[sel_data$Q3k == "Never"] <- 0
sel_data$Q3k[sel_data$Q3k == "Rarely"] <- 1
sel_data$Q3k[sel_data$Q3k == "Occasionally"] <- 2
sel_data$Q3k[sel_data$Q3k == "Frequently"] <- 3
sel_data$Q3k[sel_data$Q3k == "Very Frequently"] <- 4
sel_data$Q3k[sel_data$Q3k == "Unable to Answer"] <- NA

# Recoding of Q3l	(Q12) try to do her/his best?

sel_data$Q3l[sel_data$Q3l == "Never"] <- 0
sel_data$Q3l[sel_data$Q3l == "Rarely"] <- 1
sel_data$Q3l[sel_data$Q3l == "Occasionally"] <- 2
sel_data$Q3l[sel_data$Q3l == "Frequently"] <- 3
sel_data$Q3l[sel_data$Q3l == "Very Frequently"] <- 4
sel_data$Q3l[sel_data$Q3l == "Unable to Answer"] <- NA

# Recoding of Q3m	(Q13) seek out additional knowledge or information?

sel_data$Q3m[sel_data$Q3m == "Never"] <- 0
sel_data$Q3m[sel_data$Q3m == "Rarely"] <- 1
sel_data$Q3m[sel_data$Q3m == "Occasionally"] <- 2
sel_data$Q3m[sel_data$Q3m == "Frequently"] <- 3
sel_data$Q3m[sel_data$Q3m == "Very Frequently"] <- 4
sel_data$Q3m[sel_data$Q3m == "Unable to Answer"] <- NA

# Recoding of Q3n	(Q14) take an active role in learning?

sel_data$Q3n[sel_data$Q3n == "Never"] <- 0
sel_data$Q3n[sel_data$Q3n == "Rarely"] <- 1
sel_data$Q3n[sel_data$Q3n == "Occasionally"] <- 2
sel_data$Q3n[sel_data$Q3n == "Frequently"] <- 3
sel_data$Q3n[sel_data$Q3n == "Very Frequently"] <- 4
sel_data$Q3n[sel_data$Q3n == "Unable to Answer"] <- NA

# Recoding of Q3o	(Q15) do things independently?

sel_data$Q3o[sel_data$Q3o == "Never"] <- 0
sel_data$Q3o[sel_data$Q3o == "Rarely"] <- 1
sel_data$Q3o[sel_data$Q3o == "Occasionally"] <- 2
sel_data$Q3o[sel_data$Q3o == "Frequently"] <- 3
sel_data$Q3o[sel_data$Q3o == "Very Frequently"] <- 4
sel_data$Q3o[sel_data$Q3o == "Unable to Answer"] <- NA

# Recoding of Q3p	(Q16) say good things about his/her classmates?

sel_data$Q3p[sel_data$Q3p == "Never"] <- 0
sel_data$Q3p[sel_data$Q3p == "Rarely"] <- 1
sel_data$Q3p[sel_data$Q3p == "Occasionally"] <- 2
sel_data$Q3p[sel_data$Q3p == "Frequently"] <- 3
sel_data$Q3p[sel_data$Q3p == "Very Frequently"] <- 4
sel_data$Q3p[sel_data$Q3p == "Unable to Answer"] <- NA

# Recoding of Q3q	(Q17) act respectfully in a game or competition?

sel_data$Q3q[sel_data$Q3q == "Never"] <- 0
sel_data$Q3q[sel_data$Q3q == "Rarely"] <- 1
sel_data$Q3q[sel_data$Q3q == "Occasionally"] <- 2
sel_data$Q3q[sel_data$Q3q == "Frequently"] <- 3
sel_data$Q3q[sel_data$Q3q == "Very Frequently"] <- 4
sel_data$Q3q[sel_data$Q3q == "Unable to Answer"] <- NA

# Recoding of Q3r	(Q18) ask to take on additional work or responsibilities?

sel_data$Q3r[sel_data$Q3r == "Never"] <- 0
sel_data$Q3r[sel_data$Q3r == "Rarely"] <- 1
sel_data$Q3r[sel_data$Q3r == "Occasionally"] <- 2
sel_data$Q3r[sel_data$Q3r == "Frequently"] <- 3
sel_data$Q3r[sel_data$Q3r == "Very Frequently"] <- 4
sel_data$Q3r[sel_data$Q3r == "Unable to Answer"] <- NA

# Recoding of Q3s	(Q19) respect another persons opinion?

sel_data$Q3s[sel_data$Q3s == "Never"] <- 0
sel_data$Q3s[sel_data$Q3s == "Rarely"] <- 1
sel_data$Q3s[sel_data$Q3s == "Occasionally"] <- 2
sel_data$Q3s[sel_data$Q3s == "Frequently"] <- 3
sel_data$Q3s[sel_data$Q3s == "Very Frequently"] <- 4
sel_data$Q3s[sel_data$Q3s == "Unable to Answer"] <- NA

# Recoding of Q3t	(Q20) encourage positive behavior in others?

sel_data$Q3t[sel_data$Q3t == "Never"] <- 0
sel_data$Q3t[sel_data$Q3t == "Rarely"] <- 1
sel_data$Q3t[sel_data$Q3t == "Occasionally"] <- 2
sel_data$Q3t[sel_data$Q3t == "Frequently"] <- 3
sel_data$Q3t[sel_data$Q3t == "Very Frequently"] <- 4
sel_data$Q3t[sel_data$Q3t == "Unable to Answer"] <- NA

# Recoding of Q3u	(Q21) prepare for school, activities, or upcoming events?

sel_data$Q3u[sel_data$Q3u == "Never"] <- 0
sel_data$Q3u[sel_data$Q3u == "Rarely"] <- 1
sel_data$Q3u[sel_data$Q3u == "Occasionally"] <- 2
sel_data$Q3u[sel_data$Q3u == "Frequently"] <- 3
sel_data$Q3u[sel_data$Q3u == "Very Frequently"] <- 4
sel_data$Q3u[sel_data$Q3u == "Unable to Answer"] <- NA

# Recoding of Q3v	(Q22) contribute to group efforts?

sel_data$Q3v[sel_data$Q3v == "Never"] <- 0
sel_data$Q3v[sel_data$Q3v == "Rarely"] <- 1
sel_data$Q3v[sel_data$Q3v == "Occasionally"] <- 2
sel_data$Q3v[sel_data$Q3v == "Frequently"] <- 3
sel_data$Q3v[sel_data$Q3v == "Very Frequently"] <- 4
sel_data$Q3v[sel_data$Q3v == "Unable to Answer"] <- NA

# Recoding of Q3w	(Q23) do routine tasks or chores without being reminded?

sel_data$Q3w[sel_data$Q3w == "Never"] <- 0
sel_data$Q3w[sel_data$Q3w == "Rarely"] <- 1
sel_data$Q3w[sel_data$Q3w == "Occasionally"] <- 2
sel_data$Q3w[sel_data$Q3w == "Frequently"] <- 3
sel_data$Q3w[sel_data$Q3w == "Very Frequently"] <- 4
sel_data$Q3w[sel_data$Q3w == "Unable to Answer"] <- NA

# Recoding of Q3x	(Q24) act as a leader in a peer group?

sel_data$Q3x[sel_data$Q3x == "Never"] <- 0
sel_data$Q3x[sel_data$Q3x == "Rarely"] <- 1
sel_data$Q3x[sel_data$Q3x == "Occasionally"] <- 2
sel_data$Q3x[sel_data$Q3x == "Frequently"] <- 3
sel_data$Q3x[sel_data$Q3x == "Very Frequently"] <- 4
sel_data$Q3x[sel_data$Q3x == "Unable to Answer"] <- NA

# Recoding of Q3y	(Q25) resolve a disagreement?

sel_data$Q3y[sel_data$Q3y == "Never"] <- 0
sel_data$Q3y[sel_data$Q3y == "Rarely"] <- 1
sel_data$Q3y[sel_data$Q3y == "Occasionally"] <- 2
sel_data$Q3y[sel_data$Q3y == "Frequently"] <- 3
sel_data$Q3y[sel_data$Q3y == "Very Frequently"] <- 4
sel_data$Q3y[sel_data$Q3y == "Unable to Answer"] <- NA

# Recoding of Q3z	(Q26) show creativity in completing a task?

sel_data$Q3z[sel_data$Q3z == "Never"] <- 0
sel_data$Q3z[sel_data$Q3z == "Rarely"] <- 1
sel_data$Q3z[sel_data$Q3z == "Occasionally"] <- 2
sel_data$Q3z[sel_data$Q3z == "Frequently"] <- 3
sel_data$Q3z[sel_data$Q3z == "Very Frequently"] <- 4
sel_data$Q3z[sel_data$Q3z == "Unable to Answer"] <- NA

# Recoding of Q3aa (Q27) share with others?

sel_data$Q3aa[sel_data$Q3aa == "Never"] <- 0
sel_data$Q3aa[sel_data$Q3aa == "Rarely"] <- 1
sel_data$Q3aa[sel_data$Q3aa == "Occasionally"] <- 2
sel_data$Q3aa[sel_data$Q3aa == "Frequently"] <- 3
sel_data$Q3aa[sel_data$Q3aa == "Very Frequently"] <- 4
sel_data$Q3aa[sel_data$Q3aa == "Unable to Answer"] <- NA

# Recoding of Q3ab (Q 28)	get things done in a timely fashion?

sel_data$Q3ab[sel_data$Q3ab == "Never"] <- 0
sel_data$Q3ab[sel_data$Q3ab == "Rarely"] <- 1
sel_data$Q3ab[sel_data$Q3ab == "Occasionally"] <- 2
sel_data$Q3ab[sel_data$Q3ab == "Frequently"] <- 3
sel_data$Q3ab[sel_data$Q3ab == "Very Frequently"] <- 4
sel_data$Q3ab[sel_data$Q3ab == "Unable to Answer"] <- NA

# Recoding of Q3ac (Q29) seek out challenging tasks?

sel_data$Q3ac[sel_data$Q3ac == "Never"] <- 0
sel_data$Q3ac[sel_data$Q3ac == "Rarely"] <- 1
sel_data$Q3ac[sel_data$Q3ac == "Occasionally"] <- 2
sel_data$Q3ac[sel_data$Q3ac == "Frequently"] <- 3
sel_data$Q3ac[sel_data$Q3ac == "Very Frequently"] <- 4
sel_data$Q3ac[sel_data$Q3ac == "Unable to Answer"] <- NA

# Recoding of Q3ad (Q30) say good things about the future?
sel_data$Q3ad[sel_data$Q3ad == "Never"] <- 0
sel_data$Q3ad[sel_data$Q3ad == "Rarely"] <- 1
sel_data$Q3ad[sel_data$Q3ad == "Occasionally"] <- 2
sel_data$Q3ad[sel_data$Q3ad == "Frequently"] <- 3
sel_data$Q3ad[sel_data$Q3ad == "Very Frequently"] <- 4
sel_data$Q3ad[sel_data$Q3ad == "Unable to Answer"] <- NA

# Recoding of Q3ae (Q31) cooperate with peers or siblings?

sel_data$Q3ae[sel_data$Q3ae == "Never"] <- 0
sel_data$Q3ae[sel_data$Q3ae == "Rarely"] <- 1
sel_data$Q3ae[sel_data$Q3ae == "Occasionally"] <- 2
sel_data$Q3ae[sel_data$Q3ae == "Frequently"] <- 3
sel_data$Q3ae[sel_data$Q3ae == "Very Frequently"] <- 4
sel_data$Q3ae[sel_data$Q3ae == "Unable to Answer"] <- NA

# Recoding of Q3af (Q32) show care when doing a project or school work?

sel_data$Q3af[sel_data$Q3af == "Never"] <- 0
sel_data$Q3af[sel_data$Q3af == "Rarely"] <- 1
sel_data$Q3af[sel_data$Q3af == "Occasionally"] <- 2
sel_data$Q3af[sel_data$Q3af == "Frequently"] <- 3
sel_data$Q3af[sel_data$Q3af == "Very Frequently"] <- 4
sel_data$Q3af[sel_data$Q3af == "Unable to Answer"] <- NA

# Recoding of Q3ag (Q33) work hard on projects?

sel_data$Q3ag[sel_data$Q3ag == "Never"] <- 0
sel_data$Q3ag[sel_data$Q3ag == "Rarely"] <- 1
sel_data$Q3ag[sel_data$Q3ag == "Occasionally"] <- 2
sel_data$Q3ag[sel_data$Q3ag == "Frequently"] <- 3
sel_data$Q3ag[sel_data$Q3ag == "Very Frequently"] <- 4
sel_data$Q3ag[sel_data$Q3ag == "Unable to Answer"] <- NA

# Recoding of Q3ah (Q34) forgive somebody who hurt or upset her/him?

sel_data$Q3ah[sel_data$Q3ah == "Never"] <- 0
sel_data$Q3ah[sel_data$Q3ah == "Rarely"] <- 1
sel_data$Q3ah[sel_data$Q3ah == "Occasionally"] <- 2
sel_data$Q3ah[sel_data$Q3ah == "Frequently"] <- 3
sel_data$Q3ah[sel_data$Q3ah == "Very Frequently"] <- 4
sel_data$Q3ah[sel_data$Q3ah == "Unable to Answer"] <- NA

# Recoding Q3ai	(Q35) follow rules?

sel_data$Q3ai[sel_data$Q3ai == "Never"] <- 0
sel_data$Q3ai[sel_data$Q3ai == "Rarely"] <- 1
sel_data$Q3ai[sel_data$Q3ai == "Occasionally"] <- 2
sel_data$Q3ai[sel_data$Q3ai == "Frequently"] <- 3
sel_data$Q3ai[sel_data$Q3ai == "Very Frequently"] <- 4
sel_data$Q3ai[sel_data$Q3ai == "Unable to Answer"] <- NA

# Recoding Q3aj	(Q36) express high expectations for himself/herself?

sel_data$Q3aj[sel_data$Q3aj == "Never"] <- 0
sel_data$Q3aj[sel_data$Q3aj == "Rarely"] <- 1
sel_data$Q3aj[sel_data$Q3aj == "Occasionally"] <- 2
sel_data$Q3aj[sel_data$Q3aj == "Frequently"] <- 3
sel_data$Q3aj[sel_data$Q3aj == "Very Frequently"] <- 4
sel_data$Q3aj[sel_data$Q3aj == "Unable to Answer"] <- NA

# Recoding of Q3ak (Q37) follow the example of a positive role model?

sel_data$Q3ak[sel_data$Q3ak == "Never"] <- 0
sel_data$Q3ak[sel_data$Q3ak == "Rarely"] <- 1
sel_data$Q3ak[sel_data$Q3ak == "Occasionally"] <- 2
sel_data$Q3ak[sel_data$Q3ak == "Frequently"] <- 3
sel_data$Q3ak[sel_data$Q3ak == "Very Frequently"] <- 4
sel_data$Q3ak[sel_data$Q3ak == "Unable to Answer"] <- NA

# Recoding of Q3al (Q38) compliment or congratulate somebody?

sel_data$Q3al[sel_data$Q3al == "Never"] <- 0
sel_data$Q3al[sel_data$Q3al == "Rarely"] <- 1
sel_data$Q3al[sel_data$Q3al == "Occasionally"] <- 2
sel_data$Q3al[sel_data$Q3al == "Frequently"] <- 3
sel_data$Q3al[sel_data$Q3al == "Very Frequently"] <- 4
sel_data$Q3al[sel_data$Q3al == "Unable to Answer"] <- NA

# Recoding of Q3am	(Q39) accept responsibility for what she/he did?

sel_data$Q3am[sel_data$Q3am == "Never"] <- 0
sel_data$Q3am[sel_data$Q3am == "Rarely"] <- 1
sel_data$Q3am[sel_data$Q3am == "Occasionally"] <- 2
sel_data$Q3am[sel_data$Q3am == "Frequently"] <- 3
sel_data$Q3am[sel_data$Q3am == "Very Frequently"] <- 4
sel_data$Q3am[sel_data$Q3am == "Unable to Answer"] <- NA

# Recoding of Q3an	(Q40) do something nice for somebody?

sel_data$Q3an[sel_data$Q3an == "Never"] <- 0
sel_data$Q3an[sel_data$Q3an == "Rarely"] <- 1
sel_data$Q3an[sel_data$Q3an == "Occasionally"] <- 2
sel_data$Q3an[sel_data$Q3an == "Frequently"] <- 3
sel_data$Q3an[sel_data$Q3an == "Very Frequently"] <- 4
sel_data$Q3an[sel_data$Q3an == "Unable to Answer"] <- NA

# Recoding of Q3ao (Q41) make accurate statements about events in her/his life?

sel_data$Q3ao[sel_data$Q3ao == "Never"] <- 0
sel_data$Q3ao[sel_data$Q3ao == "Rarely"] <- 1
sel_data$Q3ao[sel_data$Q3ao == "Occasionally"] <- 2
sel_data$Q3ao[sel_data$Q3ao == "Frequently"] <- 3
sel_data$Q3ao[sel_data$Q3ao == "Very Frequently"] <- 4
sel_data$Q3ao[sel_data$Q3ao == "Unable to Answer"] <- NA

# Recoding of Q3ap (Q42) show good judgment?

sel_data$Q3ap[sel_data$Q3ap == "Never"] <- 0
sel_data$Q3ap[sel_data$Q3ap == "Rarely"] <- 1
sel_data$Q3ap[sel_data$Q3ap == "Occasionally"] <- 2
sel_data$Q3ap[sel_data$Q3ap == "Frequently"] <- 3
sel_data$Q3ap[sel_data$Q3ap == "Very Frequently"] <- 4
sel_data$Q3ap[sel_data$Q3ap == "Unable to Answer"] <- NA

# Recode of the Q3aq (Q43) pay attention?

sel_data$Q3aq[sel_data$Q3aq == "Never"] <- 0
sel_data$Q3aq[sel_data$Q3aq == "Rarely"] <- 1
sel_data$Q3aq[sel_data$Q3aq == "Occasionally"] <- 2
sel_data$Q3aq[sel_data$Q3aq == "Frequently"] <- 3
sel_data$Q3aq[sel_data$Q3aq == "Very Frequently"] <- 4
sel_data$Q3aq[sel_data$Q3aq == "Unable to Answer"] <- NA

# Recode of Q3ar (Q44) wait for her/his turn?

sel_data$Q3ar[sel_data$Q3ar == "Never"] <- 0
sel_data$Q3ar[sel_data$Q3ar == "Rarely"] <- 1
sel_data$Q3ar[sel_data$Q3ar == "Occasionally"] <- 2
sel_data$Q3ar[sel_data$Q3ar == "Frequently"] <- 3
sel_data$Q3ar[sel_data$Q3ar == "Very Frequently"] <- 4
sel_data$Q3ar[sel_data$Q3ar == "Unable to Answer"] <- NA

# Recoding of the Q3as (Q45) show appreciation of others?

sel_data$Q3as[sel_data$Q3as == "Never"] <- 0
sel_data$Q3as[sel_data$Q3as == "Rarely"] <- 1
sel_data$Q3as[sel_data$Q3as == "Occasionally"] <- 2
sel_data$Q3as[sel_data$Q3as == "Frequently"] <- 3
sel_data$Q3as[sel_data$Q3as == "Very Frequently"] <- 4
sel_data$Q3as[sel_data$Q3as == "Unable to Answer"] <- NA

# Recoding Q3at	(Q46) focus on a task despite a problem or distraction?

sel_data$Q3at[sel_data$Q3at == "Never"] <- 0
sel_data$Q3at[sel_data$Q3at == "Rarely"] <- 1
sel_data$Q3at[sel_data$Q3at == "Occasionally"] <- 2
sel_data$Q3at[sel_data$Q3at == "Frequently"] <- 3
sel_data$Q3at[sel_data$Q3at == "Very Frequently"] <- 4
sel_data$Q3at[sel_data$Q3at == "Unable to Answer"] <- NA

# Recoding Q3au	(Q47) greet a person in a polite way?

sel_data$Q3au[sel_data$Q3au == "Never"] <- 0
sel_data$Q3au[sel_data$Q3au == "Rarely"] <- 1
sel_data$Q3au[sel_data$Q3au == "Occasionally"] <- 2
sel_data$Q3au[sel_data$Q3au == "Frequently"] <- 3
sel_data$Q3au[sel_data$Q3au == "Very Frequently"] <- 4
sel_data$Q3au[sel_data$Q3au == "Unable to Answer"] <- NA

# Recoding Q3av	(Q48) act comfortable in a new situation?

sel_data$Q3av[sel_data$Q3av == "Never"] <- 0
sel_data$Q3av[sel_data$Q3av == "Rarely"] <- 1
sel_data$Q3av[sel_data$Q3av == "Occasionally"] <- 2
sel_data$Q3av[sel_data$Q3av == "Frequently"] <- 3
sel_data$Q3av[sel_data$Q3av == "Very Frequently"] <- 4
sel_data$Q3av[sel_data$Q3av == "Unable to Answer"] <- NA

# Recoding Q3aw	(Q49) teach another person to do something?

sel_data$Q3aw[sel_data$Q3aw == "Never"] <- 0
sel_data$Q3aw[sel_data$Q3aw == "Rarely"] <- 1
sel_data$Q3aw[sel_data$Q3aw == "Occasionally"] <- 2
sel_data$Q3aw[sel_data$Q3aw == "Frequently"] <- 3
sel_data$Q3aw[sel_data$Q3aw == "Very Frequently"] <- 4
sel_data$Q3aw[sel_data$Q3aw == "Unable to Answer"] <- NA

# Recoding Q3ax	(Q50) attract positive attention from peers?

sel_data$Q3ax[sel_data$Q3ax == "Never"] <- 0
sel_data$Q3ax[sel_data$Q3ax == "Rarely"] <- 1
sel_data$Q3ax[sel_data$Q3ax == "Occasionally"] <- 2
sel_data$Q3ax[sel_data$Q3ax == "Frequently"] <- 3
sel_data$Q3ax[sel_data$Q3ax == "Very Frequently"] <- 4
sel_data$Q3ax[sel_data$Q3ax == "Unable to Answer"] <- NA

# Recoding Q3ay	(Q51) perform the steps of a task in order?

sel_data$Q3ay[sel_data$Q3ay == "Never"] <- 0
sel_data$Q3ay[sel_data$Q3ay == "Rarely"] <- 1
sel_data$Q3ay[sel_data$Q3ay == "Occasionally"] <- 2
sel_data$Q3ay[sel_data$Q3ay == "Frequently"] <- 3
sel_data$Q3ay[sel_data$Q3ay == "Very Frequently"] <- 4
sel_data$Q3ay[sel_data$Q3ay == "Unable to Answer"] <- NA

# Recoding Q3az	(Q52) seek advice?

sel_data$Q3az[sel_data$Q3az == "Never"] <- 0
sel_data$Q3az[sel_data$Q3az == "Rarely"] <- 1
sel_data$Q3az[sel_data$Q3az == "Occasionally"] <- 2
sel_data$Q3az[sel_data$Q3az == "Frequently"] <- 3
sel_data$Q3az[sel_data$Q3az == "Very Frequently"] <- 4
sel_data$Q3az[sel_data$Q3az == "Unable to Answer"] <- NA

# Recoding Q3ba	(Q53) think before he/she acted?

sel_data$Q3ba[sel_data$Q3ba == "Never"] <- 0
sel_data$Q3ba[sel_data$Q3ba == "Rarely"] <- 1
sel_data$Q3ba[sel_data$Q3ba == "Occasionally"] <- 2
sel_data$Q3ba[sel_data$Q3ba == "Frequently"] <- 3
sel_data$Q3ba[sel_data$Q3ba == "Very Frequently"] <- 4
sel_data$Q3ba[sel_data$Q3ba == "Unable to Answer"] <- NA

# Recoding Q3bb	(Q54) pass up something he/she wanted, or do something he/she did not like, to get something better in the future?

sel_data$Q3bb[sel_data$Q3bb == "Never"] <- 0
sel_data$Q3bb[sel_data$Q3bb == "Rarely"] <- 1
sel_data$Q3bb[sel_data$Q3bb == "Occasionally"] <- 2
sel_data$Q3bb[sel_data$Q3bb == "Frequently"] <- 3
sel_data$Q3bb[sel_data$Q3bb == "Very Frequently"] <- 4
sel_data$Q3bb[sel_data$Q3bb == "Unable to Answer"] <- NA

# Recoding Q3bc (Q55) express concern for another person?

sel_data$Q3bc[sel_data$Q3bc == "Never"] <- 0
sel_data$Q3bc[sel_data$Q3bc == "Rarely"] <- 1
sel_data$Q3bc[sel_data$Q3bc == "Occasionally"] <- 2
sel_data$Q3bc[sel_data$Q3bc == "Frequently"] <- 3
sel_data$Q3bc[sel_data$Q3bc == "Very Frequently"] <- 4
sel_data$Q3bc[sel_data$Q3bc == "Unable to Answer"] <- NA

# Recoding Q3bd	(Q56) accept another choice when his/her first choice was unacceptable?

sel_data$Q3bd[sel_data$Q3bd == "Never"] <- 0
sel_data$Q3bd[sel_data$Q3bd == "Rarely"] <- 1
sel_data$Q3bd[sel_data$Q3bd == "Occasionally"] <- 2
sel_data$Q3bd[sel_data$Q3bd == "Frequently"] <- 3
sel_data$Q3bd[sel_data$Q3bd == "Very Frequently"] <- 4
sel_data$Q3bd[sel_data$Q3bd == "Unable to Answer"] <- NA

# Recoding Q3be	(Q57) ask questions to clarify what he/she did not understand?

sel_data$Q3be[sel_data$Q3be == "Never"] <- 0
sel_data$Q3be[sel_data$Q3be == "Rarely"] <- 1
sel_data$Q3be[sel_data$Q3be == "Occasionally"] <- 2
sel_data$Q3be[sel_data$Q3be == "Frequently"] <- 3
sel_data$Q3be[sel_data$Q3be == "Very Frequently"] <- 4
sel_data$Q3be[sel_data$Q3be == "Unable to Answer"] <- NA

# Recoding ... Q3bf	(Q58) show an awareness of her/his personal strengths?

sel_data$Q3bf[sel_data$Q3bf == "Never"] <- 0
sel_data$Q3bf[sel_data$Q3bf == "Rarely"] <- 1
sel_data$Q3bf[sel_data$Q3bf == "Occasionally"] <- 2
sel_data$Q3bf[sel_data$Q3bf == "Frequently"] <- 3
sel_data$Q3bf[sel_data$Q3bf == "Very Frequently"] <- 4
sel_data$Q3bf[sel_data$Q3bf == "Unable to Answer"] <- NA

# Recoding of Q3bg (Q59) ask somebody for feedback?

sel_data$Q3bg[sel_data$Q3bg == "Never"] <- 0
sel_data$Q3bg[sel_data$Q3bg == "Rarely"] <- 1
sel_data$Q3bg[sel_data$Q3bg == "Occasionally"] <- 2
sel_data$Q3bg[sel_data$Q3bg == "Frequently"] <- 3
sel_data$Q3bg[sel_data$Q3bg == "Very Frequently"] <- 4
sel_data$Q3bg[sel_data$Q3bg == "Unable to Answer"] <- NA

# Recoding of Q3bh (Q60) stay calm when faced with a challenge?

sel_data$Q3bh[sel_data$Q3bh == "Never"] <- 0
sel_data$Q3bh[sel_data$Q3bh == "Rarely"] <- 1
sel_data$Q3bh[sel_data$Q3bh == "Occasionally"] <- 2
sel_data$Q3bh[sel_data$Q3bh == "Frequently"] <- 3
sel_data$Q3bh[sel_data$Q3bh == "Very Frequently"] <- 4
sel_data$Q3bh[sel_data$Q3bh == "Unable to Answer"] <- NA

# Recoding of Q3bi (Q61) attract positive attention from adults?

sel_data$Q3bi[sel_data$Q3bi == "Never"] <- 0
sel_data$Q3bi[sel_data$Q3bi == "Rarely"] <- 1
sel_data$Q3bi[sel_data$Q3bi == "Occasionally"] <- 2
sel_data$Q3bi[sel_data$Q3bi == "Frequently"] <- 3
sel_data$Q3bi[sel_data$Q3bi == "Very Frequently"] <- 4
sel_data$Q3bi[sel_data$Q3bi == "Unable to Answer"] <- NA

# Recoding of Q3bj	(Q62) describe how he/she was feeling?

sel_data$Q3bj[sel_data$Q3bj == "Never"] <- 0
sel_data$Q3bj[sel_data$Q3bj == "Rarely"] <- 1
sel_data$Q3bj[sel_data$Q3bj == "Occasionally"] <- 2
sel_data$Q3bj[sel_data$Q3bj == "Frequently"] <- 3
sel_data$Q3bj[sel_data$Q3bj == "Very Frequently"] <- 4
sel_data$Q3bj[sel_data$Q3bj == "Unable to Answer"] <- NA

# Recoding of Q3bk (Q63) give an opinion when asked?

sel_data$Q3bk[sel_data$Q3bk == "Never"] <- 0
sel_data$Q3bk[sel_data$Q3bk == "Rarely"] <- 1
sel_data$Q3bk[sel_data$Q3bk == "Occasionally"] <- 2
sel_data$Q3bk[sel_data$Q3bk == "Frequently"] <- 3
sel_data$Q3bk[sel_data$Q3bk == "Very Frequently"] <- 4
sel_data$Q3bk[sel_data$Q3bk == "Unable to Answer"] <- NA

# Recoding of Q3bl (Q64) make a suggestion or request in a polite way?

sel_data$Q3bl[sel_data$Q3bl == "Never"] <- 0
sel_data$Q3bl[sel_data$Q3bl == "Rarely"] <- 1
sel_data$Q3bl[sel_data$Q3bl == "Occasionally"] <- 2
sel_data$Q3bl[sel_data$Q3bl == "Frequently"] <- 3
sel_data$Q3bl[sel_data$Q3bl == "Very Frequently"] <- 4
sel_data$Q3bl[sel_data$Q3bl == "Unable to Answer"] <- NA

# Recoding of Q3bm (Q65) learn from experience?

sel_data$Q3bm[sel_data$Q3bm == "Never"] <- 0
sel_data$Q3bm[sel_data$Q3bm == "Rarely"] <- 1
sel_data$Q3bm[sel_data$Q3bm == "Occasionally"] <- 2
sel_data$Q3bm[sel_data$Q3bm == "Frequently"] <- 3
sel_data$Q3bm[sel_data$Q3bm == "Very Frequently"] <- 4
sel_data$Q3bm[sel_data$Q3bm == "Unable to Answer"] <- NA

# Recoding of Q3bn	(Q66) follow the advice of a trusted adults?

sel_data$Q3bn[sel_data$Q3bn == "Never"] <- 0
sel_data$Q3bn[sel_data$Q3bn == "Rarely"] <- 1
sel_data$Q3bn[sel_data$Q3bn == "Occasionally"] <- 2
sel_data$Q3bn[sel_data$Q3bn == "Frequently"] <- 3
sel_data$Q3bn[sel_data$Q3bn == "Very Frequently"] <- 4
sel_data$Q3bn[sel_data$Q3bn == "Unable to Answer"] <- NA

# Let's recode Q3bo	(Q67) adjust well to changes in plans?

sel_data$Q3bo[sel_data$Q3bo == "Never"] <- 0
sel_data$Q3bo[sel_data$Q3bo == "Rarely"] <- 1
sel_data$Q3bo[sel_data$Q3bo == "Occasionally"] <- 2
sel_data$Q3bo[sel_data$Q3bo == "Frequently"] <- 3
sel_data$Q3bo[sel_data$Q3bo == "Very Frequently"] <- 4
sel_data$Q3bo[sel_data$Q3bo == "Unable to Answer"] <- NA

# Recoding Q3bp	(Q68) show the ability to decide between right and wrong?

sel_data$Q3bp[sel_data$Q3bp == "Never"] <- 0
sel_data$Q3bp[sel_data$Q3bp == "Rarely"] <- 1
sel_data$Q3bp[sel_data$Q3bp == "Occasionally"] <- 2
sel_data$Q3bp[sel_data$Q3bp == "Frequently"] <- 3
sel_data$Q3bp[sel_data$Q3bp == "Very Frequently"] <- 4
sel_data$Q3bp[sel_data$Q3bp == "Unable to Answer"] <- NA

# Recoding of Q3bq (Q69) use available resources (people or objects) to solve a problem?

sel_data$Q3bq[sel_data$Q3bq == "Never"] <- 0
sel_data$Q3bq[sel_data$Q3bq == "Rarely"] <- 1
sel_data$Q3bq[sel_data$Q3bq == "Occasionally"] <- 2
sel_data$Q3bq[sel_data$Q3bq == "Frequently"] <- 3
sel_data$Q3bq[sel_data$Q3bq == "Very Frequently"] <- 4
sel_data$Q3bq[sel_data$Q3bq == "Unable to Answer"] <- NA

# Recoding of Q3br (Q70) offer to help somebody?

sel_data$Q3br[sel_data$Q3br == "Never"] <- 0
sel_data$Q3br[sel_data$Q3br == "Rarely"] <- 1
sel_data$Q3br[sel_data$Q3br == "Occasionally"] <- 2
sel_data$Q3br[sel_data$Q3br == "Frequently"] <- 3
sel_data$Q3br[sel_data$Q3br == "Very Frequently"] <- 4
sel_data$Q3br[sel_data$Q3br == "Unable to Answer"] <- NA

# Recoding of Q3bs (Q71) respond to another persons feelings?

sel_data$Q3bs[sel_data$Q3bs == "Never"] <- 0
sel_data$Q3bs[sel_data$Q3bs == "Rarely"] <- 1
sel_data$Q3bs[sel_data$Q3bs == "Occasionally"] <- 2
sel_data$Q3bs[sel_data$Q3bs == "Frequently"] <- 3
sel_data$Q3bs[sel_data$Q3bs == "Very Frequently"] <- 4
sel_data$Q3bs[sel_data$Q3bs == "Unable to Answer"] <- NA

# Recoding of Q3bt (Q72) adjust well when going from one setting to another?

sel_data$Q3bt[sel_data$Q3bt == "Never"] <- 0
sel_data$Q3bt[sel_data$Q3bt == "Rarely"] <- 1
sel_data$Q3bt[sel_data$Q3bt == "Occasionally"] <- 2
sel_data$Q3bt[sel_data$Q3bt == "Frequently"] <- 3
sel_data$Q3bt[sel_data$Q3bt == "Very Frequently"] <- 4
sel_data$Q3bt[sel_data$Q3bt == "Unable to Answer"] <- NA

# ---- 1.3 Renaming the columns ----
# Viewing the dataset and the recoding so far:
View(sel_data)

# We will remove the row in the dataset that represents the naratives of the questions 
# after verifying that the questions in the dataset match the ones in the questionnaire;
# But, before let's rename the columns :

names(sel_data) <-c("format_employed", "Complete_response_received", "Date_interview_start", "Start_time", "Date_interview_end","End_time", "Interview_duration", "Your_ID", "Child_ID", "Years", "Months", "Q_knowlege_of_child",
                    "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10",
                    "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23",
                    "Q24", "Q25", "Q26", "Q27", "Q28", "Q29", "Q30", "Q31", "Q32", "Q33", "Q34", "Q35", "Q36", "Q37",
                    "Q38", "Q39", "Q40", "Q41", "Q42", "Q43", "Q44", "Q45", "Q46", "Q47", "Q48", "Q49", "Q50",
                    "Q51", "Q52", "Q53", "Q54", "Q55", "Q56", "Q57", "Q58", "Q59", "Q60", "Q61", "Q62", "Q63", "Q64",
                    "Q65", "Q66", "Q67", "Q68", "Q69", "Q70", "Q71", "Q72", "Q_answered", "Your_ID2")



# Verifications ... the renamed questions corresponds to the questionnaire questions:
# And the ID
View(sel_data)

# Removing the naratives .... the 1rst row of the dataset :
# Remember [rows, col]

sel_data <- sel_data[-1, ]


# ---- 1.4 Transforming the variables ----
str(sel_data)

# All the variables are in "chr", we need to transforme them into "numeric".

# We start by defining the columns we whant to tranform to numeric :
cols_to_numerize <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9",
                     "Q10","Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17",
                     "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25",
                     "Q26", "Q27", "Q28", "Q29", "Q30", "Q31", "Q32", "Q33", 
                     "Q34", "Q35", "Q36", "Q37", "Q38", "Q39", "Q40", "Q41", 
                     "Q42", "Q43", "Q44", "Q45", "Q46", "Q47", "Q48", "Q49", 
                     "Q50", "Q51", "Q52", "Q53", "Q54", "Q55", "Q56", "Q57", 
                     "Q58", "Q59", "Q60", "Q61", "Q62", "Q63", "Q64","Q65", 
                     "Q66", "Q67", "Q68", "Q69", "Q70", "Q71", "Q72")

sel_data[cols_to_numerize] <- sapply(sel_data[cols_to_numerize], as.numeric)

# Quick note :
# When using a data frame the data types must all be the same type or class 
# otherwise they will be subjected to type conversion.
# Other info ... see documentations about the apply(), tapply() and sapply() family of functions 

# Verifying that all is correct ....
sapply(sel_data, class)
str(sel_data) # same thing ...
