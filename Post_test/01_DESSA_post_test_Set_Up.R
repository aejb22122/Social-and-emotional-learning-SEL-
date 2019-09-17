# Title:    Devereux Student Strengths Assessment (DESSA) - Spring 2019
# Author:   Annick Eudes JB Research Manager
# Date:     July 22, 2019
# Version:  1.0

# Post test 
# ---- Libraries ---- 

# Load the R packages that will be needed throughout this script. These must
# have been previously installed.

library(tidyverse); library(stringr)
library(ggplot2); library(rbokeh);
library(ggrepel); library(pander); library(knitr);
library(stringr); library(grid); library(gridExtra)
library(frequencies); library(psych)
library(RColorBrewer)
library(rmarkdown)
library(flextable)
library(sm)
library(plotrix)


# ---- Loading the post test data ----
# The data comes from different form of data collection methods - web, phone and tablet
setwd("P:/RE/Private/SEL Assessment Results/DESSA Scoring/data/post-test")
list.files()

library(readxl)
df_post_test <- read_excel("all_post_test_data.xlsx")
View(df_post_test)


# ---- 1.1 Cleaning the dataset ----
# we will remove some of the columns that we might not use:
df_post_test$ID.format <- NULL
df_post_test$ID.completed <- NULL
df_post_test$ID.date <- NULL
df_post_test$ID.start <- NULL
df_post_test$ID.endDate <- NULL
df_post_test$ID.end <- NULL
df_post_test$ID.time <- NULL
df_post_test$Q5 <- NULL   # This column is the same has the Your_ID column. 

# ---- 1.2 Coding the variables to a scoring ready format ----

df_post_test$Q2[df_post_test$Q2 == "Strongly Disagree"] <- 1
df_post_test$Q2[df_post_test$Q2 == "Disagree"] <- 2
df_post_test$Q2[df_post_test$Q2 == "Neither agree or disagree"] <- 3
df_post_test$Q2[df_post_test$Q2 == "Agree"] <- 4
df_post_test$Q2[df_post_test$Q2 == "Strongly agree"] <- 5

# From this point and moving foward, the recoding of the variables are similar...
# From question Q3a to question Q3bt and Q4 we will use the following codes :
# 0 = Never, 1 = Rarely, 2 = Occasionally, 3 = Frequently, 4 = Very Frequently
# NA = Unable to Answer

# Recoding of Q3a	1 "remember important information?"

df_post_test$Q3a[df_post_test$Q3a == "Never"] <- 0
df_post_test$Q3a[df_post_test$Q3a == "Rarely"] <- 1
df_post_test$Q3a[df_post_test$Q3a == "Occasionally"] <- 2
df_post_test$Q3a[df_post_test$Q3a == "Frequently"] <- 3
df_post_test$Q3a[df_post_test$Q3a == "Very Frequently"] <- 4
df_post_test$Q3a[df_post_test$Q3a == "Unable to Answer"] <- NA

# Recoding of Q3b	(Q2) carry himself or herself with confidence?

df_post_test$Q3b[df_post_test$Q3b == "Never"] <- 0
df_post_test$Q3b[df_post_test$Q3b == "Rarely"] <- 1
df_post_test$Q3b[df_post_test$Q3b == "Occasionally"] <- 2
df_post_test$Q3b[df_post_test$Q3b == "Frequently"] <- 3
df_post_test$Q3b[df_post_test$Q3b == "Very Frequently"] <- 4
df_post_test$Q3b[df_post_test$Q3b == "Unable to Answer"] <- NA

# Recoding of Q3c	(Q3) keep trying when unsuccessful?

df_post_test$Q3c[df_post_test$Q3c == "Never"] <- 0
df_post_test$Q3c[df_post_test$Q3c == "Rarely"] <- 1
df_post_test$Q3c[df_post_test$Q3c == "Occasionally"] <- 2
df_post_test$Q3c[df_post_test$Q3c == "Frequently"] <- 3
df_post_test$Q3c[df_post_test$Q3c == "Very Frequently"] <- 4
df_post_test$Q3c[df_post_test$Q3c == "Unable to Answer"] <- NA

# Recoding of Q3d	(Q4) handle his/her belongings with care?

df_post_test$Q3d[df_post_test$Q3d == "Never"] <- 0
df_post_test$Q3d[df_post_test$Q3d == "Rarely"] <- 1
df_post_test$Q3d[df_post_test$Q3d == "Occasionally"] <- 2
df_post_test$Q3d[df_post_test$Q3d == "Frequently"] <- 3
df_post_test$Q3d[df_post_test$Q3d == "Very Frequently"] <- 4
df_post_test$Q3d[df_post_test$Q3d == "Unable to Answer"] <- NA

# Recoding of Q3e	(Q5) say good things about himself/herself?

df_post_test$Q3e[df_post_test$Q3e == "Never"] <- 0
df_post_test$Q3e[df_post_test$Q3e == "Rarely"] <- 1
df_post_test$Q3e[df_post_test$Q3e == "Occasionally"] <- 2
df_post_test$Q3e[df_post_test$Q3e == "Frequently"] <- 3
df_post_test$Q3e[df_post_test$Q3e == "Very Frequently"] <- 4
df_post_test$Q3e[df_post_test$Q3e == "Unable to Answer"] <- NA

# View(df_post_test) # So far .... so good

# Recoding of Q3f	(Q6) serve an important role at home or school?

df_post_test$Q3f[df_post_test$Q3f == "Never"] <- 0
df_post_test$Q3f[df_post_test$Q3f == "Rarely"] <- 1
df_post_test$Q3f[df_post_test$Q3f == "Occasionally"] <- 2
df_post_test$Q3f[df_post_test$Q3f == "Frequently"] <- 3
df_post_test$Q3f[df_post_test$Q3f == "Very Frequently"] <- 4
df_post_test$Q3f[df_post_test$Q3f == "Unable to Answer"] <- NA

# Recoding of Q3g	(Q7) speak about positive things?

df_post_test$Q3g[df_post_test$Q3g == "Never"] <- 0
df_post_test$Q3g[df_post_test$Q3g == "Rarely"] <- 1
df_post_test$Q3g[df_post_test$Q3g == "Occasionally"] <- 2
df_post_test$Q3g[df_post_test$Q3g == "Frequently"] <- 3
df_post_test$Q3g[df_post_test$Q3g == "Very Frequently"] <- 4
df_post_test$Q3g[df_post_test$Q3g == "Unable to Answer"] <- NA

# Recoding of Q3h	(Q8) cope well with insults and mean comments?

df_post_test$Q3h[df_post_test$Q3h == "Never"] <- 0
df_post_test$Q3h[df_post_test$Q3h == "Rarely"] <- 1
df_post_test$Q3h[df_post_test$Q3h == "Occasionally"] <- 2
df_post_test$Q3h[df_post_test$Q3h == "Frequently"] <- 3
df_post_test$Q3h[df_post_test$Q3h == "Very Frequently"] <- 4
df_post_test$Q3h[df_post_test$Q3h == "Unable to Answer"] <- NA

# Recoding of Q3i	(Q9) take steps to achieve goals?

df_post_test$Q3i[df_post_test$Q3i == "Never"] <- 0
df_post_test$Q3i[df_post_test$Q3i == "Rarely"] <- 1
df_post_test$Q3i[df_post_test$Q3i == "Occasionally"] <- 2
df_post_test$Q3i[df_post_test$Q3i == "Frequently"] <- 3
df_post_test$Q3i[df_post_test$Q3i == "Very Frequently"] <- 4
df_post_test$Q3i[df_post_test$Q3i == "Unable to Answer"] <- NA

# Recoding of Q3j	(Q10) look forward to classes or activities at school?

df_post_test$Q3j[df_post_test$Q3j == "Never"] <- 0
df_post_test$Q3j[df_post_test$Q3j == "Rarely"] <- 1
df_post_test$Q3j[df_post_test$Q3j == "Occasionally"] <- 2
df_post_test$Q3j[df_post_test$Q3j == "Frequently"] <- 3
df_post_test$Q3j[df_post_test$Q3j == "Very Frequently"] <- 4
df_post_test$Q3j[df_post_test$Q3j == "Unable to Answer"] <- NA

# Recoding of Q3k	(Q11) get along with different types of people?

df_post_test$Q3k[df_post_test$Q3k == "Never"] <- 0
df_post_test$Q3k[df_post_test$Q3k == "Rarely"] <- 1
df_post_test$Q3k[df_post_test$Q3k == "Occasionally"] <- 2
df_post_test$Q3k[df_post_test$Q3k == "Frequently"] <- 3
df_post_test$Q3k[df_post_test$Q3k == "Very Frequently"] <- 4
df_post_test$Q3k[df_post_test$Q3k == "Unable to Answer"] <- NA

# Recoding of Q3l	(Q12) try to do her/his best?

df_post_test$Q3l[df_post_test$Q3l == "Never"] <- 0
df_post_test$Q3l[df_post_test$Q3l == "Rarely"] <- 1
df_post_test$Q3l[df_post_test$Q3l == "Occasionally"] <- 2
df_post_test$Q3l[df_post_test$Q3l == "Frequently"] <- 3
df_post_test$Q3l[df_post_test$Q3l == "Very Frequently"] <- 4
df_post_test$Q3l[df_post_test$Q3l == "Unable to Answer"] <- NA

# Recoding of Q3m	(Q13) seek out additional knowledge or information?

df_post_test$Q3m[df_post_test$Q3m == "Never"] <- 0
df_post_test$Q3m[df_post_test$Q3m == "Rarely"] <- 1
df_post_test$Q3m[df_post_test$Q3m == "Occasionally"] <- 2
df_post_test$Q3m[df_post_test$Q3m == "Frequently"] <- 3
df_post_test$Q3m[df_post_test$Q3m == "Very Frequently"] <- 4
df_post_test$Q3m[df_post_test$Q3m == "Unable to Answer"] <- NA

# Recoding of Q3n	(Q14) take an active role in learning?

df_post_test$Q3n[df_post_test$Q3n == "Never"] <- 0
df_post_test$Q3n[df_post_test$Q3n == "Rarely"] <- 1
df_post_test$Q3n[df_post_test$Q3n == "Occasionally"] <- 2
df_post_test$Q3n[df_post_test$Q3n == "Frequently"] <- 3
df_post_test$Q3n[df_post_test$Q3n == "Very Frequently"] <- 4
df_post_test$Q3n[df_post_test$Q3n == "Unable to Answer"] <- NA

# Recoding of Q3o	(Q15) do things independently?

df_post_test$Q3o[df_post_test$Q3o == "Never"] <- 0
df_post_test$Q3o[df_post_test$Q3o == "Rarely"] <- 1
df_post_test$Q3o[df_post_test$Q3o == "Occasionally"] <- 2
df_post_test$Q3o[df_post_test$Q3o == "Frequently"] <- 3
df_post_test$Q3o[df_post_test$Q3o == "Very Frequently"] <- 4
df_post_test$Q3o[df_post_test$Q3o == "Unable to Answer"] <- NA

# Recoding of Q3p	(Q16) say good things about his/her classmates?

df_post_test$Q3p[df_post_test$Q3p == "Never"] <- 0
df_post_test$Q3p[df_post_test$Q3p == "Rarely"] <- 1
df_post_test$Q3p[df_post_test$Q3p == "Occasionally"] <- 2
df_post_test$Q3p[df_post_test$Q3p == "Frequently"] <- 3
df_post_test$Q3p[df_post_test$Q3p == "Very Frequently"] <- 4
df_post_test$Q3p[df_post_test$Q3p == "Unable to Answer"] <- NA

# Recoding of Q3q	(Q17) act respectfully in a game or competition?

df_post_test$Q3q[df_post_test$Q3q == "Never"] <- 0
df_post_test$Q3q[df_post_test$Q3q == "Rarely"] <- 1
df_post_test$Q3q[df_post_test$Q3q == "Occasionally"] <- 2
df_post_test$Q3q[df_post_test$Q3q == "Frequently"] <- 3
df_post_test$Q3q[df_post_test$Q3q == "Very Frequently"] <- 4
df_post_test$Q3q[df_post_test$Q3q == "Unable to Answer"] <- NA

# Recoding of Q3r	(Q18) ask to take on additional work or responsibilities?

df_post_test$Q3r[df_post_test$Q3r == "Never"] <- 0
df_post_test$Q3r[df_post_test$Q3r == "Rarely"] <- 1
df_post_test$Q3r[df_post_test$Q3r == "Occasionally"] <- 2
df_post_test$Q3r[df_post_test$Q3r == "Frequently"] <- 3
df_post_test$Q3r[df_post_test$Q3r == "Very Frequently"] <- 4
df_post_test$Q3r[df_post_test$Q3r == "Unable to Answer"] <- NA

# Recoding of Q3s	(Q19) respect another persons opinion?

df_post_test$Q3s[df_post_test$Q3s == "Never"] <- 0
df_post_test$Q3s[df_post_test$Q3s == "Rarely"] <- 1
df_post_test$Q3s[df_post_test$Q3s == "Occasionally"] <- 2
df_post_test$Q3s[df_post_test$Q3s == "Frequently"] <- 3
df_post_test$Q3s[df_post_test$Q3s == "Very Frequently"] <- 4
df_post_test$Q3s[df_post_test$Q3s == "Unable to Answer"] <- NA

# Recoding of Q3t	(Q20) encourage positive behavior in others?

df_post_test$Q3t[df_post_test$Q3t == "Never"] <- 0
df_post_test$Q3t[df_post_test$Q3t == "Rarely"] <- 1
df_post_test$Q3t[df_post_test$Q3t == "Occasionally"] <- 2
df_post_test$Q3t[df_post_test$Q3t == "Frequently"] <- 3
df_post_test$Q3t[df_post_test$Q3t == "Very Frequently"] <- 4
df_post_test$Q3t[df_post_test$Q3t == "Unable to Answer"] <- NA

# Recoding of Q3u	(Q21) prepare for school, activities, or upcoming events?

df_post_test$Q3u[df_post_test$Q3u == "Never"] <- 0
df_post_test$Q3u[df_post_test$Q3u == "Rarely"] <- 1
df_post_test$Q3u[df_post_test$Q3u == "Occasionally"] <- 2
df_post_test$Q3u[df_post_test$Q3u == "Frequently"] <- 3
df_post_test$Q3u[df_post_test$Q3u == "Very Frequently"] <- 4
df_post_test$Q3u[df_post_test$Q3u == "Unable to Answer"] <- NA

# Recoding of Q3v	(Q22) contribute to group efforts?

df_post_test$Q3v[df_post_test$Q3v == "Never"] <- 0
df_post_test$Q3v[df_post_test$Q3v == "Rarely"] <- 1
df_post_test$Q3v[df_post_test$Q3v == "Occasionally"] <- 2
df_post_test$Q3v[df_post_test$Q3v == "Frequently"] <- 3
df_post_test$Q3v[df_post_test$Q3v == "Very Frequently"] <- 4
df_post_test$Q3v[df_post_test$Q3v == "Unable to Answer"] <- NA

# Recoding of Q3w	(Q23) do routine tasks or chores without being reminded?

df_post_test$Q3w[df_post_test$Q3w == "Never"] <- 0
df_post_test$Q3w[df_post_test$Q3w == "Rarely"] <- 1
df_post_test$Q3w[df_post_test$Q3w == "Occasionally"] <- 2
df_post_test$Q3w[df_post_test$Q3w == "Frequently"] <- 3
df_post_test$Q3w[df_post_test$Q3w == "Very Frequently"] <- 4
df_post_test$Q3w[df_post_test$Q3w == "Unable to Answer"] <- NA

# Recoding of Q3x	(Q24) act as a leader in a peer group?

df_post_test$Q3x[df_post_test$Q3x == "Never"] <- 0
df_post_test$Q3x[df_post_test$Q3x == "Rarely"] <- 1
df_post_test$Q3x[df_post_test$Q3x == "Occasionally"] <- 2
df_post_test$Q3x[df_post_test$Q3x == "Frequently"] <- 3
df_post_test$Q3x[df_post_test$Q3x == "Very Frequently"] <- 4
df_post_test$Q3x[df_post_test$Q3x == "Unable to Answer"] <- NA

# Recoding of Q3y	(Q25) resolve a disagreement?

df_post_test$Q3y[df_post_test$Q3y == "Never"] <- 0
df_post_test$Q3y[df_post_test$Q3y == "Rarely"] <- 1
df_post_test$Q3y[df_post_test$Q3y == "Occasionally"] <- 2
df_post_test$Q3y[df_post_test$Q3y == "Frequently"] <- 3
df_post_test$Q3y[df_post_test$Q3y == "Very Frequently"] <- 4
df_post_test$Q3y[df_post_test$Q3y == "Unable to Answer"] <- NA

# Recoding of Q3z	(Q26) show creativity in completing a task?

df_post_test$Q3z[df_post_test$Q3z == "Never"] <- 0
df_post_test$Q3z[df_post_test$Q3z == "Rarely"] <- 1
df_post_test$Q3z[df_post_test$Q3z == "Occasionally"] <- 2
df_post_test$Q3z[df_post_test$Q3z == "Frequently"] <- 3
df_post_test$Q3z[df_post_test$Q3z == "Very Frequently"] <- 4
df_post_test$Q3z[df_post_test$Q3z == "Unable to Answer"] <- NA

# Recoding of Q3aa (Q27) share with others?

df_post_test$Q3aa[df_post_test$Q3aa == "Never"] <- 0
df_post_test$Q3aa[df_post_test$Q3aa == "Rarely"] <- 1
df_post_test$Q3aa[df_post_test$Q3aa == "Occasionally"] <- 2
df_post_test$Q3aa[df_post_test$Q3aa == "Frequently"] <- 3
df_post_test$Q3aa[df_post_test$Q3aa == "Very Frequently"] <- 4
df_post_test$Q3aa[df_post_test$Q3aa == "Unable to Answer"] <- NA

# Recoding of Q3ab (Q 28)	get things done in a timely fashion?

df_post_test$Q3ab[df_post_test$Q3ab == "Never"] <- 0
df_post_test$Q3ab[df_post_test$Q3ab == "Rarely"] <- 1
df_post_test$Q3ab[df_post_test$Q3ab == "Occasionally"] <- 2
df_post_test$Q3ab[df_post_test$Q3ab == "Frequently"] <- 3
df_post_test$Q3ab[df_post_test$Q3ab == "Very Frequently"] <- 4
df_post_test$Q3ab[df_post_test$Q3ab == "Unable to Answer"] <- NA

# Recoding of Q3ac (Q29) seek out challenging tasks?

df_post_test$Q3ac[df_post_test$Q3ac == "Never"] <- 0
df_post_test$Q3ac[df_post_test$Q3ac == "Rarely"] <- 1
df_post_test$Q3ac[df_post_test$Q3ac == "Occasionally"] <- 2
df_post_test$Q3ac[df_post_test$Q3ac == "Frequently"] <- 3
df_post_test$Q3ac[df_post_test$Q3ac == "Very Frequently"] <- 4
df_post_test$Q3ac[df_post_test$Q3ac == "Unable to Answer"] <- NA

# Recoding of Q3ad (Q30) say good things about the future?
df_post_test$Q3ad[df_post_test$Q3ad == "Never"] <- 0
df_post_test$Q3ad[df_post_test$Q3ad == "Rarely"] <- 1
df_post_test$Q3ad[df_post_test$Q3ad == "Occasionally"] <- 2
df_post_test$Q3ad[df_post_test$Q3ad == "Frequently"] <- 3
df_post_test$Q3ad[df_post_test$Q3ad == "Very Frequently"] <- 4
df_post_test$Q3ad[df_post_test$Q3ad == "Unable to Answer"] <- NA

# Recoding of Q3ae (Q31) cooperate with peers or siblings?

df_post_test$Q3ae[df_post_test$Q3ae == "Never"] <- 0
df_post_test$Q3ae[df_post_test$Q3ae == "Rarely"] <- 1
df_post_test$Q3ae[df_post_test$Q3ae == "Occasionally"] <- 2
df_post_test$Q3ae[df_post_test$Q3ae == "Frequently"] <- 3
df_post_test$Q3ae[df_post_test$Q3ae == "Very Frequently"] <- 4
df_post_test$Q3ae[df_post_test$Q3ae == "Unable to Answer"] <- NA

# Recoding of Q3af (Q32) show care when doing a project or school work?

df_post_test$Q3af[df_post_test$Q3af == "Never"] <- 0
df_post_test$Q3af[df_post_test$Q3af == "Rarely"] <- 1
df_post_test$Q3af[df_post_test$Q3af == "Occasionally"] <- 2
df_post_test$Q3af[df_post_test$Q3af == "Frequently"] <- 3
df_post_test$Q3af[df_post_test$Q3af == "Very Frequently"] <- 4
df_post_test$Q3af[df_post_test$Q3af == "Unable to Answer"] <- NA

# Recoding of Q3ag (Q33) work hard on projects?

df_post_test$Q3ag[df_post_test$Q3ag == "Never"] <- 0
df_post_test$Q3ag[df_post_test$Q3ag == "Rarely"] <- 1
df_post_test$Q3ag[df_post_test$Q3ag == "Occasionally"] <- 2
df_post_test$Q3ag[df_post_test$Q3ag == "Frequently"] <- 3
df_post_test$Q3ag[df_post_test$Q3ag == "Very Frequently"] <- 4
df_post_test$Q3ag[df_post_test$Q3ag == "Unable to Answer"] <- NA

# Recoding of Q3ah (Q34) forgive somebody who hurt or upset her/him?

df_post_test$Q3ah[df_post_test$Q3ah == "Never"] <- 0
df_post_test$Q3ah[df_post_test$Q3ah == "Rarely"] <- 1
df_post_test$Q3ah[df_post_test$Q3ah == "Occasionally"] <- 2
df_post_test$Q3ah[df_post_test$Q3ah == "Frequently"] <- 3
df_post_test$Q3ah[df_post_test$Q3ah == "Very Frequently"] <- 4
df_post_test$Q3ah[df_post_test$Q3ah == "Unable to Answer"] <- NA

# Recoding Q3ai	(Q35) follow rules?

df_post_test$Q3ai[df_post_test$Q3ai == "Never"] <- 0
df_post_test$Q3ai[df_post_test$Q3ai == "Rarely"] <- 1
df_post_test$Q3ai[df_post_test$Q3ai == "Occasionally"] <- 2
df_post_test$Q3ai[df_post_test$Q3ai == "Frequently"] <- 3
df_post_test$Q3ai[df_post_test$Q3ai == "Very Frequently"] <- 4
df_post_test$Q3ai[df_post_test$Q3ai == "Unable to Answer"] <- NA

# Recoding Q3aj	(Q36) express high expectations for himself/herself?

df_post_test$Q3aj[df_post_test$Q3aj == "Never"] <- 0
df_post_test$Q3aj[df_post_test$Q3aj == "Rarely"] <- 1
df_post_test$Q3aj[df_post_test$Q3aj == "Occasionally"] <- 2
df_post_test$Q3aj[df_post_test$Q3aj == "Frequently"] <- 3
df_post_test$Q3aj[df_post_test$Q3aj == "Very Frequently"] <- 4
df_post_test$Q3aj[df_post_test$Q3aj == "Unable to Answer"] <- NA

# Recoding of Q3ak (Q37) follow the example of a positive role model?

df_post_test$Q3ak[df_post_test$Q3ak == "Never"] <- 0
df_post_test$Q3ak[df_post_test$Q3ak == "Rarely"] <- 1
df_post_test$Q3ak[df_post_test$Q3ak == "Occasionally"] <- 2
df_post_test$Q3ak[df_post_test$Q3ak == "Frequently"] <- 3
df_post_test$Q3ak[df_post_test$Q3ak == "Very Frequently"] <- 4
df_post_test$Q3ak[df_post_test$Q3ak == "Unable to Answer"] <- NA

# Recoding of Q3al (Q38) compliment or congratulate somebody?

df_post_test$Q3al[df_post_test$Q3al == "Never"] <- 0
df_post_test$Q3al[df_post_test$Q3al == "Rarely"] <- 1
df_post_test$Q3al[df_post_test$Q3al == "Occasionally"] <- 2
df_post_test$Q3al[df_post_test$Q3al == "Frequently"] <- 3
df_post_test$Q3al[df_post_test$Q3al == "Very Frequently"] <- 4
df_post_test$Q3al[df_post_test$Q3al == "Unable to Answer"] <- NA

# Recoding of Q3am	(Q39) accept responsibility for what she/he did?

df_post_test$Q3am[df_post_test$Q3am == "Never"] <- 0
df_post_test$Q3am[df_post_test$Q3am == "Rarely"] <- 1
df_post_test$Q3am[df_post_test$Q3am == "Occasionally"] <- 2
df_post_test$Q3am[df_post_test$Q3am == "Frequently"] <- 3
df_post_test$Q3am[df_post_test$Q3am == "Very Frequently"] <- 4
df_post_test$Q3am[df_post_test$Q3am == "Unable to Answer"] <- NA

# Recoding of Q3an	(Q40) do something nice for somebody?

df_post_test$Q3an[df_post_test$Q3an == "Never"] <- 0
df_post_test$Q3an[df_post_test$Q3an == "Rarely"] <- 1
df_post_test$Q3an[df_post_test$Q3an == "Occasionally"] <- 2
df_post_test$Q3an[df_post_test$Q3an == "Frequently"] <- 3
df_post_test$Q3an[df_post_test$Q3an == "Very Frequently"] <- 4
df_post_test$Q3an[df_post_test$Q3an == "Unable to Answer"] <- NA

# Recoding of Q3ao (Q41) make accurate statements about events in her/his life?

df_post_test$Q3ao[df_post_test$Q3ao == "Never"] <- 0
df_post_test$Q3ao[df_post_test$Q3ao == "Rarely"] <- 1
df_post_test$Q3ao[df_post_test$Q3ao == "Occasionally"] <- 2
df_post_test$Q3ao[df_post_test$Q3ao == "Frequently"] <- 3
df_post_test$Q3ao[df_post_test$Q3ao == "Very Frequently"] <- 4
df_post_test$Q3ao[df_post_test$Q3ao == "Unable to Answer"] <- NA

# Recoding of Q3ap (Q42) show good judgment?

df_post_test$Q3ap[df_post_test$Q3ap == "Never"] <- 0
df_post_test$Q3ap[df_post_test$Q3ap == "Rarely"] <- 1
df_post_test$Q3ap[df_post_test$Q3ap == "Occasionally"] <- 2
df_post_test$Q3ap[df_post_test$Q3ap == "Frequently"] <- 3
df_post_test$Q3ap[df_post_test$Q3ap == "Very Frequently"] <- 4
df_post_test$Q3ap[df_post_test$Q3ap == "Unable to Answer"] <- NA

# Recode of the Q3aq (Q43) pay attention?

df_post_test$Q3aq[df_post_test$Q3aq == "Never"] <- 0
df_post_test$Q3aq[df_post_test$Q3aq == "Rarely"] <- 1
df_post_test$Q3aq[df_post_test$Q3aq == "Occasionally"] <- 2
df_post_test$Q3aq[df_post_test$Q3aq == "Frequently"] <- 3
df_post_test$Q3aq[df_post_test$Q3aq == "Very Frequently"] <- 4
df_post_test$Q3aq[df_post_test$Q3aq == "Unable to Answer"] <- NA

# Recode of Q3ar (Q44) wait for her/his turn?

df_post_test$Q3ar[df_post_test$Q3ar == "Never"] <- 0
df_post_test$Q3ar[df_post_test$Q3ar == "Rarely"] <- 1
df_post_test$Q3ar[df_post_test$Q3ar == "Occasionally"] <- 2
df_post_test$Q3ar[df_post_test$Q3ar == "Frequently"] <- 3
df_post_test$Q3ar[df_post_test$Q3ar == "Very Frequently"] <- 4
df_post_test$Q3ar[df_post_test$Q3ar == "Unable to Answer"] <- NA

# Recoding of the Q3as (Q45) show appreciation of others?

df_post_test$Q3as[df_post_test$Q3as == "Never"] <- 0
df_post_test$Q3as[df_post_test$Q3as == "Rarely"] <- 1
df_post_test$Q3as[df_post_test$Q3as == "Occasionally"] <- 2
df_post_test$Q3as[df_post_test$Q3as == "Frequently"] <- 3
df_post_test$Q3as[df_post_test$Q3as == "Very Frequently"] <- 4
df_post_test$Q3as[df_post_test$Q3as == "Unable to Answer"] <- NA

# Recoding Q3at	(Q46) focus on a task despite a problem or distraction?

df_post_test$Q3at[df_post_test$Q3at == "Never"] <- 0
df_post_test$Q3at[df_post_test$Q3at == "Rarely"] <- 1
df_post_test$Q3at[df_post_test$Q3at == "Occasionally"] <- 2
df_post_test$Q3at[df_post_test$Q3at == "Frequently"] <- 3
df_post_test$Q3at[df_post_test$Q3at == "Very Frequently"] <- 4
df_post_test$Q3at[df_post_test$Q3at == "Unable to Answer"] <- NA

# Recoding Q3au	(Q47) greet a person in a polite way?

df_post_test$Q3au[df_post_test$Q3au == "Never"] <- 0
df_post_test$Q3au[df_post_test$Q3au == "Rarely"] <- 1
df_post_test$Q3au[df_post_test$Q3au == "Occasionally"] <- 2
df_post_test$Q3au[df_post_test$Q3au == "Frequently"] <- 3
df_post_test$Q3au[df_post_test$Q3au == "Very Frequently"] <- 4
df_post_test$Q3au[df_post_test$Q3au == "Unable to Answer"] <- NA

# Recoding Q3av	(Q48) act comfortable in a new situation?

df_post_test$Q3av[df_post_test$Q3av == "Never"] <- 0
df_post_test$Q3av[df_post_test$Q3av == "Rarely"] <- 1
df_post_test$Q3av[df_post_test$Q3av == "Occasionally"] <- 2
df_post_test$Q3av[df_post_test$Q3av == "Frequently"] <- 3
df_post_test$Q3av[df_post_test$Q3av == "Very Frequently"] <- 4
df_post_test$Q3av[df_post_test$Q3av == "Unable to Answer"] <- NA

# Recoding Q3aw	(Q49) teach another person to do something?

df_post_test$Q3aw[df_post_test$Q3aw == "Never"] <- 0
df_post_test$Q3aw[df_post_test$Q3aw == "Rarely"] <- 1
df_post_test$Q3aw[df_post_test$Q3aw == "Occasionally"] <- 2
df_post_test$Q3aw[df_post_test$Q3aw == "Frequently"] <- 3
df_post_test$Q3aw[df_post_test$Q3aw == "Very Frequently"] <- 4
df_post_test$Q3aw[df_post_test$Q3aw == "Unable to Answer"] <- NA

# Recoding Q3ax	(Q50) attract positive attention from peers?

df_post_test$Q3ax[df_post_test$Q3ax == "Never"] <- 0
df_post_test$Q3ax[df_post_test$Q3ax == "Rarely"] <- 1
df_post_test$Q3ax[df_post_test$Q3ax == "Occasionally"] <- 2
df_post_test$Q3ax[df_post_test$Q3ax == "Frequently"] <- 3
df_post_test$Q3ax[df_post_test$Q3ax == "Very Frequently"] <- 4
df_post_test$Q3ax[df_post_test$Q3ax == "Unable to Answer"] <- NA

# Recoding Q3ay	(Q51) perform the steps of a task in order?

df_post_test$Q3ay[df_post_test$Q3ay == "Never"] <- 0
df_post_test$Q3ay[df_post_test$Q3ay == "Rarely"] <- 1
df_post_test$Q3ay[df_post_test$Q3ay == "Occasionally"] <- 2
df_post_test$Q3ay[df_post_test$Q3ay == "Frequently"] <- 3
df_post_test$Q3ay[df_post_test$Q3ay == "Very Frequently"] <- 4
df_post_test$Q3ay[df_post_test$Q3ay == "Unable to Answer"] <- NA

# Recoding Q3az	(Q52) seek advice?

df_post_test$Q3az[df_post_test$Q3az == "Never"] <- 0
df_post_test$Q3az[df_post_test$Q3az == "Rarely"] <- 1
df_post_test$Q3az[df_post_test$Q3az == "Occasionally"] <- 2
df_post_test$Q3az[df_post_test$Q3az == "Frequently"] <- 3
df_post_test$Q3az[df_post_test$Q3az == "Very Frequently"] <- 4
df_post_test$Q3az[df_post_test$Q3az == "Unable to Answer"] <- NA

# Recoding Q3ba	(Q53) think before he/she acted?

df_post_test$Q3ba[df_post_test$Q3ba == "Never"] <- 0
df_post_test$Q3ba[df_post_test$Q3ba == "Rarely"] <- 1
df_post_test$Q3ba[df_post_test$Q3ba == "Occasionally"] <- 2
df_post_test$Q3ba[df_post_test$Q3ba == "Frequently"] <- 3
df_post_test$Q3ba[df_post_test$Q3ba == "Very Frequently"] <- 4
df_post_test$Q3ba[df_post_test$Q3ba == "Unable to Answer"] <- NA

# Recoding Q3bb	(Q54) pass up something he/she wanted, or do something he/she did not like, to get something better in the future?

df_post_test$Q3bb[df_post_test$Q3bb == "Never"] <- 0
df_post_test$Q3bb[df_post_test$Q3bb == "Rarely"] <- 1
df_post_test$Q3bb[df_post_test$Q3bb == "Occasionally"] <- 2
df_post_test$Q3bb[df_post_test$Q3bb == "Frequently"] <- 3
df_post_test$Q3bb[df_post_test$Q3bb == "Very Frequently"] <- 4
df_post_test$Q3bb[df_post_test$Q3bb == "Unable to Answer"] <- NA

# Recoding Q3bc (Q55) express concern for another person?

df_post_test$Q3bc[df_post_test$Q3bc == "Never"] <- 0
df_post_test$Q3bc[df_post_test$Q3bc == "Rarely"] <- 1
df_post_test$Q3bc[df_post_test$Q3bc == "Occasionally"] <- 2
df_post_test$Q3bc[df_post_test$Q3bc == "Frequently"] <- 3
df_post_test$Q3bc[df_post_test$Q3bc == "Very Frequently"] <- 4
df_post_test$Q3bc[df_post_test$Q3bc == "Unable to Answer"] <- NA

# Recoding Q3bd	(Q56) accept another choice when his/her first choice was unacceptable?

df_post_test$Q3bd[df_post_test$Q3bd == "Never"] <- 0
df_post_test$Q3bd[df_post_test$Q3bd == "Rarely"] <- 1
df_post_test$Q3bd[df_post_test$Q3bd == "Occasionally"] <- 2
df_post_test$Q3bd[df_post_test$Q3bd == "Frequently"] <- 3
df_post_test$Q3bd[df_post_test$Q3bd == "Very Frequently"] <- 4
df_post_test$Q3bd[df_post_test$Q3bd == "Unable to Answer"] <- NA

# Recoding Q3be	(Q57) ask questions to clarify what he/she did not understand?

df_post_test$Q3be[df_post_test$Q3be == "Never"] <- 0
df_post_test$Q3be[df_post_test$Q3be == "Rarely"] <- 1
df_post_test$Q3be[df_post_test$Q3be == "Occasionally"] <- 2
df_post_test$Q3be[df_post_test$Q3be == "Frequently"] <- 3
df_post_test$Q3be[df_post_test$Q3be == "Very Frequently"] <- 4
df_post_test$Q3be[df_post_test$Q3be == "Unable to Answer"] <- NA

# Recoding ... Q3bf	(Q58) show an awareness of her/his personal strengths?

df_post_test$Q3bf[df_post_test$Q3bf == "Never"] <- 0
df_post_test$Q3bf[df_post_test$Q3bf == "Rarely"] <- 1
df_post_test$Q3bf[df_post_test$Q3bf == "Occasionally"] <- 2
df_post_test$Q3bf[df_post_test$Q3bf == "Frequently"] <- 3
df_post_test$Q3bf[df_post_test$Q3bf == "Very Frequently"] <- 4
df_post_test$Q3bf[df_post_test$Q3bf == "Unable to Answer"] <- NA

# Recoding of Q3bg (Q59) ask somebody for feedback?

df_post_test$Q3bg[df_post_test$Q3bg == "Never"] <- 0
df_post_test$Q3bg[df_post_test$Q3bg == "Rarely"] <- 1
df_post_test$Q3bg[df_post_test$Q3bg == "Occasionally"] <- 2
df_post_test$Q3bg[df_post_test$Q3bg == "Frequently"] <- 3
df_post_test$Q3bg[df_post_test$Q3bg == "Very Frequently"] <- 4
df_post_test$Q3bg[df_post_test$Q3bg == "Unable to Answer"] <- NA

# Recoding of Q3bh (Q60) stay calm when faced with a challenge?

df_post_test$Q3bh[df_post_test$Q3bh == "Never"] <- 0
df_post_test$Q3bh[df_post_test$Q3bh == "Rarely"] <- 1
df_post_test$Q3bh[df_post_test$Q3bh == "Occasionally"] <- 2
df_post_test$Q3bh[df_post_test$Q3bh == "Frequently"] <- 3
df_post_test$Q3bh[df_post_test$Q3bh == "Very Frequently"] <- 4
df_post_test$Q3bh[df_post_test$Q3bh == "Unable to Answer"] <- NA

# Recoding of Q3bi (Q61) attract positive attention from adults?

df_post_test$Q3bi[df_post_test$Q3bi == "Never"] <- 0
df_post_test$Q3bi[df_post_test$Q3bi == "Rarely"] <- 1
df_post_test$Q3bi[df_post_test$Q3bi == "Occasionally"] <- 2
df_post_test$Q3bi[df_post_test$Q3bi == "Frequently"] <- 3
df_post_test$Q3bi[df_post_test$Q3bi == "Very Frequently"] <- 4
df_post_test$Q3bi[df_post_test$Q3bi == "Unable to Answer"] <- NA

# Recoding of Q3bj	(Q62) describe how he/she was feeling?

df_post_test$Q3bj[df_post_test$Q3bj == "Never"] <- 0
df_post_test$Q3bj[df_post_test$Q3bj == "Rarely"] <- 1
df_post_test$Q3bj[df_post_test$Q3bj == "Occasionally"] <- 2
df_post_test$Q3bj[df_post_test$Q3bj == "Frequently"] <- 3
df_post_test$Q3bj[df_post_test$Q3bj == "Very Frequently"] <- 4
df_post_test$Q3bj[df_post_test$Q3bj == "Unable to Answer"] <- NA

# Recoding of Q3bk (Q63) give an opinion when asked?

df_post_test$Q3bk[df_post_test$Q3bk == "Never"] <- 0
df_post_test$Q3bk[df_post_test$Q3bk == "Rarely"] <- 1
df_post_test$Q3bk[df_post_test$Q3bk == "Occasionally"] <- 2
df_post_test$Q3bk[df_post_test$Q3bk == "Frequently"] <- 3
df_post_test$Q3bk[df_post_test$Q3bk == "Very Frequently"] <- 4
df_post_test$Q3bk[df_post_test$Q3bk == "Unable to Answer"] <- NA

# Recoding of Q3bl (Q64) make a suggestion or request in a polite way?

df_post_test$Q3bl[df_post_test$Q3bl == "Never"] <- 0
df_post_test$Q3bl[df_post_test$Q3bl == "Rarely"] <- 1
df_post_test$Q3bl[df_post_test$Q3bl == "Occasionally"] <- 2
df_post_test$Q3bl[df_post_test$Q3bl == "Frequently"] <- 3
df_post_test$Q3bl[df_post_test$Q3bl == "Very Frequently"] <- 4
df_post_test$Q3bl[df_post_test$Q3bl == "Unable to Answer"] <- NA

# Recoding of Q3bm (Q65) learn from experience?

df_post_test$Q3bm[df_post_test$Q3bm == "Never"] <- 0
df_post_test$Q3bm[df_post_test$Q3bm == "Rarely"] <- 1
df_post_test$Q3bm[df_post_test$Q3bm == "Occasionally"] <- 2
df_post_test$Q3bm[df_post_test$Q3bm == "Frequently"] <- 3
df_post_test$Q3bm[df_post_test$Q3bm == "Very Frequently"] <- 4
df_post_test$Q3bm[df_post_test$Q3bm == "Unable to Answer"] <- NA

# Recoding of Q3bn	(Q66) follow the advice of a trusted adults?

df_post_test$Q3bn[df_post_test$Q3bn == "Never"] <- 0
df_post_test$Q3bn[df_post_test$Q3bn == "Rarely"] <- 1
df_post_test$Q3bn[df_post_test$Q3bn == "Occasionally"] <- 2
df_post_test$Q3bn[df_post_test$Q3bn == "Frequently"] <- 3
df_post_test$Q3bn[df_post_test$Q3bn == "Very Frequently"] <- 4
df_post_test$Q3bn[df_post_test$Q3bn == "Unable to Answer"] <- NA

# Let's recode Q3bo	(Q67) adjust well to changes in plans?

df_post_test$Q3bo[df_post_test$Q3bo == "Never"] <- 0
df_post_test$Q3bo[df_post_test$Q3bo == "Rarely"] <- 1
df_post_test$Q3bo[df_post_test$Q3bo == "Occasionally"] <- 2
df_post_test$Q3bo[df_post_test$Q3bo == "Frequently"] <- 3
df_post_test$Q3bo[df_post_test$Q3bo == "Very Frequently"] <- 4
df_post_test$Q3bo[df_post_test$Q3bo == "Unable to Answer"] <- NA

# Recoding Q3bp	(Q68) show the ability to decide between right and wrong?

df_post_test$Q3bp[df_post_test$Q3bp == "Never"] <- 0
df_post_test$Q3bp[df_post_test$Q3bp == "Rarely"] <- 1
df_post_test$Q3bp[df_post_test$Q3bp == "Occasionally"] <- 2
df_post_test$Q3bp[df_post_test$Q3bp == "Frequently"] <- 3
df_post_test$Q3bp[df_post_test$Q3bp == "Very Frequently"] <- 4
df_post_test$Q3bp[df_post_test$Q3bp == "Unable to Answer"] <- NA

# Recoding of Q3bq (Q69) use available resources (people or objects) to solve a problem?

df_post_test$Q3bq[df_post_test$Q3bq == "Never"] <- 0
df_post_test$Q3bq[df_post_test$Q3bq == "Rarely"] <- 1
df_post_test$Q3bq[df_post_test$Q3bq == "Occasionally"] <- 2
df_post_test$Q3bq[df_post_test$Q3bq == "Frequently"] <- 3
df_post_test$Q3bq[df_post_test$Q3bq == "Very Frequently"] <- 4
df_post_test$Q3bq[df_post_test$Q3bq == "Unable to Answer"] <- NA

# Recoding of Q3br (Q70) offer to help somebody?

df_post_test$Q3br[df_post_test$Q3br == "Never"] <- 0
df_post_test$Q3br[df_post_test$Q3br == "Rarely"] <- 1
df_post_test$Q3br[df_post_test$Q3br == "Occasionally"] <- 2
df_post_test$Q3br[df_post_test$Q3br == "Frequently"] <- 3
df_post_test$Q3br[df_post_test$Q3br == "Very Frequently"] <- 4
df_post_test$Q3br[df_post_test$Q3br == "Unable to Answer"] <- NA

# Recoding of Q3bs (Q71) respond to another persons feelings?

df_post_test$Q3bs[df_post_test$Q3bs == "Never"] <- 0
df_post_test$Q3bs[df_post_test$Q3bs == "Rarely"] <- 1
df_post_test$Q3bs[df_post_test$Q3bs == "Occasionally"] <- 2
df_post_test$Q3bs[df_post_test$Q3bs == "Frequently"] <- 3
df_post_test$Q3bs[df_post_test$Q3bs == "Very Frequently"] <- 4
df_post_test$Q3bs[df_post_test$Q3bs == "Unable to Answer"] <- NA

# Recoding of Q3bt (Q72) adjust well when going from one setting to another?

df_post_test$Q3bt[df_post_test$Q3bt == "Never"] <- 0
df_post_test$Q3bt[df_post_test$Q3bt == "Rarely"] <- 1
df_post_test$Q3bt[df_post_test$Q3bt == "Occasionally"] <- 2
df_post_test$Q3bt[df_post_test$Q3bt == "Frequently"] <- 3
df_post_test$Q3bt[df_post_test$Q3bt == "Very Frequently"] <- 4
df_post_test$Q3bt[df_post_test$Q3bt == "Unable to Answer"] <- NA


# ---- 1.3 Renaming the columns ----
# Viewing the dataset and the recoding so far:
View(df_post_test)

# We will remove the row in the dataset that represents the naratives of the questions 
# after verifying that the questions in the dataset match the ones in the questionnaire;
# But, before let's rename the columns :
names(df_post_test) <-c("Your_ID", "Child_ID", "Q_knowlege_of_child",
                        "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10",
                        "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23",
                        "Q24", "Q25", "Q26", "Q27", "Q28", "Q29", "Q30", "Q31", "Q32", "Q33", "Q34", "Q35", "Q36", "Q37",
                        "Q38", "Q39", "Q40", "Q41", "Q42", "Q43", "Q44", "Q45", "Q46", "Q47", "Q48", "Q49", "Q50",
                        "Q51", "Q52", "Q53", "Q54", "Q55", "Q56", "Q57", "Q58", "Q59", "Q60", "Q61", "Q62", "Q63", "Q64",
                        "Q65", "Q66", "Q67", "Q68", "Q69", "Q70", "Q71", "Q72", "Q_answered")

# We do this to avoide errors and the verify that all is correct.
# Verifications ... the renamed questions corresponds to the questionnaire questions:
# And the ID
View(df_post_test)

# Removing the naratives .... the 1rst row of the dataset :
# Remember [rows, col]

df_post_test <- df_post_test[-1, ]


# We will be using a "full" joint to keep all the observation in the 3 datasets 
# This process is case sensitive, thus we have to convert all the ID to uppercase string characters - toupper()

df_post_test$Your_ID = toupper(df_post_test$Your_ID)
View(df_post_test)

# There are a few cells with errors in the ID. WE can change it directly within the data frame by indexing
df_post_test$Your_ID[67] <- "MB2"
df_post_test$Your_ID[18] <- "JS7"
df_post_test$Your_ID[302] <- "MG18"
df_post_test$Your_ID[303] <- "MG18"
df_post_test$Your_ID[304] <- "MG18"

# Verifying that all is correct
table(df_post_test$Your_ID)


# ---- 1.4 Transforming the variables into the appropriate format ----
str(df_post_test)

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

df_post_test[cols_to_numerize] <- sapply(df_post_test[cols_to_numerize], as.numeric)

# Quick note :
# When using a data frame the data types must all be the same type or class 
# otherwise they will be subjected to type conversion.
# Other info ... see documentations about the apply(), tapply() and sapply() family of functions 

# Verifying that all is correct ....
sapply(df_post_test, class)
str(df_post_test) # same thing ...
