# Title:    Devereux Student Strengths Assessment (DESSA) - Spring 2019
# Author:   Annick Eudes Jean-Baptiste - Research Manager
# Date:     July 22, 2019
# Version:  1.0

# Note : Do not run this script twice, you might get an error on section 1B.3.

# ---- Preliminaries ----



# If you don't have these packages installed in your R work place, use :
#install.packages("tidyverse") # Data cleaning and alot more
#install.packages("ggplot2")   # Graphs
#install.packages("plyr")      # data manipulation - subset of the tydyverse
# install.packages("mice")    # Missing value diagnosis and imputation | MICE (Multivariate Imputation via Chained Equations)
# install.packages("VIM")       # Creates plots to visualize missing data
# install.packages("naniar")    # Creates plots to visualize missing data

# Loading the packages that we might need :

library(tidyverse)
library(ggplot2)
library(plyr)
library(mice)
library(VIM)
library(naniar)

# ---- 2.1 Handling missing values ----

# This script replaces missing values in the DESSA data
# using a method called "multiple imputation."
# The result is a data set called sel_data_imputed.
# In subsequent scripts, calculate raw scale scores for both 
# sel_data and sel_data_imputed.
# This will allow us to compare the results for data with missing values
# and data with imputed values.

# Examine prevalence of missing values

# * Subset the data ----

df_items <- dplyr::select(df_post_test, Your_ID:Q72)
names(df_items) # Verification

# * Determine percent missing for each item ----

cases <- nrow(df_items)
cases
perc_missing <- sapply(df_items, function(x) sum(is.na(x))/cases)
View(perc_missing)

# * Table of missing value patterns ----
# Missing data pattern : md.pattern() - is from the "mice" package.
table_missing <- md.pattern(df_items)
View(table_missing)

# IMPORTANT

# Note that several items have more than 5% missing values
# and about five items have more than 10% missing values
# Multiple imputation is not advised for such items.


# ---- 2.2 Visualize patterns of missing values ----

# * Using the mice package ----
mice_plot <- aggr(df_items, col = c('navyblue','yellow'),
                  numbers = TRUE, sortVars = TRUE,
                  labels = names(df_items), 
                  cex.axis = .7, cex.numbers = 0.3,
                  gap = 3, ylab = c("Missing Data", "Pattern"))

# * Using the naniar package ----
naniar::gg_miss_var(df_items)   # This graph is elegant and intuitive


# ---- 2.3 Impute missing data ----
# Results in new data set: sel_data_imputed

# IMPORTANT :
# Let this peace of code run, it will take some time ... ... do not rush it!!!

# The missing values method computed by the MICE package to deal with missing data has given an error due (I suppose)
# to the method used to treat the missing values and the high possible correlation between the variables (Q1, Q2, Q3, ... or/and Qn).
# We've used so far the "Predictive mean matching" in the methode = 'pmm' argument of the function.
# temp_sel_data <- mice(sel_data_items, m = 5, maxit = 50, meth = 'pmm', seed = 500)

temp_sel_data <- mice(df_items, m = 5, maxit = 50, method = 'sample', seed = 500)

summary(temp_sel_data)

# Complete the imputed data set
sel_data_imputed <- complete(temp_sel_data, 1)

# Check for missing values in the imputed data
sapply(sel_data_imputed, function(x) sum(is.na(x)))

# There are no missing values!
