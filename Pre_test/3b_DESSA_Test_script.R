# Test script

# Import reference table
library(readxl)
reftable <- read_excel("tscores.xlsx")

# Assign data set to temporary data frame for testing
df <- sel_data_imputed

# Create T score for Personal Responsibility by matching with PR_raw in reftable
df$PRTScore.matched <- with(reftable,
                    T[match(df$Personal_Responsibility, PR_raw)])

# Check results:
# Compare T scores obtained using both methods
df_test <- df %>% select(Personal_Responsibility, PRTScore, PRTScore.matched)

# Results of check:
# Values of PRTScore for 14 and 28 are incorrect

# Create T score for Optimistic Thinking by matching with OT_raw in reftable
df$OTTScore.matched <- with(reftable, T[match(df$Optimistic_Thinking, OT_raw)])

                      
# Etc...
