
# =============================================================================================================
# Project:        Freddie Mac Data
# Discription:    data_wrangling_template.R 
# Purpose:        template for getting data into shape for use by loanportr
# Authors:        N van Heerden, DJ de Villiers
# Date:           3 Oct 2017
# =============================================================================================================


# libraries
# ---------
library(dplyr)
library(readr)
library(tidyverse)
library(readxl)
library(lubridate)
# ---------

# import data 
# -----------
# read_csv from readr saves the file already as a tibble. 
# i.e. the result of a as_data_frame(). 

df <- read_csv("filename.txt")

df1 <- read_excel("filename.xlsx", 
                  col_types = c("numeric", "date", "text",...))

df2 <- read_excel("LGD data2.xlsx", 
                  col_types = c("numeric", "date", "text", 
                                "text", "numeric", "date", "numeric", 
                                "date", "numeric", "numeric", "numeric", 
                                "date", "numeric", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "text", "numeric", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "text", "numeric", "numeric"))
df3 <- etc. 

# join any relational tables if necessary


# -----------

# bind new data to old
df <- bind_rows(df, df1, df2)

# data wrangling
# --------------
# sort correct for easy viewing:
df <- arrange(df, ...)

# delete unnecessary variables / select necessary ones:
df <- df %>% select(-unwanted_variable, ...)

# set names of remaining variables to lower case
# (just helps because you don't have to rename some of them)
names(df) <- tolower(names(df))

# rename variables, colnames(df),  to correct internal naming convention. 
# client segments do not need to be renamed as they are always different.

df <- rename(df, 
       contract_key = , 
       pointintime_month = , 
       product_name = ,
       term = ,
       orig_date = ,
       orig_month = ,
       loan_period = ,
       fpd_month = , 
       fpd_period = ,
       default_month = ,
       months_in_default = ,
       exposure_at_default = ,
       payment = ,
       months_arrears = ,
       instalment  = ,
       closing_balance = 
       )

# make sure all variables are of the right type:
df <- df %>%
  mutate(orig_date = as_date(orig_date)) 

# create/convert variabels that are needed
# orig_month 
last_day <- function(date) {
  ceiling_date(date, "month") - days(1)
}
df$orig_month <- last_day(df$orig_date)

# contract_key: make sure that the contract key is unique, if not create a
# unique version

# re-arrange to get the right order for easy viewing of contracts. 
df <- arrange(df, contract_key, pointintime_month)

# delete any unnecessary rows
df <- df %>% filter(cancelled != "Yes")
# --------------

# do any binning if necessary


# fix any obvious data errors. this should always be well documented. 



# check that all data types match the data dictionary



# If there are any variables that need to be created from what is there, that
# will be dealt with in seperate scripts.




# export data 
# -----------
write_rds(x = df, "df.rds", compress = "gz")
