
# =============================================================================================================
# Project:        Freddie Mac Data
# Discription:    data_wrangling.R 
# Purpose:        code for getting freddie mac data into shape for use by loanportr
# Authors:        N van Heerden, DJ de Villiers
# Date:           6 Oct 2017
# =============================================================================================================

# Used the template. Have just commented out the irrelevant parts. 

# libraries
# ---------
library(dplyr)
library(readr)
library(tidyverse)
#library(readxl)
library(lubridate)
# ---------

# set default definition
# -----------------------
default_definition <- 3
# -----------------------

# source functions: insta
source("utility_functions.R")
source("default_functions.R")

# import data 
# -----------
# read_csv from readr saves the file already as a tibble. 
# i.e. the result of a as_data_frame(). 

# will add the other year files once initial coding and testing is done. 

# read all year files into one dataframe (df)
dataset_start_year <- 2012
dataset_end_year <- 2016
dataset_range <- c(dataset_start_year:dataset_end_year)

df <- NULL

for (val in dataset_range) {
  
  origfile <- read_delim(paste0("data/sample_orig_", val, ".txt"), delim = "|", col_names = FALSE)
  names(origfile)=c('fico','dt_first_pi','flag_fthb','dt_matr','cd_msa',"mi_pct",'cnt_units','occpy_sts','cltv' ,'dti','orig_upb','ltv','int_rt','channel','ppmt_pnlty','prod_type','st', 'prop_type','zipcode','id_loan','loan_purpose', 'orig_loan_term','cnt_borr','seller_name','servicer_name', 'flag_sc')
  
  svcgfile <- read_delim(paste0("data/sample_svcg_", val, ".txt"), delim = "|", col_names = FALSE)
  names(svcgfile)=c('id_loan','svcg_cycle','current_upb','delq_sts','loan_age','mths_remng', 'repch_flag','flag_mod', 'cd_zero_bal', 'dt_zero_bal','current_int_rt','non_int_brng_upb','dt_lst_pi','mi_recoveries', 'net_sale_proceeds','non_mi_recoveries','expenses', 'legal_costs', 'maint_pres_costs','taxes_ins_costs','misc_costs','actual_loss', 'modcost')
  
  # join sets 
  df_inner <- inner_join(x = origfile, y = svcgfile, by = NULL)
  
  df = rbind(df, df_inner)
}
# sample: 2012 to 2016 = 6 990 979 obs

rm(df_inner, origfile, svcgfile)

# single year files read
# origfile_2012 <- read_delim("data/sample_orig_2012.txt", delim = "|", col_names = FALSE)
# names(origfile_2012)=c('fico','dt_first_pi','flag_fthb','dt_matr','cd_msa',"mi_pct",'cnt_units','occpy_sts','cltv' ,'dti','orig_upb','ltv','int_rt','channel','ppmt_pnlty','prod_type','st', 'prop_type','zipcode','id_loan','loan_purpose', 'orig_loan_term','cnt_borr','seller_name','servicer_name', 'flag_sc')
# 
# svcgfile_2012 <- read_delim("data/sample_svcg_2012.txt", delim = "|", col_names = FALSE)
# names(svcgfile_2012)=c('id_loan','svcg_cycle','current_upb','delq_sts','loan_age','mths_remng', 'repch_flag','flag_mod', 'cd_zero_bal', 'dt_zero_bal','current_int_rt','non_int_brng_upb','dt_lst_pi','mi_recoveries', 'net_sale_proceeds','non_mi_recoveries','expenses', 'legal_costs', 'maint_pres_costs','taxes_ins_costs','misc_costs','actual_loss', 'modcost')
# 
# # join sets 
# df <- inner_join(x = origfile_2012, y = svcgfile_2012, by = NULL)
# -----------



# bind new data to old
# df <- bind_rows(df, df1, df2)

# data wrangling
# --------------
# sort correct for easy viewing:
#df <- arrange(df, ...)

# delete unnecessary variables / select necessary ones:
df <- df %>% select(dt_first_pi, id_loan, channel, orig_loan_term
                    , svcg_cycle, current_upb, delq_sts, loan_age, 
                    fico, orig_upb, occpy_sts, cltv, dti)

# set names of remaining variables to lower case
# (just helps because you don't have to rename some of them)
# names(df) <- tolower(names(df))

# rename variables, colnames(df),  to correct internal naming convention. 
# client segments do not need to be renamed as they are always different.

df <- rename(df, 
             contract_key = id_loan, 
             pointintime_month = svcg_cycle, 
             #product_name = prod_type,
             term = orig_loan_term,
             loan_amount = orig_upb,
             #orig_date = ,
             #orig_month = ,
             #loan_period = ,
             fpd_month = dt_first_pi, 
             fpd_period = loan_age,
             #default_month = ,
             #months_in_default = ,
             #exposure_at_default = ,
             #payment = ,
             months_arrears = delq_sts,
             #instalment  = ,
             closing_balance = current_upb
)

# create/convert variabels that are needed
# convert to date format:
df <- df %>%
  mutate(pointintime_month = trans_int_month_date(pointintime_month)) %>%
  mutate(fpd_month = trans_int_month_date(fpd_month))

# add default flag:
df <- add_def_month_flag(data = df, default_definition = default_definition)

# create orig_month and loan_period from fdp. 
# these data dimensions are missing from the data set
# this is not a accurate transformation, but just
# a hack to have them in the set. 
df <- df %>%
  mutate(orig_month = fpd_month) %>%
  mutate(loan_period = fpd_period) 


# contract_key: make sure that the contract key is unique, if not create a
# unique version

# re-arrange to get the right order for easy viewing of contracts. 
df <- arrange(df, contract_key, pointintime_month)

# delete any unnecessary rows
# df <- df %>% filter(cancelled != "Yes")
# --------------


# binning of continous variables
# --------------
# determine 33th and 66th percentile thresholds for variables fico, 
fico_perc <- quantile(df$fico, c(.33, .66), na.rm = TRUE)
dti_perc <- quantile(as.numeric(df$dti), c(.33, .66), na.rm = TRUE)
cltv_perc <- quantile(df$cltv, c(.33, .66), na.rm = TRUE)

dft <- df %>%
  mutate(fico_bin = ifelse(fico < fico_perc[[1]], paste("<", fico_perc[[1]]),
                           ifelse(fico > fico_perc[[2]], paste(">", fico_perc[[2]]), paste(fico_perc[[1]], "-", fico_perc[[2]])))) %>%
  mutate(dti_bin = ifelse(dti < dti_perc[[1]], paste("<", dti_perc[[1]]),
                           ifelse(dti > dti_perc[[2]], paste(">", dti_perc[[2]]), paste(dti_perc[[1]], "-", dti_perc[[2]])))) %>%
  mutate(cltv_bin = ifelse(cltv < cltv_perc[[1]], paste("<", cltv_perc[[1]]),
                           ifelse(cltv > cltv_perc[[2]], paste(">", cltv_perc[[2]]), paste(cltv_perc[[1]], "-", cltv_perc[[2]]))))


# fix any obvious data errors. this should always be well documented. 


# check that all data types match the data dictionary



# If there are any variables that need to be created from what is there, that
# will be dealt with in seperate scripts.




# export data 
# -----------
write_rds(x = df, "df.rds", compress = "gz")











