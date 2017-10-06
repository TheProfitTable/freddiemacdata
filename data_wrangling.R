
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

# import data 
# -----------
# read_csv from readr saves the file already as a tibble. 
# i.e. the result of a as_data_frame(). 

# will add the other year files once initial coding and testing is done. 

origfile_2012 <- read_delim("data/sample_orig_2012.txt", delim = "|", col_names = FALSE)
names(origfile_2012)=c('fico','dt_first_pi','flag_fthb','dt_matr','cd_msa',"mi_pct",'cnt_units','occpy_sts','cltv' ,'dti','orig_upb','ltv','int_rt','channel','ppmt_pnlty','prod_type','st', 'prop_type','zipcode','id_loan','loan_purpose', 'orig_loan_term','cnt_borr','seller_name','servicer_name', 'flag_sc')

svcgfile_2012 <- read_delim("data/sample_svcg_2012.txt", delim = "|", col_names = FALSE)
names(svcgfile_2012)=c('id_loan','svcg_cycle','current_upb','delq_sts','loan_age','mths_remng', 'repch_flag','flag_mod', 'cd_zero_bal', 'dt_zero_bal','current_int_rt','non_int_brng_upb','dt_lst_pi','mi_recoveries', 'net_sale_proceeds','non_mi_recoveries','expenses', 'legal_costs', 'maint_pres_costs','taxes_ins_costs','misc_costs','actual_loss', 'modcost')

# join sets 
df <- inner_join(x = origfile_2012, y = svcgfile_2012, by = NULL)
# -----------



# bind new data to old
# df <- bind_rows(df, df1, df2)

# data wrangling
# --------------
# sort correct for easy viewing:
#df <- arrange(df, ...)

# delete unnecessary variables / select necessary ones:
df <- df %>% select(dt_first_pi, flag_fthb, id_loan, channel, orig_loan_term
                    , svcg_cycle, current_upb, delq_sts, loan_age, mi_recoveries, net_sale_proceeds, 
                    actual_loss, fico, prod_type)

# set names of remaining variables to lower case
# (just helps because you don't have to rename some of them)
# names(df) <- tolower(names(df))

# rename variables, colnames(df),  to correct internal naming convention. 
# client segments do not need to be renamed as they are always different.

df <- rename(df, 
             contract_key = id_loan, 
             pointintime_month = svcg_cycle, 
             product_name = prod_type,
             term = orig_loan_term,
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

# make sure all variables are of the right type: Danie, for this step you need
# to convert fpd_month and pointintime_,month to dates that are the last day of
# the month. can do in separate script. 
df <- df %>%
  mutate(orig_date = as_date(orig_date)) 

# create/convert variabels that are needed
# orig_month 

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
