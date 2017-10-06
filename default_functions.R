# =============================================================================================================
# Project:        Freddie Mac Data 
# Discription:    default_functions.R 
# Purpose: functions for creating default_month and default_flag for use by loanportr. To be
# moved to a relevant package at a later date. 
# Authors:        N van Heerden, DJ de Villiers 
# Date:           6 Oct 2017 
# =============================================================================================================

default_definition <- 3

# default_month
# =======================================

#' @title create_default_month
#' @description creates a data frame with contract_key and default_date. One obs per contract / loan.
#' @param data a data frame that contains at least contract_key, months_arrears and pointintime_month
#' @param default_definition 
#'
#' @return calculates the default_date and returns data frame with default_date and contract_key
#' @export
#' @note non defaulted loans are given a default date far into the future. makes calculating default_flag easier. 
#' @seealso add_def_month_flag
#' @examples
#' df_def_flag <- create_default_month(df, 3)
#' 
create_default_month <- function(data, default_definition) {
  df_def_flag <- data %>%
    group_by(contract_key) %>%
    mutate(lag_months_arrears = lag(months_arrears)) %>%
    mutate(default_month_all = if_else(lag_months_arrears < months_arrears & months_arrears == default_definition, 
                                      pointintime_month, as_date("2222-01-01"))) %>% # to change when new date format available
    summarise(default_month = min(default_month_all)) 
   # mutate(default_month = if_else(default_month == "2222-01-01", 0, default_month))
  return(df_def_flag)
}

# =======================================






# default_flag 
# =======================================

#' @title add_def_month_flag
#' @description adds default_flag and default_month to data frame. 
#' @param data a data frame that contains at least contract_key, months_arrears and pointintime_month
#' @param default_definition 
#'
#' @return the input data frame plus default_flag and default_month
#' @export
#' @seealso create_default_month()
#' @examples
#' df_test <- add_def_month_flag(df, 3)
#' 
add_def_month_flag <- function(data, default_definition) {
  df_def_flag <- create_default_month(df, 3)
  
  df_def_flag_join <- inner_join(x = df, y = df_def_flag, by = "contract_key") %>%
    mutate(default_flag = if_else(pointintime_month >= default_month, true = 1, false = 0))
  
  return(df_def_flag_join)
  
}

# good contract to check F112Q1018643

# =======================================












