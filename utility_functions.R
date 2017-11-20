# =============================================================================================================
# Project:        Freddie Mac Data 
# Discription:    utility_functions.R 
# Purpose: functions for getting freddie mac data into shape for use by loanportr. To be
# moved to a relevant package at a later date. 
# Authors:        N van Heerden, DJ de Villiers 
# Date:           6 Oct 2017 
# =============================================================================================================

#' @title last_day
#' @description calculate last day of the month given a date.  
#' @param date 
#'
#' @return last day of the month
#' @export
#'
#' @examples
#' last_day(date = as_date("2017-01-05"))
#' 
last_day <- function(date) {
  ceiling_date(date, "month") - days(1)
}


#' @title trans_int_month_date
#'
#' @param date_var 
#'
#' @return date variable in end of month format
#' @export
#'
#' @examples
#' trans_int_month_date(201601)
#' df <- df %>% mutate(pointintime_month = trans_int_month_date(pointintime_month))
trans_int_month_date <- function(date_var) {
  last_day(ymd(paste0(date_var,11)))
}


#' @title get_list_trimmed_contracts
#' @description get the list of contracts with incomplete performance history 
#'              considering the last available month in the dataset
#' @param data a data frame that contains at least contract_key and pointintime_month
#'
#' @return named list of contract_key with continuity issues
#' @export
#'
#' @examples
#' contract_list <- get_list_trimmed_contracts(df)
#' contract_list$contract_key
get_list_trimmed_contracts <- function(data) {
  ## Note: pull() is available on dplyr since v0.7.0
  contract_list <- 
    get_df_trimmed_contracts(data) %>% 
    pull(contract_key) %>% 
    list("contract_key" = .)
  
  return(contract_list)
}


#' @title get_contracts_by_last_month
#' @description creates a data frame of contracts with their respective last month reported
#' @param data a data frame that contains at least contract_key and pointintime_month
#'
#' @return a data frame with contract_key and the last pointintime_month reported for each one
#' @export
#'
#' @examples
#' by_last_month <- get_contracts_by_last_month(df)
get_contracts_by_last_month <- function(data) {
  by_last_month <-
    data %>% 
    group_by(contract_key) %>% 
    summarize(max_pointintime_month = max(pointintime_month)) %>% 
    ungroup()
  
  return(by_last_month)
}


#' @title get_df_trimmed_contracts
#' @description get a data frame of contracts with incomplete performance history 
#'              considering the last available month in the dataset
#' @param data a data frame that contains at least contract_key and pointintime_month
#'
#' @return data frame of contract_key and max_pointintime_month with continuity issues
#' @export
#'
#' @examples
#' contract_df <- get_df_trimmed_contracts(df)
get_df_trimmed_contracts <- function(data) {
  by_last_month <- get_contracts_by_last_month(data)
  
  last_month_in_df <- max(by_last_month$max_pointintime_month)
  
  contract_df <- 
    by_last_month %>% 
    filter(max_pointintime_month < last_month_in_df)
  
  return(contract_df)
}


#' @title get_prop_trimmed_contracts
#' @description get the proportion of contracts with incomplete performance history 
#'              considering the last available month in the dataset
#' @param data a data frame that contains at least contract_key and pointintime_month
#'
#' @return proportion (percentage in decimals) of contracts with continuity issues
#' @export
#'
#' @examples
#' get_prop_trimmed_contracts(df) 
get_prop_trimmed_contracts <- function(data) {
  # TODO(floresfdev): 
  # Proof of Concept. To be defined how to proceed with this computation
  # Ongoing discussion on GH issue #8
  n_total_contracts <- nrow(get_contracts_by_last_month(data))
  n_trimmed_contracts <- nrow(get_df_trimmed_contracts(data))
  
  return(n_trimmed_contracts / n_total_contracts)
}


