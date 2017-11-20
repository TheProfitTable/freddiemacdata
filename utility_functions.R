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


#' @title get_trimmed_contracts
#' @description get the list of contracts with incomplete performance history considering the last available 
#'              month in the dataset
#' @param data a data frame that contains at least contract_key and pointintime_month
#'
#' @return named list of contract_key with continuity issues
#' @export
#'
#' @examples
#' contract_list <- get_trimmed_contracts(df)
#' contract_list$contract_key
get_trimmed_contracts <- function(data) {
  by_last_month <-
    data %>% 
    group_by(contract_key) %>% 
    summarize(max_pointintime_month = max(pointintime_month)) %>% 
    ungroup()
  
  last_month_in_df <- max(by_last_month$max_pointintime_month)
  
  ## Note: pull() is available on dplyr since v0.7.0
  contract_list <- 
    by_last_month %>% 
    filter(max_pointintime_month < last_month_in_df) %>% 
    pull(contract_key) %>% 
    list("contract_key" = .)
  
  return(contract_list)
}


