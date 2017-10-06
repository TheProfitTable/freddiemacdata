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



