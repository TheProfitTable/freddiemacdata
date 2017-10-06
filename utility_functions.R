# =============================================================================================================
# Project:        Freddie Mac Data 
# Discription:    utility_functions.R 
# Purpose: functions for getting freddie mac data into shape for use by loanportr. To be
# moved to a relevant package at a later date. 
# Authors:        N van Heerden, DJ de Villiers 
# Date:           6 Oct 2017 
# =============================================================================================================

#' last_day
#'
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
