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


#' @title get_contracts_by_stop_month
#' @description creates a data frame of contracts with their respective last month reported
#' @param data a data frame that contains at least \code{contract_key} and \code{pointintime_month}
#'
#' @return a data frame with \code{contract_key} and \code{max_pointintime_month}, which
#'         is the last \code{pointintime_month} reported for each one
#' @export
#'
#' @examples
#' by_stop_month <- get_contracts_by_stop_month(df)
#' 
get_contracts_by_stop_month <- function(data) {
  by_stop_month <-
    data %>% 
    group_by(contract_key) %>% 
    summarize(max_pointintime_month = max(pointintime_month)) %>% 
    ungroup()
  
  return(by_stop_month)
}


#' @title get_continuity_issues
#' @description get the list and proportion of contracts with incomplete performance 
#'              history considering the last available month in the dataset.
#' @param data a data frame that contains at least \code{contract_key} and \code{pointintime_month}
#'
#' @return list with two elements:
#'         - \code{contract_key}: list of contracts with continuity issues
#'         - \code{proportion_stopped_contracts}: proportion (percentage in decimals) of 
#'         contracts with continuity issues
#' @export
#'
#' @examples
#' continuity_issues <- get_continuity_issues(df)
#' continuity_issues$contract_key
#' continuity_issues$proportion_stopped_contracts
#' 
get_continuity_issues <- function(data) {
  by_stop_month <- get_contracts_by_stop_month(data)

  last_stop_month <- max(by_stop_month$max_pointintime_month)
  
  # Note: pull() is available on dplyr since v0.7.0
  continuity_issues <- 
    by_stop_month %>% 
    filter(max_pointintime_month < last_stop_month) %>% 
    pull(contract_key) %>% 
    list("contract_key" = .)
  
  n_stopped_contracts <- length(continuity_issues$contract_key)
  n_total_contracts <- nrow(by_stop_month)
  
  continuity_issues$proportion_stopped_contracts <- n_stopped_contracts / n_total_contracts
  
  return(continuity_issues)
}


#' @title has_continuity_issues
#' @description analyze if the data has incomplete performance history considering 
#'              the last available month per contract.
#' @param data a data frame that contains at least \code{contract_key} and \code{pointintime_month}
#'
#' @return \code{TRUE} if at least one contract has continuity issues, \code{FALSE} otherwise.
#' @export
#'
#' @examples
#' if (has_continuity_issues(df)) {
#'   message("At least one contract in the data frame has incomplete history. Please check.")
#' }
#' 
has_continuity_issues <- function(data) {
  by_stop_month <- get_contracts_by_stop_month(data)

  last_stop_month <- max(by_stop_month$max_pointintime_month)
  first_stop_month <- min(by_stop_month$max_pointintime_month)
  
  return(last_stop_month > first_stop_month)
}


#' @title complete_history
#' @description fill the missing rows in contracts with continuity issues up to the 
#'              last \code{pointintime_month} in the data frame.
#' @param data a data frame that contains at least \code{contract_key}, \code{pointintime_month},
#'             \code{fpd_period} and \code{loan_period}
#'
#' @return the input data frame with extra rows inserted to solve continuity issues
#' @export
#'
#' @examples
#' df_complete <- complete_history(df)
#' 
complete_history <- function(data) {
  last_stop_month <- max(data$pointintime_month)
  
  # Used on workaround for missing values in fico and cltv columns (issue #12)
  UNKNOWN_STRING <- "unknown"

  # Dependency: Package padr v0.4.0
  data_complete <-
    data %>% 
    # Workaround for missing values in fico and cltv columns (issue #12)
    replace_na(list(fico = UNKNOWN_STRING, 
                    fico_bin = UNKNOWN_STRING,
                    cltv = UNKNOWN_STRING,
                    cltv_bin = UNKNOWN_STRING)) %>% 
    thicken(by = "pointintime_month", 
            interval = "month") %>% 
    pad(by = "pointintime_month_month",
        group = "contract_key",
        interval = "month",
        end_val = last_stop_month,
        break_above = 2) %>%
    tidyr::fill(-c(pointintime_month, fpd_period, loan_period)) %>% 
    mutate(pointintime_month = if_else(is.na(pointintime_month),
                                       last_day(pointintime_month_month),
                                       pointintime_month)) %>% 
    group_by(group_cum_sum = cumsum(!is.na(fpd_period)), contract_key) %>% 
    # Avoiding if_else() here because strict type checking is raising issues
    # see: https://github.com/tidyverse/dplyr/issues/2365
    mutate(fpd_period = if (n() > 1) as.integer(fpd_period[1L] + row_number() - 1) else fpd_period) %>%
    ungroup %>% 
    mutate(loan_period = if_else(is.na(loan_period),
                                 as.integer(fpd_period),
                                 as.integer(loan_period))) %>% 
    select(-c(pointintime_month_month, group_cum_sum)) %>% 
    # Workaround for missing values in fico and cltv columns (issue #12)
    mutate(fico = replace(fico, which(fico == UNKNOWN_STRING), NA),
           fico_bin = replace(fico_bin, which(fico_bin == UNKNOWN_STRING), NA),
           cltv = replace(cltv, which(cltv == UNKNOWN_STRING), NA),
           cltv_bin = replace(cltv_bin, which(cltv_bin == UNKNOWN_STRING), NA))

  return(data_complete)
}

