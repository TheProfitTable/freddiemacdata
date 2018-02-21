# =============================================================================================================
# Project:        Freddie Mac Data 
# Discription:    utility_functions.R 
# Purpose: functions for getting freddie mac data into shape for use by loanportr. To be
# moved to a relevant package at a later date. 
# Authors:        N van Heerden, DJ de Villiers 
# Date:           6 Oct 2017 
# =============================================================================================================


# Constants used in functions
# =======================================

# Used on workaround for missing values columns (issue #12)
UNKNOWN_STRING <- "unknown"
UNKNOWN_INT <- -1
UNKNOWN_DATE <- as.Date("2222-12-31")


# Functions
# =======================================

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
  
  # Replace NA with default values
  data_complete <- impute_na(data)

  # Dependency: Package padr v0.4.0
  data_complete <-
    data_complete %>% 
    thicken(by = "pointintime_month", 
            interval = "month") %>% 
    pad(by = "pointintime_month_month",
        group = "contract_key",
        interval = "month",
        end_val = last_stop_month,
        break_above = 10) %>%
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
    select(-c(pointintime_month_month, group_cum_sum))
  
  # Replace default values with NA
  data_complete <- reset_na(data_complete)

  return(data_complete)
}


#' @title impute_na
#' @description Impute NA in some columns of the performance history data with a default value
#' @param data a data frame that contains the columns to impute:
#'             - \code{months_arrears}
#'             - \code{fico}
#'             - \code{cltv}
#'             - \code{dti}
#'             - \code{default_flag}
#'             - \code{default_month}
#'             - \code{fico_bin}
#'             - \code{cltv_bin}
#'             - \code{dti_bin}
#'
#' @return the input data frame with default values instead of NA in the affected columns
#' @export
#'
#' @examples
#' df_imputed <- impute_na(df)
#' 
impute_na <- function(data) {
  # Column details:
  # --------------
  # Name           - Type - Remarks
  # months_arrears - int  - value can include 0
  # fico           - int  - value doesn't include 0
  # cltv           - int  - value doesn't include 0
  # dti            - int  - value doesn't include 0
  # default_flag   - num  - value can include 0
  # default_month  - Date - usually "2222-01-01" (can be a valid date in the past)
  # fico_bin       - chr  - bin expressed as a string
  # cltv_bin       - chr  - bin expressed as a string
  # dti_bin        - chr  - bin expressed as a string
  
  # Workaround for missing values (issue #12)
  # TODO(floresfdev): Generalize to unobserved cases
  data_processed <- 
    data %>% 
    replace_na(list(months_arrears = as.integer(UNKNOWN_INT),
                    fico = as.integer(UNKNOWN_INT), 
                    cltv = as.integer(UNKNOWN_INT),
                    dti = as.integer(UNKNOWN_INT),
                    default_flag = as.numeric(UNKNOWN_INT),
                    default_month = as.Date(UNKNOWN_DATE),
                    fico_bin = UNKNOWN_STRING,
                    cltv_bin = UNKNOWN_STRING,
                    dti_bin = UNKNOWN_STRING))
  
  return(data_processed)
}


#' @title reset_na
#' @description Reinstate NA from a default value in some columns of the performance history data
#' @param data a data frame that contains the imputed columns:
#'             - \code{months_arrears}
#'             - \code{fico}
#'             - \code{cltv}
#'             - \code{dti}
#'             - \code{default_flag}
#'             - \code{default_month}
#'             - \code{fico_bin}
#'             - \code{cltv_bin}
#'             - \code{dti_bin}
#'
#' @return the input data frame with NA instead of the default values in the affected columns
#' @export
#'
#' @examples
#' df_na_reset <- reset_na(df_imputed)
#' 
reset_na <- function(data) {
  # Workaround for missing values (issue #12)
  # TODO(floresfdev): Generalize to unobserved values
  data_processed <- 
    data %>% 
    mutate(months_arrears = replace(months_arrears, which(months_arrears == UNKNOWN_INT), NA),
           fico = replace(fico, which(fico == UNKNOWN_INT), NA),
           cltv = replace(cltv, which(cltv == UNKNOWN_INT), NA),
           dti = replace(dti, which(dti == UNKNOWN_INT), NA),
           default_flag = replace(default_flag, which(default_flag == UNKNOWN_INT), NA),
           default_month = replace(default_month, which(default_month == UNKNOWN_DATE), NA),
           fico_bin = replace(fico_bin, which(fico_bin == UNKNOWN_STRING), NA),
           cltv_bin = replace(cltv_bin, which(cltv_bin == UNKNOWN_STRING), NA),
           dti_bin = replace(dti_bin, which(dti_bin == UNKNOWN_STRING), NA))
  
  return(data_processed)
}

