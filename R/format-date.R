### Functions to take and format dates

#' Parse Fiscal Dates
#'
#' This function takes a vector of dates and returns either the fiscal year, month,
#' or week, or months, weeks, or days since the start of the fiscal year.
#' @param date Date to be parsed
#' @param start.month Starting month number of the fiscal year; defaults to 7 (July)
#' @param output Output format, including \code{year} for fiscal year number or \code{days}
#' for days passed since the beginning of the fiscal year
#' @export
ToFiscalDate <- function(date, start.month=7, output="year") {
  # Valid output: year, month, week, months, weeks, days
  return(NA)
}

#' Collapse Dates
#'
#' This function takes a vector of dates and rolls them up to the specified time
#' unit, setting all smaller increments to 0 or 1 as applicable.
#' @param date Date to be parsed
#' @param by Level at which to aggregate, such as \code{hour} or \code{day}; defaults
#' to \code{month}
#' @param each Number of \code{by} periods to combine
CollapseDates <- function(date, by="month", each=1) {
  # Valid by: year, month, day, hour, minute
  # each: should make sense given the time unit (binning)
  return(NA)
}
