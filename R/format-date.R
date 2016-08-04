### Functions to take and format dates

#' Fiscal Date Generation
#'
#' These functions take a vector of dates and return the fiscal year start or end dates.
#' @name GetFYDate
#' @param date Dates to be parsed
#' @param start.month Starting month number of the fiscal year; defaults to 7 (July)
NULL
# Start of fiscal year function
#' @rdname GetFYDate
#' @export
#' @examples
#' (dts <- ToDate(paste("2012", 1:12, 1, sep="-")))
#'
#' GetFYStart(dts)
#' GetFYEnd(dts)
#'
#' GetFYStart(dts, start.month = 1)
#' GetFYEnd(dts, start.month = 1)
#' # Properly handles calendar years (i.e. no conversion)
#'
#' GetFYEnd(dts, start.month = 3)
#' # Properly accounts for leap years
GetFYStart <- function(date, start.month=7) {
  # If start.month is January, use years as-is, otherwise modify
  if (start.month == 1) {
    yrs <- lubridate::year(date)
  } else {
    yrs <- lubridate::year(date) + ifelse(lubridate::month(date) < start.month, -1, 0)
  }
  # Return ymd dates
  return(
    lubridate::ymd(paste(yrs, start.month, 1, sep="-"))
  )
}
# End of fiscal year function
#' @rdname GetFYDate
#' @export
GetFYEnd <- function(date, start.month=7) {
  # Year is whatever the fiscal year number is
  yrs <- ToFiscalDate(date, start.month, output="year")
  # If January (1) use December (12), otherwise previous month
  if (start.month == 1) {
    mos <- 12
  } else {
    mos <- start.month - 1
  }
  # Use last day of target month/year
  days <- lubridate::days_in_month(
    lubridate::ymd(paste(yrs, mos, 1, sep="-"))
  )
  return(
    lubridate::ymd(paste(yrs, mos, days, sep="-"))
  )
}

#' Calendar To Fiscal Date Parsing
#'
#' This function takes a vector of dates and returns either the fiscal year, month,
#' or week, or months, weeks, or days since the start of the fiscal year.
#' @param date Dates to be parsed
#' @param start.month Starting month number of the fiscal year; defaults to 7 (July)
#' @param output Output format, including \code{year} for fiscal year number or \code{days}
#' for days passed since the beginning of the fiscal year
#' @export
#' @examples
#' (dts <- ToDate(paste("2012", 1:12, 1, sep="-")))
#'
#' ToFiscalDate(dts)
#' # Return fiscal year by default
#'
#' ToFiscalDate(dts, start.month=9, output="month")
#' # Return fiscal month number
#'
#' ToFiscalDate(dts, start.month=1, output="week")
#' ToFiscalDate(dts, start.month=1, output="weeks")
#' # 'week' returns week number while 'weeks' returns weeks elapsed
ToFiscalDate <- function(date, start.month=7, output="year") {
  # Check for usable start.month
  if (!(start.month %in% 1:12)) {
    stop(
      paste("Invalid start.month: use 1 to 12")
    )
  }
  # Check for valid output method
  valid.outputs <- c("year", "month", "week", "day", "weeks", "days")
  if (!(output %in% valid.outputs)) {
    stop(
      paste("Invalid output format, try one of: "),
      paste(valid.outputs, collapse=" ")
    )
  }
  # Parse each valid output format
  if (output == "year") {
    # Return fiscal year
    # If fiscal start is in January, return current year
    if (start.month == 1) {
      return(lubridate::year(date))
    }
    # Otherwise, add 1 to year when needed
    return(
      lubridate::year(date) + ifelse(lubridate::month(date) >= start.month, 1, 0)
    )
  } else if (output == "month") {
    # Return fiscal month
    return(
      (lubridate::month(date) - start.month) %% 12 + 1
    )
  }
  # Calculate FY start date, which is needed by the other output format methods
  fy.start.date <- GetFYStart(date, start.month)
  # Remaining output format methods
  # Return week or day number
  if (output %in% c("week", "day")) {
    return(
      floor(
        as.numeric(
          difftime(date, fy.start.date, units=output)
        )
      )
    )
  } else {
    # Return time period since start of fiscal year in chosen unit
    return(
      as.numeric(
        difftime(date, fy.start.date, units=output)
      )
    )
  }
  # Fallback if none of the above output methods were used
  stop(
    paste("Output format '", output, "' not found", sep="")
  )
}

#' Date Binning
#'
#' This function takes a vector of dates and rolls them up to the specified time
#' unit, setting all smaller increments to 0 or 1 as applicable.
#' @param date Date to be parsed
#' @param by Level at which to aggregate, such as \code{hour} or \code{day}; defaults
#' to \code{month}
#' @param each Number of \code{by} periods to combine
#' @export
BinDates <- function(date, by="month", each=1) {
  # Valid by: year, month, day, hour, minute, second
  # each: should make sense given the time unit (binning)
  return(NA)
}
