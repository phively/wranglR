### Functions to take and format text

#' Rename Headers
#'
#' This function renames a set of data frame headers
#' @param dframe Data frame to be manipulated
#' @param old Character vector of old header names
#' @param new Character vector of new header names, same length as old
#' @param debug If TRUE, print debug output
#' @export
#' @examples
#' iris <- RenameHeaders(iris, old=c("Sepal.Length", "Sepal.Width"),
#'                       new=c("Sep.L", "Sep.W"))
RenameHeaders <- function(dframe, old, new, debug=FALSE) {
  # Iterate through each provided header
  for (i in 1:max(length(old), 1)) {
    try(
      colnames(dframe)[which(colnames(dframe) == old[i])] <- new[i],
      silent = debug
    )
  }
  return(dframe)
}

#' To Date
#'
#' This function converts a set of character strings to date format
#' using the specified lubridate method
#' @param data Data frame or character string or vector to be manipulated
#' @param fields Character vector of fields; not necessary if passing a vector
#' @param method Which lubridate method to use for parsing
#' @param debug If TRUE, print debug output
#' @export
#' @examples
#' ToDate(c("3-21-1911", "8-8-2008"))
#'
#' my.dates <- data.frame(x1 = c("1911-03-21", "2008-08-08"),
#'                        x2 = c("2009-01-01", "3001-01-01"),
#'                        x3 = c("1805-12-31", "1555-06-12"),
#'                        stringsAsFactors = FALSE)
#' # All three columns are chr
#' str(my.dates)
#'
#' my.dates2 <- ToDate(my.dates, fields=c("x1", "x3"), method="ymd")
#' # The first and third column are now Date, but the second is still chr
#' str(my.dates2)
#'
#' my.dates3 <- ToDate(my.dates, fields=(1:3), method="ymd")
#' # All three columns are now Date
#' str(my.dates3)
ToDate <- function(data, fields=NA, method="mdy", debug=FALSE) {
  # Check for a valid method
  if (!(method %in% c("dmy", "myd", "ymd", "ydm", "dym", "mdy", "ymd_hms"))) {
    stop("Invalid method; try dmy, myd, ymd, ydm, dym, mdy, ymd_hms")
  }
  # Check for if fields were passed or not
  if (is.na(fields[1])) {
    return(
      eval(parse(text = paste(method, "(data)", sep = "")))
    )
  }
  # If fields were passed, loop through them
  for (i in fields) {
    # Evaluate using the provided method
    try(
      {
        # Grab current field
        tmp.dat <- c(unlist(data[, i]))
        data[, i] <- eval(parse(text = paste(method, "(tmp.dat)", sep = "")))
      },
      silent = debug
    )
  }
  return(data)
}
