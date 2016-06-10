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
  # Return an error if old and new are different length
  if (length(old) != length(new)){
    stop(paste("Header lengths unequal: old =", length(old), "new =", length(new)))
  }
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
#' @param method Which lubridate method to use for parsing; defaults to ymd
#' @param debug If TRUE, print debug output
#' @export
#' @examples
#' ToDate(c("3-21-1911", "8-8-2008"), method="mdy")
#'
#' my.dates <- data.frame(x1 = c("1911-03-21", "2008-08-08"),
#'                        x2 = c("2009-01-01", "3001-01-01"),
#'                        x3 = c("1805-12-31", "1555-06-12"),
#'                        stringsAsFactors = FALSE)
#' # All three columns are chr
#' str(my.dates)
#'
#' my.dates2 <- ToDate(my.dates, fields=c("x1", "x3"))
#' # The first and third column are now Date, but the second is still chr
#' str(my.dates2)
#'
#' my.dates3 <- ToDate(my.dates, fields=1:3)
#' # All three columns are now Date
#' str(my.dates3)
ToDate <- function(data, fields=NA, method="ymd", debug=FALSE) {
  # Check for a valid method
  valid.methods <- c("dmy", "myd", "ymd", "ydm", "dym", "mdy", "ymd_hms")
  if (!(method %in% valid.methods)) {
    stop(
      paste("Invalid method, try one of: "),
      paste(valid.methods, collapse=" ")
    )
  }
  # If fields were not passed, use alternate logic
  alt.branch <- is.na(fields[1]) & length(fields) == 1
  # If fields were passed, loop through them
  for (i in fields) {
    # Evaluate using the provided method; construct command to be evaluated
    if (alt.branch) {
      tmp.dat <- "data"
    } else {
      tmp.dat <- parse(text = paste("c(unlist(data[, i]))", sep=""))
    }
    cmd <- parse(text = paste("lubridate::", method, "(", tmp.dat, ")", sep=""))
    # Debug output
    if (debug) {print(paste("Evaluating:", cmd, "@ i =", i))}
    # If no fields, return the entire vector
    if (alt.branch) {
      return(eval(cmd))
    } else {
      # Otherwise continue looping
      try(
        data[, i] <- eval(cmd),
        silent = debug
      )
    }
  }
  return(data)
}