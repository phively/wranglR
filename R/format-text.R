### Functions to take and format text

#' Rename Headers
#'
#' This function renames a set of data frame headers
#' @param dframe Data frame to be manipulated
#' @param old Character vector of old header names
#' @param new Character vector of new header names, same length as \code{old}
#' @param debug If \code{TRUE}, print debug output
#' @export
#' @examples
#' head(iris)
#' iris <- RenameHeaders(iris, old=c("Sepal.Length", "Sepal.Width"),
#'                       new=c("Sep.L", "Sep.W"))
#' head(iris)
RenameHeaders <- function(dframe, old, new, debug=FALSE) {
  # Return an error if old and new are different length
  if (length(old) != length(new)) {
    stop(paste("Header lengths unequal: old =", length(old), "new =", length(new)))
  }
  # Iterate through each provided header
  for (i in 1:max(length(old), 1)) {
    try(
      colnames(dframe)[which(colnames(dframe) == old[i])] <- new[i],
      silent = !debug
    )
  }
  return(dframe)
}

#' String To Date
#'
#' This function converts a set of character strings to date format
#' using the specified lubridate method
#' @param data Data frame or character string or vector to be manipulated
#' @param fields Character vector of fields; not necessary if passing a vector
#' @param method Which lubridate method to use for parsing; defaults to \code{ymd}
#' @param debug If \code{TRUE}, print debug output
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
  valid.methods <- c("dmy", "myd", "ymd", "ydm", "dym", "mdy", "ymd_h", "ymd_hm", "ymd_hms")
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
      tmp.dat <- "c(unlist(data[, i]))"
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
        silent = !debug
      )
    }
  }
  return(data)
}


#' Invalid date string parsing
#'
#' This function is a wrapper for ToDate that takes a character string of integers of length
#' 4 to 8 and attempts to parse out a valid date.
#' @param datestring Numeric string of length 1-8 to interpret
#' @param method Position of year, month, day values within the string. Currently ONLY \code{ymd} is supported!
#' @param fallback_mo Month number to insert, if necessary
#' @param fallback_day Day number to insert, if necessary
#' @param debug If \code{TRUE}, print debug output
#' @export
#' @examples
#' weird_dates <- c('20200101', '2020', '20201', '202001', '20200000'
#' , '2020001', '20200001', '20200199')
#'
#' # ToDate returns errors
#' # But ToDateCleaner attempts to fix the invalid date strings
#' ToDateCleaner(weird_dates)
ToDateCleaner <- function(datestring, method = 'ymd', fallback_mo = 1, fallback_day = 1, debug = FALSE) {
  # Check that fallback_mo and fallback_day are valid
  if (fallback_mo %nin% 1:12) {
    stop('Invalid fallback month, please use 1-12')
  } else if (fallback_day %nin% 1:31) {
    stop('Invalid fallback day, please use 1-31')
  }
  # Clean up inputs
  datestring <- trimws(datestring)
  fallback_mo <- stringr::str_pad(fallback_mo, 2, pad = '0')
  fallback_day <- stringr::str_pad(fallback_day, 2, pad = '0')
  # Debug output
  if (debug) {
    cat(paste('Fallback month/day', fallback_mo, fallback_day, '\n'))
  }
  # Attempt using the ToDate function with the existing string or vector
  out <- suppressWarnings(datestring %>% ToDate(method = method))
  if (debug) {
    cat('First pass:', '\n')
    print(out)
  }
  # If it didn't work then use fallback_mo and/or fallback_day to generate a valid date string
  for (i in 1:length(out)) {
    # Computations for checks
    is_dt <- !is.na(out[i])
    ds <- datestring[i]
    if (debug) {
      cat(paste('===== Checking entry', i, ':', ds, out[i], 'is_dt', is_dt, '=====', '\n'))
    }
    if (is_dt) {
      next
    }
    # Substrings of datestring (needs to be changed to support methods besides ymd)
    ds_len <- stringr::str_length(ds)
    a <- ds %>% substr(1, 4) %>% stringr::str_pad(4, pad = '0')
    b <- ds %>% substr(5, 6) %>% stringr::str_pad(2, pad = '0')
    c <- ds %>% substr(7, 8) %>% stringr::str_pad(2, pad = '0')
    if (debug) {
      print(paste(ds, 'length', ds_len, a, b, c))
    }
    # If length 4 insert fallback_mo and fallback_day
    if (ds_len == 4) {
      out[i] <- paste0(a, fallback_mo, fallback_day) %>% ToDate(method = method)
      if (debug) {
        print(paste('len 4', a, out[i]))
      }
      # If length 5 or 6 assume the extra digits if nonzero are the month and add fallback_day
    } else if(ds_len %in% 5:6) {
      if (as.numeric(b) %in% 1:12) {
        out[i] <- paste0(a, b, fallback_day) %>% ToDate(method = method)
      } else {
        out[i] <- paste0(a, fallback_mo, fallback_day) %>% ToDate(method = method)
      }
      if (debug) {
        print(paste('len 5-6', a, b, out[i]))
      }
      # If length 7 or 8 check for 0 in the month or day and insert fallback strings as needed
    } else if (ds_len %in% 7:8) {
      if (as.numeric(b) == 0 & as.numeric(c) == 0) {
        out[i] <- paste0(a, fallback_mo, fallback_day) %>% ToDate(method = method)
      } else if(as.numeric(b) %in% 1:12) {
        out[i] <- paste0(a, b, fallback_day) %>% ToDate(method = method)
      } else if(as.numeric(c) %in% 1:31) {
        out[i] <- paste0(a, fallback_mo, c) %>% ToDate(method = method)
      }
      if (debug) {
        print(paste('len 7-8', a, b, c, out[i]))
      }
      # Fallback is just NA
    } else {
      out[i] <- NA
    }
  }
  return(out)
}

#' Currency String To Numeric
#'
#' This function takes a vector or data frame of currency values in character
#' format, strips out comma delimiters and currency symbols, and converts the
#' result to numeric.
#' @param data Data frame or character vector to be manipulated
#' @param fields Character vector of fields; not necessary if passing a vector
#' @param debug If \code{TRUE}, print debug output
#' @export
#' @examples
#' cash <- c("10,203¢", "$340", "€8,111.10", "£23,040", "¥8300")
#' CurrencyToNumeric(cash)
#'
#' cash.frame <- data.frame(c1 = cash, c2 = cash, c3 = cash, stringsAsFactors = FALSE)
#'
#' CurrencyToNumeric(cash.frame)
#' # Note that this unlists the data because no fields were included
#'
#' CurrencyToNumeric(cash.frame, fields=1:2)
#' CurrencyToNumeric(cash.frame, fields=c("c2", "c3"))
CurrencyToNumeric <- function(data, fields=NA, debug=FALSE) {
  # List of symbols to strip
  # Unicode 00A2 = ¢, 20AC = €, 00A3 = £, 00A5 = ¥
  sym <- c("\\\\$", "\u00A2", "\u20AC", "\u00A3", "\u00A5", ",")
  # If fields were not passed, use alternate logic
  alt.branch <- is.na(fields[1]) & length(fields) == 1
  for (i in fields) {
    # Construct command to be evaluated
    if (alt.branch) {
      tmp.dat <- "data"
    } else {
      tmp.dat <- "c(unlist(data[, i]))"
    }
    cmd <- parse(text =
      paste("mapply(gsub, '(", paste(sym, collapse="|"), ")', '', ", tmp.dat, ")", sep="")
    )
    if (debug) {print(paste("Evaluating:", cmd, "@ i =", i))}
    # If no fields, execute and return the vector
    if (alt.branch) {
      return(as.numeric(eval(cmd)))
    } else {
      # Otherwise loop
      try(
        data[, i] <- as.numeric(eval(cmd)),
        silent=!debug
      )
    }
  }
  return(data)
}

#' Extract Currency
#'
#' This function takes a text string and extracts the dollar amount identified as numeric,
#' allowing the suffixes K (thousand), M (million), B (billion), and T (trillion)
#' @param string String to search for dollar amounts
#' @param debug If \code{TRUE}, print debug output
#' @export
#' @examples
#' ExtractCurrency('$11')
#'
#' ExtractCurrency('$100 total')
#'
#' ExtractCurrency('Receipt of $11.57')
#'
#' ExtractCurrency('$10K-$100K', debug = TRUE) # Returns 10,000 (first match)
#'
#' strings <- c('Nearly $23K', 'A $200M Powerball', 'A $3B endowment', 'GDP last year of $17.1T (USD)')
#' ExtractCurrency(strings, debug = TRUE)
ExtractCurrency <- function(string, debug=FALSE) {
  # Look for first dollar amount
  match <- stringr::str_to_upper(string) %>%
    stringr::str_extract('\\$[0-9,KMBT\\.]*')
  # Look for suffixes and identify appropriate multiplier
  multiplier <- dplyr::case_when(
    stringr::str_detect(match, 'K') ~ 1E3
    , stringr::str_detect(match, 'M') ~ 1E6
    , stringr::str_detect(match, 'B') ~ 1E9
    , stringr::str_detect(match, 'T') ~ 1E12
    , TRUE ~ 1
  )
  # Strip out non-numeric characters
  clean <- stringr::str_replace_all(match, '[^0-9\\.]', '') %>% as.numeric()
  # Debug output
  if (debug) {
    paste0(
      '\n'
      , '-- Found ', match, ' in "', string, '"\n'
      , '    ', clean, ' extracted', '\n'
      , '    ', multiplier, ' multiplier', '\n'
    ) %>% cat()
  }
  # Return result
  return(clean * multiplier)
}
