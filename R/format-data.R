### Functions to format data elements including dataframes

#' Replace Values
#'
#' This function is a wrapper for \code{replace()} that takes a vector or data frame
#' and replaces all instances matching a specified value or condition with a new value.
#' @param data Data frame or character vector to be manipulated
#' @param old.val Current value to be found; defaults to \code{NA}
#' @param new.val New value to take the place of found values; defaults to \code{0}
#' @param condition Whether or not to parse the input as a condition; defaults to
#' \code{FALSE}
#' @export
#' @examples
#'
#' # Replace all NA with 0 (default behavior)
#' (dat <- c(-3:3, NA, NaN))
#' ReplaceValues(dat)
#'
#' # Replace negative values with -Inf
#' ReplaceValues(dat, "<0", -Inf, condition=TRUE)
#'
#' # Replace a with NA
#' set.seed(123)
#' (dat <- sample(letters[1:5], size=20, replace=TRUE))
#' ReplaceValues(dat, "a", NA)
#'
#' # Replace a and e with "vowel"
#' ReplaceValues(dat, "%in% c('a', 'e')", "vowel", condition=TRUE)
ReplaceValues <- function(data, old.val=NA, new.val=0, condition=FALSE) {
  ## Error checking
  {
    # Check that old.val is length 1
    if (length(old.val) != 1) {
      stop(
        paste("old.val is length ", length(old.val), ", must be length 1", sep="")
      )
    }
    # Check that new.val is length 1
    if (length(new.val) != 1) {
      stop(
        paste("new.val is length ", length(new.val), ", must be length 1", sep="")
      )
    }
  }
  # Replace Values core
  if (is.na(old.val)) {
    # Special case for old.val=NA
    return(
      replace(data, list=is.na(data), values=new.val)
    )
  } else if (condition == FALSE) {
    # Default case
    idx <- which(data == old.val)
    return(
      replace(data, list=idx, values=new.val)
    )
  } else {
    # Parse old.val as a condition
    cmd <- parse(text=paste("data", old.val))
    return(
      replace(data, list=eval(cmd), values=new.val)
    )
  }
}
