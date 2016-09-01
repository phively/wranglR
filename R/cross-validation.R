### Functions to assist with cross-validation

#' K-fold Cross-Validation Samples
#'
#' This function takes a dataframe or vector and returns a list of indices for use in
#' cross-validation.
#' @param dat Data to split into cross-validation groups
#' @param k Constant k for k-fold cross-validation; defaults to 2
#' @param prop Proportion of data to include in the first group; if not provided, defaults
#' to equal-sized groups. Remaining groups must be at least size 1, and are as nearly
#' equally sized as possible, with any remainder included in the final group.
#' @param seed Optional random seed to use for sampling
#' @export
#' @examples
#' # Create some sample data
#' dat <- LETTERS
#'
#' # Create a function to print the groups
#' print.kfcv <- function(q) {
#'   for (i in 1:length(q)) {print(dat[q[[i]]])}
#' }
#'
#' # Default behavior is to create 2 equally-sized sample groups
#' q <- KFoldXVal(dat, seed=123)
#' print.kfcv(q)
#'
#' # For unequal groups, the remainder goes into the last group
#' q <- KFoldXVal(dat, k=4, seed=123)
#' print.kfcv(q)
#'
#' # prop is used to fix the size of the first group
#' q <- KFoldXVal(dat, prop=.75, seed=123)
#' print.kfcv(q)
#'
#' # This may be freely combined with k, provided that there are
#' # sufficient observations that all groups are at least size 1
#' q <- KFoldXVal(dat, k=4, prop=.75, seed=123)
#' print.kfcv(q)
#'
#' q <- KFoldXVal(dat, k=4, prop=.9, seed=123)
#' print.kfcv(q)
KFoldXVal <- function(dat, k=2, prop=NA, seed=NA) {
  # Required number of indices is length(vector) or nrow(dat)
  if (is.vector(dat)) {
    n <- length(dat)
  } else {
    n <- nrow(dat)
  }
  ## Error-checking
  {
    # Check that k is an integer >= 2
    if (k != round(k) | k < 2) {
      stop(
        paste("Invalid k, use an integer >= 2")
      )
    }
    # Check that k <= n
    if (k > n) {
      stop(
        paste("Invalid k, use k <= number of observations")
      )
    }
    # Check that prop is in the range (0, 1)
    if (!is.na(prop) && (0 >= prop | 1 <= prop)) {
      stop(
        paste("Invalid prop, must be a fraction in the range 0 < prop < 1")
      )
    }
    # Check that any present seed is numeric, and set it
    if (!is.na(seed)[1]) {
      if (!is.numeric(seed)) {
        stop(
          paste("Invalid seed, must be numeric")
        )
      } else {
        set.seed(seed)
      }
    }
  }
  # Generate random list of indices
  ind <- sample(1:n, size=n, replace=F)
  # Determine the number of observations to be included in the first cross-validation group
  if (!is.na(prop)){
    # If prop was provided, use it to set the size of the first group
    n.grp1 <- stats::quantile(1:n, probs=prop)
  } else {
    # Otherwise the groups are equally sized
    n.grp1 <- n/k
  }
  # Reduce n.grp1 if it doesn't leave enough for the other groups, decrease it and print a warning
  if (n - n.grp1 < k - 1) {
    prop.new <- (n - k + 1)/n
    n.grp1 <- stats::quantile(1:n, probs=prop.new)
    warning(
      paste("'prop = ", prop, "' resulted in size 0 groups; prop decreased to ", prop.new, sep="")
    )
  }
  ## Create k-element list of indices
  ## This method passes the "are the other permutations equally likely?" test
  xval <- list()
  # First group
  end <- n.grp1
  xval[[1]] <- ind[1:end]
  # Middle groups
  n.grp <- round((n - n.grp1)/(k-1))
  if (k > 2) {
    for (i in 2:(k-1)) {
      start <- floor(end + 1)
      end <- start + n.grp - 1
      xval[[i]] <- ind[start:end]
    }
  }
  # Last group
  start <- floor(end + 1)
  xval[[k]] <- ind[start:n]
  # Return final output
  return(xval)
}
