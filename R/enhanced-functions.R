### Enhanced versions of base R functions

#' Not In
#'
#' This function returns the complement of %in% or match()
#' @param x left-hand side
#' @param y right-hand side
#' @export
'%nin%' <- function(x, y) {
  !('%in%'(x, y))
}


#' Load Packages
#'
#' This function loads
#' @param input Libraries to be loaded
#' @export
#' @examples
#' # Load a single library
#' Libraries(MASS)
#'
#' # Load two libraries at once
#'
#' # Load libraries from a list
Libraries <- function(input) {
  stop(paste("To be implemented"))
}
