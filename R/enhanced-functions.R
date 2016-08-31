### Enhanced versions of base R functions

#' Not In
#'
#' This function returns the complement of %in% or match()
#' @export
#' @examples
'%!in%' <- function(x, y) {
  !('%in%'(x,y))
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
