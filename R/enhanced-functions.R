### Enhanced versions of base R functions

#' Not In
#'
#' This function returns the complement of \code{\%in\%} or \code{match()}.
#' @param x left-hand side
#' @param y right-hand side
#' @export
#' @examples
#' # Compare !(%in%) to %nin%
#' !(1:3 %in% 2:4)
#' 1:3 %nin% 2:4
'%nin%' <- function(x, y) {
  !('%in%'(x, y))
}


#' Load Packages
#'
#' This function loads
#' @param input Libraries to be loaded
#' @export
#' #@examples
#' Libraries(MASS)
#'
#' # Load two libraries at once
#'
#' # Load libraries from a list
Libraries <- function(input) {
  stop(paste("To be implemented"))
}
