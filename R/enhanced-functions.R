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
#' This function attempts to load or install one or more packages. Adapted from
#' \url{http://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them}
#' @param package Package name to be loaded.
#' @param \ldots Optional additional packages to load, to eliminate the need for c().
#' @param install Whether to attempt to install missing packages from CRAN. Defaults to \code{TRUE}.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' # Load libraries from a list
#' libs <- c("tools", "utils")
#' Libraries(libs)
#'
#' # Load libraries directly
#' Libraries("Matrix", "MASS")
#'
#' # Warning when loading an unavailable package
#' Libraries("StrKcaQYrvg", install=FALSE)
Libraries <- function(package, ..., install=TRUE) {
  # If no arguments passed
  if (missing(package)) {
    stop("Must enter at least one package")
  }
  # If dot names were passed, append to end of package
  addl <- list(...) %>% as.character()
  if (length(addl) > 0) {
    package <- c(package, addl)
  }
  # Determine which packages (if any) are not installed
  not.found <- package[package %nin% utils::installed.packages()[,"Package"]]
  # Install needed packages
  if (length(not.found) > 0 & install) {
    pkgs <- paste(not.found, sep="", collapse="', '")
    message(
      paste("Attempting to install '", pkgs, "'", sep="")
    )
    utils::install.packages(not.found)
  }
  # Sequentially load packages
  for (p in package) {
    # Attempt to load the package
    require(p, character.only=TRUE)
  }
}
