% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format-data.R
\name{ReplaceValues}
\alias{ReplaceValues}
\title{Replace Values}
\usage{
ReplaceValues(data, old.val = NA, new.val = 0, condition = FALSE)
}
\arguments{
\item{data}{Data frame or character vector to be manipulated}

\item{old.val}{Current value to be found; defaults to \code{NA}}

\item{new.val}{New value to take the place of found values; defaults to \code{0}}

\item{condition}{Whether or not to parse the input as a condition; defaults to
\code{FALSE}}
}
\description{
This function is a wrapper for \code{replace()} that takes a vector or data frame
and replaces all instances matching a specified value or condition with a new value.
}
\examples{

# Replace all NA with 0 (default behavior)
(dat <- c(-3:3, NA, NaN))
ReplaceValues(dat)

# Replace negative values with -Inf
ReplaceValues(dat, "<0", -Inf, condition=TRUE)

# Replace a with NA
set.seed(123)
(dat <- sample(letters[1:5], size=20, replace=TRUE))
ReplaceValues(dat, "a", NA)

# Replace a and e with "vowel"
ReplaceValues(dat, "\%in\% c('a', 'e')", "vowel", condition=TRUE)
}
