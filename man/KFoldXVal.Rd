% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross-validation.R
\name{KFoldXVal}
\alias{KFoldXVal}
\title{K-fold Cross-Validation Samples}
\usage{
KFoldXVal(dat, k = 2, prop = NA, seed = NA)
}
\arguments{
\item{dat}{Data to split into cross-validation groups}

\item{k}{Constant k for k-fold cross-validation; defaults to 2}

\item{prop}{Proportion of data to include in the first group; if not provided, defaults
to equal-sized groups. Remaining groups must be at least size 1, and are as nearly
equally sized as possible, with any remainder included in the final group.}

\item{seed}{Optional random seed to use for sampling}
}
\description{
This function takes a dataframe or vector and returns a list of indices for use in
cross-validation.
}
\examples{
# Create some sample data
dat <- LETTERS

# Create a function to print the groups
print.kfcv <- function(q) {
  for (i in 1:length(q)) {print(dat[q[[i]]])}
}

# Default behavior is to create 2 equally-sized sample groups
q <- KFoldXVal(dat, seed=123)
print.kfcv(q)

# For unequal groups, the remainder goes into the last group
q <- KFoldXVal(dat, k=4, seed=123)
print.kfcv(q)

# prop is used to fix the size of the first group
q <- KFoldXVal(dat, prop=.75, seed=123)
print.kfcv(q)

# This may be freely combined with k, provided that there are
# sufficient observations that all groups are at least size 1
q <- KFoldXVal(dat, k=4, prop=.75, seed=123)
print.kfcv(q)

q <- KFoldXVal(dat, k=4, prop=.9, seed=123)
print.kfcv(q)
}
