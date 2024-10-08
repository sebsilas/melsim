% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ngram_util.R
\name{get_all_ngrams}
\alias{get_all_ngrams}
\title{Get all ngrams from a given vector}
\usage{
get_all_ngrams(x, N = 3, collapse = ",", keep_length = FALSE)
}
\arguments{
\item{x}{Arbitraty Vector}

\item{N}{(set of positve integers) N.gram lengths}

\item{collapse}{(string) Glue string for ngrams string representation}

\item{keep_length}{(logical) Shall the length of the original vector be kept by including NA at the end?}
}
\value{
Data frame of ngrams with N columns
}
\description{
Get all ngrams from a given vector
}
