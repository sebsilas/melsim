% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/melsim.R
\name{melsim}
\alias{melsim}
\title{Compute the melodic similarity between melodies}
\usage{
melsim(
  melody1,
  melody2 = NULL,
  sim_measures = similarity_measures$opti3,
  paired = FALSE,
  verbose = TRUE,
  with_progress = TRUE,
  with_checks = TRUE,
  name = "--"
)
}
\arguments{
\item{melody1}{Vector of melody objects or file names - for comparison.}

\item{melody2}{Vector of melody objects or file names - for comparison.}

\item{sim_measures}{Which similarity algorithm(s) to use.}

\item{name}{}
}
\description{
Compute the melodic similarity between melodies
}
