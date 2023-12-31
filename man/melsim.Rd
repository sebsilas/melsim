% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/melsim.R
\name{melsim}
\alias{melsim}
\title{Compute the melodic similarity of 2 or more melodies}
\usage{
melsim(
  melody1,
  melody2,
  input_type = c("midi_notes_and_durations", "midi_file", "mcsv_file", "musicxml_file",
    "audio_file", "humdrum_file"),
  query_mode = c("one_to_one", "one_to_many", "many_to_many"),
  similarity_algorithm = c("opti3", "edit_distance", "needleman_wunsch",
    "compression_distance_gzip", "set_based", "correlation"),
  ngram_length = 3L,
  transform = NULL
)
}
\arguments{
\item{melody1}{The first melody - or set of melodies - for comparison.}

\item{melody2}{The second melody - or set of melodies - for comparison.}

\item{input_type}{The format in which the melodies are being represented initially.}

\item{query_mode}{The manner in which melody comparisons are made.}

\item{similarity_algorithm}{Which similarity algorithm to use.}

\item{ngram_length}{For set-based similarity, the N-gram length.}

\item{transform}{For set-based similarity, the transform to use.}
}
\description{
Compute the melodic similarity of 2 or more melodies
}
