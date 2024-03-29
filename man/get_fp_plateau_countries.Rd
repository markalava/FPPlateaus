% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_results_for_pub.R
\name{get_fp_plateau_countries}
\alias{get_fp_plateau_countries}
\title{Filter results of plateau analysis}
\usage{
get_fp_plateau_countries(
  x,
  stall_probability,
  indicator,
  year_lim = NULL,
  .filter = TRUE
)
}
\arguments{
\item{x}{\code{\link{fpplateaus_data_frame}} loaded from e.g.,
'wra_all_res.rda'.}

\item{stall_probability}{Probability threshold for a stall.}

\item{indicator}{FP indicator to keep.}

\item{year_lim}{Limits of time frame to keep. Leave as \code{NULL}
(default) to keep all years.}

\item{.filter}{Logical; actually filter out non-stall countries?}
}
\value{
Filtered version of \code{x}, returned as a
\code{data.frame}.
}
\description{
\itemize{
\item Filters \code{\link{fpplateaus_data_frame}}s, returning only
countries with at least one FP plateau period \emph{or} fertility
stall for the indicator and time period specified.
\item Adds \code{FP_plateau} (logical) column indicating whether the
respective year is in a plateau period under the
\code{stall_probability}.
\item Removes columns that start with
\dQuote{\code{stall_year_prob_}\var{p}} where \var{p} is not
\code{stall_prob}.
}
}
\author{
Mark Wheldon
}
