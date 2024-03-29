##' Sample of FP indicator trajectories for all women
##'
##' A \emph{non-random}, \emph{non-representative} sample of indicator
##' \acronym{MCMC} trajectories for all women.
##'
##' @format ## `sample_trajectories_all_women`
##' A list of arrays. Each list element contains an array of
##' trajectories for a single country. Element are names are the ISO
##' numeric country codes. The only countries included are in
##' \code{\link{isos_test_countries}}. Each array has \code{dim}
##' \verbatim{[61, 6, 10]}. The first dimension is the year, the
##' second the indicator, and the third indexes trajectory. There are
##' ten trajectories in each array (i.e,. the size of the third
##' dimension is 10).
##'
##' @section Warning:
##' These trajectory sets are for testing purposes only. They are
##' \emph{not} suitable for use in analysis.
##'
##' @source See \url{https://www.un.org/development/desa/pd/data/family-planning-indicators} and \url{https://github.com/FPcounts/FPEMglobal}
##'
##' @family sample_trajectories
"sample_trajectories_all_women"


##' Sample of FP indicator trajectories for married/in-union women
##'
##' A \emph{non-random}, \emph{non-representative} sample of indicator
##' \acronym{MCMC} trajectories for married/in-union women.
##'
##' @format ## `sample_trajectories_married_women`
##' See \code{link{sample_trajectories_all_women}}.
##'
##' @section Warning:
##' These trajectory sets are for testing purposes only. They are
##' \emph{not} suitable for use in analysis.
##'
##' @source See \url{https://www.un.org/development/desa/pd/data/family-planning-indicators} and \url{https://github.com/FPcounts/FPEMglobal}
##'
##' @family sample_trajectories
"sample_trajectories_married_women"


##' Country ISO codes using in testing
##'
##' Vector of the numeric codes of the countries included
##' in the test data files \code{link{sample_trajectories_all_women}}
##' and \code{link{sample_trajectories_married_women}}, and the sample
##' output files \code{\link{sample_output_wra_all_res_df}} and
##' \code{\link{sample_output_mwra_all_res_df}}.
##'
##' @format ## `isos_test_countries`
##' A numeric vector.
##'
##' @source See \url{https://www.un.org/development/desa/pd/data/family-planning-indicators} and \url{https://github.com/FPcounts/FPEMglobal}
##'
"isos_test_countries"


##' Sample output for all women
##'
##' Ouptut generated by running \code{\link{make_all_results}} on
##' \code{link{sample_trajectories_all_women}}.
##'
##' @format ## `sample_output_wra_all_res_df`
##' An \code{\link{fpplateaus_data_frame}} object.
##'
##' @section Warning:
##' These results are for demonstration purposes only. They are
##' \emph{not} suitable for use in analysis.
##'
##' @source See \url{https://www.un.org/development/desa/pd/data/family-planning-indicators} and \url{https://github.com/FPcounts/FPEMglobal}
##'
##' @family sample_outputs
"sample_output_wra_all_res_df"


##' Sample output for married/in-union women
##'
##' Ouptut generated by running \code{\link{make_all_results}} on
##' \code{link{sample_trajectories_married_women}}.
##'
##' @format ## `sample_output_mwra_all_res_df`
##' An \code{\link{fpplateaus_data_frame}} object.
##'
##' @section Warning:
##' These results are for demonstration purposes only. They are
##' \emph{not} suitable for use in analysis.
##'
##' @source See \url{https://www.un.org/development/desa/pd/data/family-planning-indicators} and \url{https://github.com/FPcounts/FPEMglobal}
##'
##' @family sample_outputs
"sample_output_mwra_all_res_df"
