##' Sample of FP indicator trajectories for all women
##'
##' A \emph{non-random}, \emph{non-representative} sample of indicator
##' \acronym{MCMC} trajectories for all women.
##'
##' @format ## `sample_trajectories_all_women`
##' A list of arrays. Each list element contains an array of
##' trajectories for a single country. Element are names are the ISO
##' numeric country codes. Only three countries are included: Nigeria
##' (ISO 566), Gambia (ISO 270), Zimbabwe (ISO 716). Each array has
##' \code{dim} \verbatim{[61, 6, 10]}. The first dimension is the
##' year, the second the indicator, and the third indexes
##' trajectory. There are ten trajectories in each array (i.e,. the
##' size of the third dimension is 10).
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
##' A list of arrays. Each list element contains an array of
##' trajectories for a single country. Element are names are the ISO
##' numeric country codes. Only three countries are included: Nigeria
##' (ISO 566), Gambia (ISO 270), Zimbabwe (ISO 716). Each array has
##' \code{dim} \verbatim{[61, 6, 10]}. The first dimension is the
##' year, the second the indicator, and the third indexes
##' trajectory. There are ten trajectories in each array (i.e,. the
##' size of the third dimension is 10).
##'
##' @section Warning:
##' These trajectory sets are for testing purposes only. They are
##' \emph{not} suitable for use in analysis.
##'
##' @source See \url{https://www.un.org/development/desa/pd/data/family-planning-indicators} and \url{https://github.com/FPcounts/FPEMglobal}
##'
##' @family sample_trajectories
"sample_trajectories_married_women"
