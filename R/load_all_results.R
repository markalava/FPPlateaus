################################################################################
###
### DATE CREATED: 2022-10-13
###
### AUTHOR: Mark Wheldon
###
### PROJECT: Making Family Planning Count: CP Plateaus
###
### DESCRIPTION: Load results
###
###-----------------------------------------------------------------------------
###
################################################################################


##' Simple wrapper to load all results for a given rate condition threshold
##'
##' Loads results for a single method (e.g., linear smoothing) and a
##' single rate condition threshold. Returns a list of data frames,
##' one for each combination of indicator, marital group and
##' probability condition threshold.
##'
##' @param path_list List which must have names \dQuote{rate_01},
##'     \dQuote{rate_03}, \dQuote{rate_05}.
##' @param rate_cond_thold Rate condition threshold to load.
##' @return A list of data frames.
##' @author Mark Wheldon
load_all_plateaus <- function(path_list) {
    stopifnot(is.list(path_list))
    stopifnot(identical(names(path_list), c("rate_01", "rate_03", "rate_05")))

    res <- lapply(path_list, function(z) {
        list(wra = get(load(file.path(z, "rda/wra_all_res_df.rda"))),
             mwra = get(load(file.path(z, "rda/mwra_all_res_df.rda"))))
        })

    list(MCP = list(wra = lapply(res, function(z) {
                    list(prob_08 = make_all_results_list(z$wra, stall_probability = 0.8, indicator = "Modern"),
                         prob_09 = make_all_results_list(z$wra, stall_probability = 0.9, indicator = "Modern"),
                         prob_095 = make_all_results_list(z$wra, stall_probability = 0.95, indicator = "Modern"))
                }),
                mwra = lapply(res, function(z) {
                    list(prob_08 = make_all_results_list(z$mwra, stall_probability = 0.8, indicator = "Modern"),
                         prob_09 = make_all_results_list(z$mwra, stall_probability = 0.9, indicator = "Modern"),
                         prob_095 = make_all_results_list(z$mwra, stall_probability = 0.95, indicator = "Modern"))
                })),
         MetDemModMeth = list(wra = lapply(res, function(z) {
                    list(prob_08 = make_all_results_list(z$wra, stall_probability = 0.8, indicator = "MetDemModMeth"),
                         prob_09 = make_all_results_list(z$wra, stall_probability = 0.9, indicator = "MetDemModMeth"),
                         prob_095 = make_all_results_list(z$wra, stall_probability = 0.95, indicator = "MetDemModMeth"))
                }),
                mwra = lapply(res, function(z) {
                    list(prob_08 = make_all_results_list(z$mwra, stall_probability = 0.8, indicator = "MetDemModMeth"),
                         prob_09 = make_all_results_list(z$mwra, stall_probability = 0.9, indicator = "MetDemModMeth"),
                         prob_095 = make_all_results_list(z$mwra, stall_probability = 0.95, indicator = "MetDemModMeth"))
                })))
}
