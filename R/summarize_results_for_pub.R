################################################################################
###
### DATE CREATED: 2022-05-25
###
### AUTHOR: Mark Wheldon
###
### PROJECT: Making Family Planning Count
###
### DESCRIPTION:
###
###     Summarize results of CP Plateaus.
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
##' @export
load_all_plateaus <- function(path_list) {
    stopifnot(is.list(path_list))
    stopifnot(identical(names(path_list), c("rate_01", "rate_03", "rate_05")))

    res <- lapply(path_list, function(z) {
        list(wra = get(load(file.path(z, "rda/wra_all_res_df.rda"))),
             mwra = get(load(file.path(z, "rda/mwra_all_res_df.rda"))))
    })

    ## Upgrade objects. TEMP -- can delete when no longer needed
    res <- lapply(res, function(z) {
        attr(z$wra, "marital_group") <- "wra"
        attr(z$mwra, "marital_group") <- "mwra"
        z <- lapply(z, function(w) {
            if (is.null(attr(w, "FP_plateau_types"))) attr(w, "FP_plateau_types") <- character()
            return(w)
        })
        return(z)
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



##' Filter results of plateau analysis
##'
##' - Filters \code{\link{fpplateaus_data_frame}}s, returning only
##' countries with at least one FP plateau period \emph{or} fertility
##' stall for the indicator and time period specified.
##' - Adds \code{FP_plateau} (logical) column indicating whether the
##' respective year is in a plateau period under the
##' \code{stall_probability}.
##' - Removes columns that start with
##' \dQuote{\code{stall_year_prob_}\var{p}} where \var{p} is not
##' \code{stall_prob}.
##'
##' @param x \code{\link{fpplateaus_data_frame}} loaded from e.g.,
##'     'wra_all_res.rda'.
##' @param stall_probability Probability threshold for a stall.
##' @param indicator FP indicator to keep.
##' @param year_lim Limits of time frame to keep. Leave as \code{NULL}
##'     (default) to keep all years.
##' @param .filter Logical; actually filter out non-stall countries?
##' @return Filtered version of \code{x}, returned as a
##'     \code{data.frame}.
##' @author Mark Wheldon
##' @export
get_fp_plateau_countries <- function(x,
                           stall_probability,
                           indicator,
                           year_lim = NULL,
                           .filter = TRUE) {

    ## Helper to keep only 'stall_year_prob_...' cols that match the selected stall probability.
    remove_extra_stall_year_prob_cols <- function(x, stall_probability) {
        all_cols <- grep("stall_year_prob", colnames(x), value = TRUE)
        keep_cols <- grep(stall_probability, all_cols, value = TRUE)
        drop_cols <- all_cols[!all_cols %in% keep_cols]
        return(x[, !colnames(x) %in% drop_cols])
    }

    stopifnot(is_fpplateaus_data_frame(x))
    stopifnot(identical(length(stall_probability), 1L))
    stopifnot(identical(length(indicator), 1L))

    stopifnot(stall_probability %in% attr(x, "stall_probability_thresholds"))
    stall_prob_group <- paste0("stall_year_prob_", stall_probability, "_group_period")
    if (!stall_prob_group %in% colnames(x))
        stop("Column '", stall_prob_group, "' not found in 'x'. Check argument 'stall_probability'.")
    x <- remove_extra_stall_year_prob_cols(x, stall_probability)

    stopifnot(indicator %in% unique(x$indicator))
    x <- x[x$indicator == indicator, ]
    if (!nrow(x)) stop("No rows in 'x' with 'indicator' = '", indicator, "'.")

    if (!is.null(year_lim)) {
        stopifnot(identical(length(year_lim), 2L) && is.numeric(year_lim) && year_lim[1] <= year_lim[2])
        x_in_year_range <- which(x$year >= year_lim[1] & x$year <= year_lim[2])
        if (!length(x_in_year_range)) stop("No values in range specified by 'year_lim'.")
        x <- x[x_in_year_range, ]
    }

    ## 'FP_plateau' is based on *all* three conditions (level, rate,
    ## probability).
    x$FP_plateau <- !is.na(x[, stall_prob_group]) & x$Level_condition_met

    stall_isos <- unique(x[x$FP_plateau | x$stall_TFR_any, "iso"])
    if (.filter) x <- x[x$iso %in% stall_isos, ] # includes countries with TFR stalls

    ## Return as a plain data.frame
    return(as_fpplateaus_data_frame(x))
}


##' Make data frame of main results for plateau analysis
##'
##' Constructs new columns counting number of stall years,
##' etc. \code{\link{get_fp_plateau_countries}(..., .filter = FALSE)}
##' is immediately called on \code{x}; see the documentation of that
##' function for additional details.
##'
##' @section Note: This function can work on the un-filtered
##' results, i.e., countries with neither a stall nor a plateau are
##' included in the input.
##'
##' @param CP_plateau_type_break,MDMM_plateau_type_break \emph{Obsolete: leave at default.} Breakpoint in
##'     indicator range to use to generate two plateau types.
##' @inheritParams get_fp_plateau_countries
##' @return Data frame with extra attribute \code{"FP_plateau_types"}.
##' @author Mark Wheldon
##' @export
make_main_results_df <- function(x,
                                 stall_probability, indicator, year_lim = NULL,
                                 CP_plateau_type_break = 0.5,
                                 MDMM_plateau_type_break = 0.75) {

    stopifnot(is_fpplateaus_data_frame(x))
    CP_min <- attr(x, "CP_range_condition_min")
    CP_max <- attr(x, "CP_range_condition_max")
    MDMM_min <- attr(x, "MDMM_range_condition_min")
    MDMM_max <- attr(x, "MDMM_range_condition_max")

    ## Need columns added by 'get_fp_plateau_countries()'
    x <- get_fp_plateau_countries(x, stall_probability = stall_probability,
                                  indicator = indicator, year_lim = year_lim,
                                  .filter = FALSE)

    if (!identical(length(na.omit(unique(x$indicator))), 1L)) stop("'indicator' must have exactly one unique value.")

    ## Obsolete now BUT STILL NEEDED to so leave in.
    ## Adds classification of plateaus according to level of FP indicator.
    if (any(c("Modern","Unmet") %in% x$indicator)) {
        stopifnot(CP_plateau_type_break > CP_min && CP_plateau_type_break < CP_max)
        CP_plateau_type_1 <- paste0(CP_min * 100, "% <= MCP < ", CP_plateau_type_break * 100, "%")
        CP_plateau_type_2 <- paste0(CP_plateau_type_break * 100, "% <= MCP < ", CP_max * 100, "%")
        x <- x |>
            dplyr::mutate(FP_plateau_type =
                          dplyr::case_when(
                                     FP_plateau & Level_condition_met
                                     &
                                       indicator %in% c("Unmet", "Modern") &
                                       Modern_median <= CP_plateau_type_break ~ CP_plateau_type_1,
                                     FP_plateau & Level_condition_met
                                     &
                                       indicator %in% c("Unmet", "Modern") &
                                       Modern_median > CP_plateau_type_break ~ CP_plateau_type_2,
                                     TRUE ~ as.character(NA)))
        attr(x, "FP_plateau_types") <- c(CP_plateau_type_1, CP_plateau_type_2)
    }
    if ("MetDemModMeth" %in% x$indicator) {
        stopifnot(MDMM_plateau_type_break > MDMM_min && MDMM_plateau_type_break < MDMM_max)
        stopifnot(MDMM_plateau_type_break > MDMM_min && MDMM_plateau_type_break < MDMM_max)
        MDMM_plateau_type_1 <- paste0(MDMM_min * 100, "% <= MDMM < ", MDMM_plateau_type_break * 100, "%")
        MDMM_plateau_type_2 <- paste0(MDMM_plateau_type_break * 100, "% <= MDMM < ", MDMM_max * 100, "%")
        x <- x |>
            dplyr::mutate(FP_plateau_type =
                          dplyr::case_when(
                                     FP_plateau & Level_condition_met
                                     &
                                       indicator %in% c("MetDemModMeth") &
                                       MetDemModMeth_median <= MDMM_plateau_type_break ~ MDMM_plateau_type_1,
                                     FP_plateau & Level_condition_met
                                     &
                                       indicator %in% c("MetDemModMeth") &
                                       MetDemModMeth_median > MDMM_plateau_type_break ~ MDMM_plateau_type_2,
                                     TRUE ~ as.character(NA)))
        attr(x, "FP_plateau_types") <- c(MDMM_plateau_type_1, MDMM_plateau_type_2)
    }

    x <- x |>
        dplyr::arrange(region, name, year) |>
        dplyr::mutate(TFR_stall_type =
                          dplyr::case_when(stall_TFR ~ "Strong+ evidence",
                                           stall_TFR_moderate ~ "Moderate evidence",
                                           stall_TFR_weak ~ "Limited evidence",
                                           TRUE ~ as.character(NA)),
                      ) |>
        dplyr::mutate(hash_1 = paste(name, FP_plateau_type, TFR_stall_type,
                                     Level_condition_met),
                                # Need level condition ^here because
                                # level condition could change part
                                # way through intervals of
                                # plateaus/stalls.
                      hash_2 = paste(name, FP_plateau_type, Level_condition_met),
                                # This one is not dependent on TFR stall type.
                      block_w_TFR = 1,
                      block_w_FP_type = 1)

    for (i in 2:nrow(x)) {
        if (identical(x[i, "hash_1"], x[i - 1, "hash_1"]))
            x[i, "block_w_TFR"] <- x[i - 1, "block_w_TFR"]
        else x[i, "block_w_TFR"] <- x[i - 1, "block_w_TFR"] + 1

        if (identical(x[i, "hash_2"], x[i - 1, "hash_2"]))
            x[i, "block_w_FP_type"] <- x[i - 1, "block_w_FP_type"]
        else x[i, "block_w_FP_type"] <- x[i - 1, "block_w_FP_type"] + 1
    }

    x <- x |> dplyr::select(-starts_with("hash"))

    return(fpplateaus_data_frame(x))
}


##' Make summary table of FP plateaus and TFR stalls
##'
##' Creates a data frame that can be printed to summarize plateaus and
##' TFR stalls by country and type. Only retains countries with
##' \emph{either or both} a stall and plateau. Additionally, if
##' \code{require_level_condition = TRUE} (defualt), only periods that
##' satisfy the level condition are retained. This could result in no
##' matches, in which case a data frame with zero rows is returned.
##'
##' Compare with
##' \code{\link{make_main_results_table}} which does not include TFR
##' stalls in the blocking of time periods.
##'
##' @param x Output of \code{\link{make_main_results_df}}.
##' @param require_level_condition Logical; keep only periods which
##'     satisfy the level condition?
##' @return Data frame, possibly with zero rows.
##' @author Mark Wheldon
##' @family Main results tables
##' @export
make_main_results_table <- function(x, require_level_condition = TRUE) {
    stopifnot(is.data.frame(x))
    x <- x |>
        dplyr::filter(!is.na(FP_plateau_type) | !is.na(TFR_stall_type))
    if (require_level_condition) {
        x <- x |> dplyr::filter(Level_condition_met)
    }

    if (nrow(x)) {
    x <- x |>
        dplyr::group_by(block_w_TFR) |>
        dplyr::mutate(year_range =
                          dplyr::case_when(identical(min(year), max(year)) ~ as.character(floor(min(year))),
                                           TRUE ~ paste0(floor(min(year)), "-", floor(max(year))))) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
    if (require_level_condition)
        return(dplyr::select(x, c("region", "name", "year_range", "TFR_stall_type", "FP_plateau_type")))
    else
        return(dplyr::select(x, c("region", "name", "year_range", "Level_condition_met", "TFR_stall_type", "FP_plateau_type")))
    } else
        return(data.frame())
}


##' Make summary table of FP plateaus
##'
##' Creates a data frame that can be printed to summarize plateaus by
##' country and type. Only retains countries with a
##' plateau. Additionally, if \code{require_level_condition = TRUE}
##' (defualt), only periods that satisfy the level condition are
##' retained. This could result in no matches, in which case a data
##' frame with zero rows is returned.
##'
##' Compare with \code{\link{make_main_results_table}} which
##' includes TFR stalls in the blocking of time periods.
##'
##' @param x Output of \code{\link{make_main_results_df}}.
##' @param require_level_condition Logical; keep only periods which
##'     satisfy the level condition?
##' @return Data frame, possibly with zero rows.
##' @author Mark Wheldon
##' @family Main results tables
##' @export
make_fp_plateau_only_results_table <- function(x, require_level_condition = TRUE) {
    stopifnot(is.data.frame(x))
    x <- x |>
        dplyr::filter(!is.na(FP_plateau_type))
    if (require_level_condition) {
        x <- x |> dplyr::filter(Level_condition_met)
    }

    if (nrow(x)) {
        x <- x |>
            dplyr::group_by(block_w_FP_type) |>
            dplyr::mutate(year_range =
                              dplyr::case_when(identical(min(year), max(year)) ~ as.character(floor(min(year))),
                                               TRUE ~ paste0(floor(min(year)), "-", floor(max(year))))) |>
            dplyr::slice(1) |>
            dplyr::ungroup()
        if (require_level_condition)
            return(dplyr::select(x, c("region", "name", "year_range", "FP_plateau_type")))
        else
            return(dplyr::select(x, c("region", "name", "year_range", "Level_condition_met", "FP_plateau_type")))
    } else
        return(data.frame())
}


##' Create a list with summaries of stalls at various geographic aggregations
##'
##' Makes \code{n_stall_years_...} data frames at world, region, and country level.
##'
##' @param x Output of \code{\link{make_main_results_df}}.
##' @return List of data frames.
##' @author Mark Wheldon
##' @export
make_n_stall_years_list <- function(x) {
    stopifnot(is.data.frame(x))

    ## Stalls in countries --------------------

    pearson_phi_df <-
        x |>
        dplyr::filter(!is.na(FP_plateau_type) | !is.na(TFR_stall_type)) |>
        dplyr::filter(Level_condition_met) |>
        dplyr::group_by(iso, name, region) |>
        dplyr::summarize(pearson_phi = pearson_phi(FP_plateau, stall_TFR_any))

    n_stall_years_df <-
        x |>
        dplyr::filter(!is.na(FP_plateau_type) | !is.na(TFR_stall_type)) |>
        dplyr::filter(Level_condition_met) |>
        dplyr::group_by(iso, name, region) |>
        dplyr::summarize(TFR_n_stall_years = sum(stall_TFR | stall_TFR_moderate | stall_TFR_weak),
                         FP_n_stall_years = sum(FP_plateau),
                         TFR_not_FP_n_stall_years = sum(!FP_plateau & (stall_TFR | stall_TFR_moderate | stall_TFR_weak)),
                         MCP_not_TFR_n_stall_years = sum(FP_plateau & !(stall_TFR | stall_TFR_moderate | stall_TFR_weak)),
                         Neither_n_stall_years = sum(!FP_plateau & !(stall_TFR | stall_TFR_moderate | stall_TFR_weak)),
                         Either_n_stall_years = sum(stall_TFR | stall_TFR_moderate | stall_TFR_weak | FP_plateau),
                         Overlap_n_stall_years = sum((stall_TFR | stall_TFR_moderate | stall_TFR_weak) &
                                                     FP_plateau),
                         Overlap_n_stall_years_pct = round(100 * Overlap_n_stall_years / Either_n_stall_years, 1))|>
        dplyr::left_join(pearson_phi_df, by = c("iso", "name", "region"))


    ## World Summary --------------------

    pearson_phi_world_df <-
        x |>
        dplyr::filter(!is.na(FP_plateau_type) | !is.na(TFR_stall_type)) |>
        dplyr::filter(Level_condition_met) |>
        dplyr::summarize(pearson_phi = pearson_phi(FP_plateau, stall_TFR_any))

    n_stall_years_world_summary_df <-
        n_stall_years_df |>
        dplyr::ungroup() |>
        dplyr::summarize(TFR_n_stall_years_sum = sum(TFR_n_stall_years),
                         FP_n_stall_years_sum = sum(FP_n_stall_years),
                         TFR_not_FP_n_stall_years_sum = sum(TFR_not_FP_n_stall_years),
                         MCP_not_TFR_n_stall_years_sum = sum(MCP_not_TFR_n_stall_years),
                         Neither_n_stall_years_sum = sum(Neither_n_stall_years),
                         Either_n_stall_years_sum = sum(Either_n_stall_years),
                         Overlap_n_stall_years_sum = sum(Overlap_n_stall_years),
                         Overlap_n_stall_years_pct_sum = round(100 * Overlap_n_stall_years_sum / Either_n_stall_years_sum, 1),
                                #
                         TFR_n_stall_years_mean = mean(TFR_n_stall_years),
                         FP_n_stall_years_mean = mean(FP_n_stall_years),
                         TFR_not_FP_n_stall_years_mean = mean(TFR_not_FP_n_stall_years),
                         MCP_not_TFR_n_stall_years_mean = mean(MCP_not_TFR_n_stall_years),
                         Neither_n_stall_years_mean = mean(Neither_n_stall_years),
                         Either_n_stall_years_mean = mean(Either_n_stall_years),
                         Overlap_n_stall_years_mean = mean(Overlap_n_stall_years),
                         Overlap_n_stall_years_pct_mean = round(100 * mean(Overlap_n_stall_years / Either_n_stall_years, na.rm = TRUE), 1)) |>
        dplyr::mutate(region = "TOTAL",
                      pearson_phi = pearson_phi_world_df[1, 1])

    n_stall_years_world_summary_df <-
        n_stall_years_world_summary_df |>
        dplyr::relocate("pearson_phi", .after = "Overlap_n_stall_years_pct_sum")

    ## Country Summary --------------------

    n_stall_years_country_summary_df <- n_stall_years_df |>
        dplyr::group_by(name) |>
        dplyr::summarize(region = region,
                         TFR_n_stall_years_sum = sum(TFR_n_stall_years),
                         FP_n_stall_years_sum = sum(FP_n_stall_years),
                         TFR_not_FP_n_stall_years_sum = sum(TFR_not_FP_n_stall_years),
                         MCP_not_TFR_n_stall_years_sum = sum(MCP_not_TFR_n_stall_years),
                         Neither_n_stall_years_sum = sum(Neither_n_stall_years),
                         Either_n_stall_years_sum = sum(Either_n_stall_years),
                         Overlap_n_stall_years_sum = sum(Overlap_n_stall_years),
                         Overlap_n_stall_years_pct_sum = round(100 * Overlap_n_stall_years_sum / Either_n_stall_years_sum, 1),
                         pearson_phi = pearson_phi
                         ) |>
        dplyr::bind_rows(dplyr::select(n_stall_years_world_summary_df,
                                       -ends_with("_mean")))

    ## Region Summary --------------------

    pearson_phi_region_sum_df <-
        x |>
        dplyr::filter(Level_condition_met) |>
        dplyr::group_by(region) |>
        dplyr::summarize(pearson_phi = pearson_phi(FP_plateau, stall_TFR_any))

    ## Don't do this. Too many countries have 'NA' correlations.
    ## pearson_phi_region_mean_df <-
    ##     pearson_phi_df |>
    ##     dplyr::group_by(region) |>
    ##     dplyr::summarize(pearson_phi = mean(pearson_phi, na.rm = TRUE))

    n_stall_years_region_df <-
        n_stall_years_df |>
        dplyr::group_by(region) |>
        dplyr::summarize(TFR_n_stall_years_sum = sum(TFR_n_stall_years),
                         FP_n_stall_years_sum = sum(FP_n_stall_years),
                         TFR_not_FP_n_stall_years_sum = sum(TFR_not_FP_n_stall_years),
                         MCP_not_TFR_n_stall_years_sum = sum(MCP_not_TFR_n_stall_years),
                         Neither_n_stall_years_sum = sum(Neither_n_stall_years),
                         Either_n_stall_years_sum = sum(Either_n_stall_years),
                         Overlap_n_stall_years_sum = sum(Overlap_n_stall_years),
                         Overlap_n_stall_years_pct_sum = round(100 * Overlap_n_stall_years_sum / Either_n_stall_years_sum, 1),
                                #
                         TFR_n_stall_years_mean = mean(TFR_n_stall_years),
                         FP_n_stall_years_mean = mean(FP_n_stall_years),
                         TFR_not_FP_n_stall_years_mean = mean(TFR_not_FP_n_stall_years),
                         MCP_not_TFR_n_stall_years_mean = mean(MCP_not_TFR_n_stall_years),
                         Neither_n_stall_years_mean = mean(Neither_n_stall_years),
                         Either_n_stall_years_mean = mean(Either_n_stall_years),
                         Overlap_n_stall_years_mean = mean(Overlap_n_stall_years),
                         Overlap_n_stall_years_pct_mean = round(100 * mean(Overlap_n_stall_years / Either_n_stall_years, na.rm = TRUE), 1)) |>
        dplyr::left_join(pearson_phi_region_sum_df, by = "region") |>
        dplyr::bind_rows(n_stall_years_world_summary_df)

    n_stall_years_region_df <-
        n_stall_years_region_df |>
        dplyr::relocate("pearson_phi", .after = "Overlap_n_stall_years_pct_sum")

    ## Return

    return(list(n_stall_years_df = n_stall_years_df,
                n_stall_years_world_summary_df = n_stall_years_world_summary_df,
                n_stall_years_country_summary_df = n_stall_years_country_summary_df,
                n_stall_years_region_df = n_stall_years_region_df))
}


##' Make list of all results objects
##'
##' Wrapper function that calls all summaries and returns them in a
##' list.
##'
##' @param x \code{\link{fpplateaus_data_frame}} loaded from e.g., 'wra_all_res.rda'.
##' @inheritParams get_fp_plateau_countries
##' @return List of all summaries.
##' @author Mark Wheldon
##' @export
make_all_results_list <- function(x, stall_probability = 0.8, indicator = "Modern",
                                  year_lim = c(1980.5, 2019.5)) {
    stopifnot(is_fpplateaus_data_frame(x))

    ## Filter
    plateaus_df <- get_fp_plateau_countries(x, stall_probability = stall_probability, indicator = indicator,
                                       year_lim = year_lim)
    plateaus_ssa_df <-
        dplyr::filter(plateaus_df, sub_saharanafrica == "Yes")


    ## Results
    main_results_df <- make_main_results_df(x, stall_probability = stall_probability, indicator = indicator,
                                       year_lim = year_lim) |>
        dplyr::filter(sub_saharanafrica == "Yes")

    ## Prepare for output to table
    main_results_tbl <- make_main_results_table(main_results_df, require_level_condition = FALSE)

    main_fp_plateau_only_results_table <-
        make_fp_plateau_only_results_table(main_results_df, require_level_condition = FALSE)

    ## Stall years list
    n_stall_years_list <- make_n_stall_years_list(main_results_df)

    ## RETURN
    return(list(plateaus_df = plateaus_df,
                plateaus_ssa_df = plateaus_ssa_df,
                main_results_df = main_results_df,
                main_results_tbl = main_results_tbl,
                main_fp_plateau_only_results_table = main_fp_plateau_only_results_table,
                n_stall_years_list = n_stall_years_list))
}


##' Make table reporting number of countries with at least one plateau year
##'
##' @param x Result of \code{\link{make_all_results_list}}.
##' @return Integer.
##' @author Mark Wheldon
##' @export
count_n_plateau_countries <- function(x) {
    nrow(subset(x$n_stall_years_list$n_stall_years_country_summary_df,
                subset = FP_n_stall_years_sum > 0 & region != "TOTAL"))
}


##' Make table reporting number of years of plateaus by country
##'
##' @param x Result of \code{\link{make_all_results_list}}.
##' @param rename Character string; new name for the \dQuote{value} column.
##' @return Data frame that can be used as a table.
##' @author Mark Wheldon
##' @export
make_tbl_country_n_plateau_years <- function(x, rename = NULL) {
    tbl_df <- subset(x$n_stall_years_list$n_stall_years_country_summary_df,
                 subset = FP_n_stall_years_sum > 0 & region != "TOTAL",
                 select = c("name", "region", "FP_n_stall_years_sum")) |>
    dplyr::relocate("region") |>
    dplyr::bind_rows(subset(x$n_stall_years_list$n_stall_years_country_summary_df,
                 subset = FP_n_stall_years_sum > 0 & region == "TOTAL",
                 select = c("name", "region", "FP_n_stall_years_sum")))

    if (!is.null(rename))
        colnames(tbl_df)[colnames(tbl_df) == "FP_n_stall_years_sum"] <- rename

    return(tbl_df)
}


##' Make plot comparing FP and TFR plateau periods
##'
##' This is the horizontal bar chart showing years with CP plateaus and FP stalls.
##'
##' Note that the input is an already filtered data frame, so if you
##' want the plot for, e.g., married/in-union women you need to create
##' the appropriate input data set.
##'
##' @param x Result of \code{\link{make_main_results_df}}.
##' @param keep_only_stalls_c Logical; only plot countries with a stall, plateau or both.
##' @param by_FP_plateau_type Logical; separate the FP plateaus by type.
##' @param CP_abbrev Label for FP plateaus in the legend.
##' @param patterns Logical; use \pkg{ggpattern} functions to fill in the bars.
##' @return \code{\link{ggplot2}} plot (invisibly).
##' @author Mark Wheldon
##' @export
make_period_compare_plot <- function(x,
                                     keep_only_stalls_c = TRUE,
                                     by_FP_plateau_type = TRUE,
                                     CP_abbrev = "MCP",
                                     patterns = FALSE) {
    stopifnot(is.data.frame(x))
    ## Copied from 'https://github.com/tidyverse/ggplot2/issues/3171'
    ## --->|
    guide_axis_label_trans <- function(label_trans = identity, ...) {
        axis_guide <- guide_axis(...)
        axis_guide$label_trans <- rlang::as_function(label_trans)
        class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
        axis_guide
    }

    guide_train.guide_axis_trans <- function(x, ...) {
        trained <- NextMethod()
        trained$key$.label <- x$label_trans(trained$key$.label)
        trained
    }
    ## |<---

    indicator <- unique(x$indicator)
    if (!identical(length(indicator), 1L))
        stop("'x' does not have exactly one unique value for 'indicator'.")


    ## Data Frames

    if (keep_only_stalls_c) {
        ## iso_keep <- unique(x[x$FP_plateau | x$TFR_stall, "iso"])
        iso_keep <- unique(x[x$FP_plateau | x$stall_TFR_any, "iso"])
        x <- x[x$iso %in% iso_keep, ]
    }

    x <- x |>
        dplyr::arrange(region, name, year) |>
        dplyr::mutate(FP_out_of_range = !Level_condition_met)

    x$name <- factor(x$name, levels = sort(unique(x$name), decreasing = TRUE), ordered = TRUE)

    ## if (indicator %in% c("Modern", "Unmet"))
    ##     x <- x |> dplyr::mutate(FP_out_of_range = !Level_condition_met)
    ## else if (indicator %in% "MetDemModMeth")
    ##     x <- x |> dplyr::mutate(FP_out_of_range = !MDMM_in_range)

    x <- x |>
        dplyr::select(c("name", "year", "FP_out_of_range", "FP_plateau_type", "TFR_stall_type",
                        ## "FP_plateau", "TFR_stall", "region"))
                        "FP_plateau", "stall_TFR_any", "region","Level_condition_met"))

    x$y_loc <- x$name

    x$TFR_stall_type <-
        factor(x$TFR_stall_type,
               levels = as.character(na.omit(unique(x$TFR_stall_type))),
               labels = paste0("TFR: ", as.character(na.omit(unique(x$TFR_stall_type)))))

    if (by_FP_plateau_type) x$FP_plateau_type <- factor(x$FP_plateau_type)
    else x$FP_plateau_type <- paste0(CP_abbrev, " Plateau\n(Rate condition threshold = 0.5, probability = 80%)")

    ## Plot Features

    bar_height <- 0.4
    bar_alpha <- 0.75
    fill_values <- c("gray75", RColorBrewer::brewer.pal(n = 11, "RdBu")[c(1,2,8,10,11)])
    if (by_FP_plateau_type) {
        fill_names <- c("Level condition not met",
                        attr(x, "FP_plateau_types"),
                        "TFR: Limited evidence",
                        "TFR: Moderate evidence", "TFR: Strong+ evidence")
    } else {
        fill_names <- c("Level condition not met",
                        paste0(CP_abbrev, " Plateau\n(Rate condition threshold = 0.5, probability = 80%)"), "",
                        "TFR: Limited evidence",
                        "TFR: Moderate evidence", "TFR: Strong+ evidence")
    }

    gp <- ggplot(data = x, aes(x = year)) +
        scale_fill_manual(values = setNames(fill_values,
                                            fill_names),
                          name = "Plateau and Stall Type",
                          guide = guide_legend(ncol = 2, reverse = TRUE)) +
        guides(y.sec = guide_axis_label_trans()) +
        scale_x_continuous(expand = c(0, 0), limits = c(1985, 2020), name = "Year") +
        scale_y_discrete(expand = c(0, 0)) +
        ggforce::facet_col(region ~ ., scales = "free_y", space = "free") +
        ylab("Country") +
        theme(legend.position = "bottom",
              axis.text.y = element_text(vjust = -0.9),
              strip.background = element_rect(fill = "black"),
              strip.text = element_text(colour = "white"),
              panel.grid.major.y = element_line(colour = "black"))

    if (patterns) {
        return(gp +
               ggpattern::scale_pattern_manual(values = setNames(c("pch",
                                                                   rep("none", length(fill_names) - 1)),
                                                                 fill_names),
                                               name = "Plateau and Stall Type") +
               ggpattern::geom_tile_pattern(data = subset(x, FP_out_of_range),
                                            aes(y = y_loc, fill = "Level condition not met",
                                                pattern = "Level condition not met"),
                                            height = 0.9,
                                            col = NA,
                                            position = position_nudge(y = 0.5),
                                            alpha = bar_alpha,
                                            pattern_shape = 20,
                                            pattern_density = 0.1,
                                            pattern_angle = 0,
                                            pattern_fill = "darkgray",
                                            pattern_colour = "darkgray",
                                            pattern_alpha = 0.25
                                            ) +
               ggpattern::geom_tile_pattern(data = subset(x, ## TFR_stall
                                                          stall_TFR_any
                                                          ),
                                            aes(y = y_loc, fill = TFR_stall_type, height = bar_height,
                                                pattern = TFR_stall_type),
                                            position = position_nudge(y = 0.6), alpha = bar_alpha) +
               ggpattern::geom_tile_pattern(data = subset(x, FP_plateau),
                                            aes(y = y_loc, fill = FP_plateau_type, height = bar_height,
                                                pattern = FP_plateau_type),
                                            position = position_nudge(y = 0.4), alpha = bar_alpha))
    } else {
        return(gp +
               geom_tile(data = subset(x, FP_out_of_range),
                                    aes(y = y_loc, fill = "Level condition not met"),
                                    height = 0.9,
                                    col = NA,
                                    position = position_nudge(y = 0.5),
                                    alpha = bar_alpha) +
               geom_tile(data = subset(x, ## TFR_stall
                                       stall_TFR_any
                                       ),
                                    aes(y = y_loc, fill = TFR_stall_type, height = bar_height),
                                    position = position_nudge(y = 0.6), alpha = bar_alpha) +
               geom_tile(data = subset(x, FP_plateau),
                                    aes(y = y_loc, fill = FP_plateau_type, height = bar_height),
                                    position = position_nudge(y = 0.4), alpha = bar_alpha))
    }
}

##----------------------------------------------------------------------

##' Compare plateaus plot
##'
##' Wrapper for \code{\link{stall_plot}} that shows stall
##' probabilities for different rate condition thresholds.
##'
##' @param c_code Numeric country code
##' @param res_05_df,res_03_df,res_01_df Results to plot and compare;
##'     results of \code{\link{load_all_plateaus}}. Currently, \emph{must} be
##'     for rate condition thresholds 0.5, 0.3, and 0.1 percentage
##'     points.
##' @param CP_abbrev Label for FP plateaus in the legend.
##' @param probability_scale Percent or proportion.
##' @param line_colour Line colour
##' @param fill_legend_title Title for fill legend.
##' @param linetype_legend_title Title for linetype legend.
##' @param ... Passed to \code{stall_plot}.
##' @return A ggplot.
##' @author Mark Wheldon
##' @seealso \code{\link{plateau_ts_plot}}
##' @export
plateau_compare_def_plot <- function(c_code, res_05_df, res_03_df, res_01_df, CP_abbrev = "MCP",
                                     probability_scale = c("percent", "prop"),
                                     line_colour = "black",
                                     fill_legend_title = "Plateau Type",
                                     linetype_legend_title = "Rate Condition Threshold", ...) {

    probability_scale <- match.arg(probability_scale)

    extra_df <- rbind(data.frame(subset(res_03_df, iso == c_code)[, c("year", "stall_prob")],
                                 rate_condition = "0.3"),
                      data.frame(subset(res_01_df, iso == c_code)[, c("year", "stall_prob")],
                                 rate_condition = "0.1"),
                      data.frame(year = NA, stall_prob = NA, rate_condition = "0.5")
                      )

    ref_line_90 <- 0.9
    ref_line_95 <- 0.95

    if (identical(probability_scale, "percent")) {
        extra_df$stall_prob <- 100 * extra_df$stall_prob
        ref_line_90 <- ref_line_90 * 100
        ref_line_95 <- ref_line_95 * 100
    }

    stall_plot(subset(res_05_df, iso == c_code),
               CP_abbrev = paste0(CP_abbrev, " Plateau\n(Rate condition threshold = 0.5, probability = 80%"),
               xvar = "year",
               yvar = "stall_prob",
               stall_probability_threshold = 0.8,
               probability_scale = probability_scale,
               add_range_ref_lines = TRUE,
               add_TFR_stalls = FALSE,
               legend_title = fill_legend_title,
               line_colour = line_colour, ...) +
        geom_line(data = extra_df, aes(x = year, y = stall_prob, linetype = rate_condition),
                  colour = line_colour) +
        geom_hline(aes(yintercept = ref_line_90), col = "blue", linetype = 2) +
        geom_hline(aes(yintercept = ref_line_95), col = "blue", linetype = 2) +
        scale_linetype_manual(values = c(`0.1` = 3, `0.3` = 2, `0.5` = 1), name = linetype_legend_title) +
        guides(linetype = guide_legend(title.position = "top"))
}

##----------------------------------------------------------------------

##' Side-by-side time series plots
##'
##' Wrapper for \code{\link{stall_plot}} and \code{\link{plateau_compare_def_plot}} to produce side-by-side time
##' series plots of FP indicators and plateau probabilities.
##'
##' @inheritParams plateau_compare_def_plot
##' @param ... Passed to \code{stall_plot}.
##' @return A list of ggplot objects.
##' @import ggplot2
##' @export
plateau_ts_plot <- function(c_code, res_df,
                            res_03_df = NULL, res_01_df = NULL,
                            CP_abbrev = "MCP",
                            CP_not_in_range_abbrev = CP_abbrev,
                            yvar_fp_plot = c("Modern_median", "stall_prob"),
                            probability_scale = c("percent", "prop"),
                            add_TFR_stalls = TRUE,
                            legend_title = "Plateau Type",
                            linetype_legend_title = "Rate Condition Threshold",
                            stall_prob_line_colour = "black", ...) {

    probability_scale <- match.arg(probability_scale)

    lt_leg_title_old <- linetype_legend_title
    out_list <- lapply(yvar_fp_plot, function(z) {
        if (identical(z, "stall_prob")) {
            lt_leg_title_z <- paste0(linetype_legend_title, "\n('Plateau Probability' panels)")
            lc_z <- stall_prob_line_colour
        } else {
            lt_leg_title_z <- linetype_legend_title
            lc_z <- formals(stall_plot)$line_colour
        }
        if (identical(z, "stall_prob") && !is.null(res_03_df) && !is.null(res_01_df)) {
            plateau_compare_def_plot(c_code, res_df, res_03_df, res_01_df, CP_abbrev = CP_abbrev,
                                     probability_scale = probability_scale,
                                     line_colour = lc_z,
                                     fill_legend_title = legend_title,
                                     linetype_legend_title = lt_leg_title_z,
                                     ...)
        } else {
            stall_plot(subset(res_df, iso == c_code),
                       CP_abbrev = paste0(CP_abbrev, " Plateau\n(Rate cond. threshold = 0.5,\nprobability = 80%)"),
                       CP_not_in_range_abbrev = CP_not_in_range_abbrev,
                       facet_by_indicator = FALSE,
                       yvar = z,
                       stall_probability_threshold = 0.8,
                       probability_scale = probability_scale,
                       add_TFR_stalls = add_TFR_stalls,
                       legend_title = legend_title,
                       line_colour = lc_z,
                       ...) +
                theme(plot.title = element_text(size = rel(1)))
        }
    })

    if (length(out_list) > 1)
        out_list[2:length(out_list)] <-
            lapply(out_list[2:length(out_list)], function(z) z + labs(title = " "))

    return(out_list)
}

##----------------------------------------------------------------------

##' Side-by-side time series plots
##'
##' Wrapper for \code{\link{plateau_ts_plot}} to produce plots for
##' multiple countries in a single (multi-page) plot. This should be
##' wrapped by \code{\link{pdf}} ... \code{\link{dev.off()}} to have
##' output saved to a PDF file.
##'
##' @inheritParams plateau_ts_plot
##' @param indicator_col_title Text string; title for the indicator column of plots.
##' @param plateau_prob_col_title Text string; title for the plateau probability column of plots.
##' @param ... Passed to \code{\link{plateau_ts_plot}}.
##' @return Called for side effect of printing a ggplot.
##' @import ggplot2
##' @export
plateau_ts_plot_multiple <- function(res_df,
                                     indicator_col_title = "Modern Contraceptive Prevalence",
                                     plateau_prob_col_title = "Plateau Probability",
                                     ...) {

    all_isos <- na.omit(unique(res_df$iso))

    plot_mcp_stall_li <-
        purrr::flatten(lapply(all_isos,
                              function(z) plateau_ts_plot(z, res_df = res_df, ...)))

    n_plots <- length(plot_mcp_stall_li)
    n_rows_per_page <- 4
    n_plots_per_page <- n_rows_per_page * 2
    n_pages <- ceiling(n_plots / n_plots_per_page)
    n_plots_last_page <- n_plots - (n_pages - 1) * n_plots_per_page
    page_assignments <- gl(n_pages, n_plots_per_page, length = n_plots)

    for (pg in 1:(n_pages - 1)) {
        gridExtra::grid.arrange(grobs = c(list(grid::textGrob(indicator_col_title),
                                               grid::textGrob(plateau_prob_col_title)),
                                          lapply(plot_mcp_stall_li[page_assignments == pg],
                                                 function(z) z + theme(legend.position="none")),
                                          list(cowplot::get_legend(plot_mcp_stall_li[[1]]))),
                                layout_matrix = rbind(c(1, 2),
                                                      matrix(c(rep(seq(from = 3, to = n_plots_per_page + 2,
                                                                       by = 2), each = 6),
                                                               rep(seq(from = 4, to = n_plots_per_page + 3,
                                                                       by = 2), each = 6)),
                                                             ncol = 2),
                                                      matrix(n_plots_per_page + 3, nrow = 2, ncol = 2)))
    }
    gridExtra::grid.arrange(grobs = c(list(grid::textGrob(indicator_col_title),
                                           grid::textGrob(plateau_prob_col_title)),
                                      lapply(plot_mcp_stall_li[page_assignments == n_pages],
                                             function(z) z + theme(legend.position="none")),
                                      list(cowplot::get_legend(plot_mcp_stall_li[[1]]))),
                            layout_matrix = rbind(c(1, 2),
                                                  matrix(c(rep(seq(from = 3, to = n_plots_last_page + 2,
                                                                   by = 2), each = 6),
                                                           rep(seq(from = 4, to = n_plots_last_page + 3,
                                                                   by = 2), each = 6)),
                                                         ncol = 2),
                                                  matrix(n_plots_per_page + 3, nrow = 2, ncol = 2)))
}
