

###-----------------------------------------------------------------------------
### * Math / Stat

##' @export
bound_within_a_b <- function(x, a, b, tol = 1e-6, tol_a = tol, tol_b = tol) {
    stopifnot(a < b)
    stopifnot(is.finite(tol_a) && is.finite(tol_b))
    stopifnot((b - tol_b) > a && (a + tol_a) < b)
    if (is.list(x))
        return(lapply(x, "bound_within_a_b", a = a, b = b, tol = tol, tol_a = tol_a, tol_b = tol_b))
    x[x >= a & x > b - tol_b] <- b - tol_b
    x[x <= b & x < a + tol_a] <- a + tol_a
    return(x)
}

##' @export
bound_within_zero_one <- function(x, tol = 1e-6, tol_0 = NULL, tol_1 = NULL) {
    bound_within_a_b(x = x, a = 0, b = 1, tol = tol)
}

##' @export
logit <- function(p) log(p / (1 - p))

##' @export
logitSafer <- function(x) {
    ## Budge values of x == 0 or x == 1
    x <- bound_within_zero_one(x)
    log(x/(1-x))
}


###-----------------------------------------------------------------------------
### * Prepare Data

##----------------------------------------------------------------------

##' @export
make_q_diff_df <- function(iso_code, run_name, output_dir = NULL,
                           root_dir = ".",
                           differences,
                           denominator_count_filename,
                           add_iso_column = TRUE,
                           .testing = FALSE) {

    ## MCMC meta
    load(file.path(output_dir, "mcmc.meta.rda"))

    ## Marital group
    if (isTRUE(mcmc.meta$general$all.women.run.copy)) marital_group_long <- "all women"
    else if (identical(mcmc.meta$general$marital.group, "UWRA")) marital_group_long <- "unmarried"
    else marital_group_long <- "married"

    ## Denominators
    denom <- FPEMglobal.aux::get_csv_denominators(run_name = run_name, output_dir = output_dir,
                                       root_dir = root_dir,
                                       filename = denominator_count_filename,
                                       marital_group = marital_group_long) |>
            dplyr::filter(iso == iso_code)

    if (identical(marital_group_long, "all women")) {

        if (isTRUE(.testing)) {
            ## TESTING: Use pre-saved small trajectory arrays
            tra <- new.env()
            data("sample_trajectories_all_women", package = "FPPlateaus", verbose = FALSE, envir = tra)
            tra <- tra$sample_trajectories_all_women[[as.character(iso_code)]]
        } else {
            tra <- FPEMglobal.aux::get_country_traj_aw(run_name = run_name, output_dir = output_dir,
                                                       root_dir = root_dir, iso_code = iso_code)
        }

        denom <- array(denom$count,
                       dim = c(nrow(denom), 1, dim(tra)[[3]]),
                       dimnames = list(denom$year, "count"))
        stopifnot(identical(as.numeric(dim(denom)[1]), as.numeric(dim(tra)[1])))
        for (j in seq_len(dim(tra)[2])) {
            tra[dimnames(denom)[[1]], j, ] <-
                tra[dimnames(denom)[[1]], j, , drop = FALSE] / denom * 1e3
        }
        tra <- aperm(array(
            c(aperm(tra, c(1,3,2)),
              aperm(tra[, "Total", , drop = FALSE] / tra[, "TotalPlusUnmet", , drop = FALSE],
                    c(1,3,2)),
              aperm(tra[, "Modern", , drop = FALSE] / tra[, "TotalPlusUnmet", , drop = FALSE],
                    c(1,3,2))),
            dim = c(dim(tra)[1], dim(tra)[3], dim(tra)[2] + 2),
            dimnames = list(dimnames(tra)[[1]], NULL,
                            c(dimnames(tra)[[2]], "Met Demand", "MetDemModMeth"))),
            c(1,3,2))

    } else {

        if (isTRUE(.testing)) {
            ## TESTING: Use pre-saved small trajectory arrays
            tra <- new.env()
            data("sample_trajectories_married_women", package = "FPPlateaus", verbose = FALSE, envir = tra)
            tra <- tra$sample_trajectories_married_women[[as.character(iso_code)]]
        } else {
            tra <- FPEMglobal.aux::get_country_traj_muw(run_name = run_name, output_dir = output_dir,
                                     root_dir = root_dir,
                                     iso_code = iso_code)
        }

        tra <-
            aperm(
                array(c(aperm(tra, c(1,3,2)),
                        aperm(array(tra[, "Traditional", , drop = FALSE] +
                                    tra[, "Modern", , drop = FALSE],
                                    dim = c(dim(tra)[1], 1, dim(tra)[3]),
                                    dimnames = list(dimnames(tra)[[1]],
                                                    "Total",
                                                    dimnames(tra)[[3]])), c(1,3,2)),
                        aperm(array((tra[, "Traditional", , drop = FALSE] +
                                     tra[, "Modern", , drop = FALSE]) /
                                    (tra[, "Traditional", , drop = FALSE] +
                                     tra[, "Modern", , drop = FALSE] +
                                     tra[, "Unmet", , drop = FALSE]),
                                    dim = c(dim(tra)[1], 1, dim(tra)[3]),
                                    dimnames = list(dimnames(tra)[[1]],
                                                    "Met Demand",
                                                    dimnames(tra)[[3]])), c(1,3,2)),
                        aperm(array((tra[, "Modern", , drop = FALSE]) /
                                    (tra[, "Traditional", , drop = FALSE] +
                                     tra[, "Modern", , drop = FALSE] +
                                     tra[, "Unmet", , drop = FALSE]),
                                    dim = c(dim(tra)[1], 1, dim(tra)[3]),
                                    dimnames = list(dimnames(tra)[[1]],
                                                    "MetDemModMeth",
                                                    dimnames(tra)[[3]])), c(1,3,2)),
                        aperm(array(NA,
                                    dim = c(dim(tra)[1], 1, dim(tra)[3]),
                                    dimnames = list(dimnames(tra)[[1]],
                                                    "TotalPlusUnmet",
                                                    dimnames(tra)[[3]])), c(1,3,2)),
                        aperm(array(NA,
                                    dim = c(dim(tra)[1], 1, dim(tra)[3]),
                                    dimnames = list(dimnames(tra)[[1]],
                                                    "TradPlusUnmet",
                                                    dimnames(tra)[[3]])), c(1,3,2))),
                      dim = c(dim(tra)[1], dim(tra)[3], dim(tra)[2] + 5),
                      dimnames = list(dimnames(tra)[[1]],
                                      dimnames(tra)[[3]],
                                      c(dimnames(tra)[[2]], "Total", "Met Demand", "MetDemModMeth",
                                        "TotalPlusUnmet", "TradPlusUnmet"))),
                c(1,3,2))
    }
    tra_diff <- apply(tra, 2:3, "diff", differences = differences)
    probs <- c(0.025, 0.1, 0.5, 0.9, 0.975)
    q_diff <- apply(tra_diff, 1:2, "quantile", probs = probs, na.rm = TRUE)
    q_diff_df <- plyr::adply(q_diff, 3, .fun = function(z) {
                           tz <- t(z)
                           data.frame(tz * 100, #<< % scale
                                      year = as.numeric(rownames(tz)),
                                      check.names = FALSE)
                       }, .id = "indicator")
    if (add_iso_column) q_diff_df$iso <- iso_code

    ## Medians
    tra_med_df <- as.data.frame(apply(tra, 1:2, "quantile", probs = 0.5, na.rm = TRUE))
    tra_med_df$year <- rownames(tra_med_df)
    colnames(tra_med_df)[!colnames(tra_med_df) == "year"] <-
        paste0(colnames(tra_med_df)[!colnames(tra_med_df) == "year"],
               "_median")

    out <- base::merge(q_diff_df, tra_med_df, by = c("year"))
    colnames(out)[colnames(out) %in% paste0(probs * 100, "%")] <-
        paste0("annual_change_", paste0(probs * 100, "%"))
    return(out)
}

##----------------------------------------------------------------------

##' @export
add_stall_indicators_annual_changes <- function(df, change_condition_percent,
                                                CP_range_condition_min,
                                                CP_range_condition_max,
                                                MDMM_range_condition_min,
                                                MDMM_range_condition_max) {

    df <- dplyr::mutate(df,
                        CP_in_range = Modern_median > CP_range_condition_min &
                            Modern_median <= CP_range_condition_max,
                        MDMM_in_range = MetDemModMeth_median > MDMM_range_condition_min &
                            MetDemModMeth_median <= MDMM_range_condition_max
                        )

    df <- dplyr::mutate(df,
                  stall_year_annual_change =
                      dplyr::case_when(
                                 indicator == "Modern" & CP_in_range & `annual_change_50%` <= 0.5 ~ TRUE,
                                 indicator == "MetDemModMeth" & MDMM_in_range & `annual_change_50%` <= 0.5 ~ TRUE,
                                 indicator == "Unmet" & CP_in_range & (-`annual_change_50%`) <= (-0.5) ~ TRUE,
                                 TRUE ~ FALSE),
                  stall_year_annual_change_U80 = dplyr::case_when(
                                 indicator == "Modern" & CP_in_range & `annual_change_90%` <= 0.5 ~ TRUE,
                                 indicator == "MetDemModMeth" & MDMM_in_range & `annual_change_90%` <= 0.5 ~ TRUE,
                                 indicator == "Unmet" & CP_in_range & (-`annual_change_90%`) <= (-0.5) ~ TRUE,
                                 TRUE ~ FALSE),
                  stall_year_annual_change_U95 = dplyr::case_when(
                                 indicator == "Modern" & CP_in_range & `annual_change_97.5%` <= 0.5 ~ TRUE,
                                 indicator == "MetDemModMeth" & MDMM_in_range & `annual_change_97.5%` <= 0.5 ~ TRUE,
                                 indicator == "Unmet" & CP_in_range & (-`annual_change_97.5%`) <= (-0.5) ~ TRUE,
                                 TRUE ~ FALSE))
    row.names(df) <- NULL
    attr(df, "change_condition_percent") <- change_condition_percent
    attr(df, "CP_range_condition_min") <- CP_range_condition_min
    attr(df, "CP_range_condition_max") <- CP_range_condition_max
    attr(df, "MDMM_range_condition_min") <- MDMM_range_condition_min
    attr(df, "MDMM_range_condition_max") <- MDMM_range_condition_max
    return(df)
}

##----------------------------------------------------------------------

## Computes local linear approximations to trajectories. Argument 'x'
## is assumed to be a matrix for a single indicator with dimensions: 1 = year, 2 = trajectory.

##' @export
lm_local_arr <- function(x, bandwidth, return_value = c("beta", "Y_hat")) {

    stopifnot((bandwidth %% 2) > 0)
    return_value <- match.arg(return_value, several.ok = TRUE)
    if (identical(sort(return_value), sort(c("beta", "Y_hat")))) return_value <- "both"

    X <- matrix(c(rep(1, bandwidth), scale(1:bandwidth, scale = FALSE)), ncol = 2)
    Y_idx <- rep(1:(dim(x)[1] - 2), each = bandwidth) +
        rep(0:2, length.out = bandwidth * (dim(x)[1] - 2))

    lm_local <- function(z, model_matrix, Y_idx, bandwidth, return_value) {

        Y <- cbind(rep(NA, bandwidth),
                   matrix(z[Y_idx], nrow = bandwidth, ncol = length(z) - 2),
                   rep(NA, bandwidth))
        beta <- solve(t(model_matrix) %*% model_matrix) %*% t(model_matrix) %*% Y
        if (identical(return_value, "beta")) return(beta[2,])
        else {
            Y_hat <- (model_matrix %*% beta)
            if (identical(return_value, "Y_hat")) return(Y_hat[2,])
            else return(cbind(beta = beta[2,], Y_hat = Y_hat[2,]))
        }
    }

    apply_lm_local <- function(x, model_matrix, Y_idx, bandwidth, return_value) {
        apply(X = x, MARGIN = 2, FUN = "lm_local", model_matrix = model_matrix,
              Y_idx = Y_idx, bandwidth = bandwidth, return_value = return_value)
    }

    if (!identical(return_value, "both")) {
        return(apply_lm_local(x = x, model_matrix = X, Y_idx = Y_idx,
                              bandwidth = bandwidth, return_value = return_value))
    } else {
        return(array(apply_lm_local(x = x, model_matrix = X, Y_idx = Y_idx,
                                    bandwidth = bandwidth, return_value = return_value),
                     dim = c(dim(x)[1], 2, dim(x)[2]),
                     dimnames = list(year = dimnames(x)[[1]],
                                     parameter = c("beta", "Y_hat"),
                                     traj = 1:dim(x)[2])))
    }
}

##----------------------------------------------------------------------

##' @export
make_stall_prob_df <- function(iso_code, run_name, output_dir = NULL,
                               root_dir = ".",
                               differences,
                               change_condition,
                               smooth_type = c("Annual Difference", "Moving Average", "Local Linear Smooth"),
                               filter_width,
                               denominator_count_filename,
                               add_iso_column = TRUE,
                               .special_return_trajectories = FALSE,
                               .testing = FALSE) {

    smooth_type <- match.arg(smooth_type)
    if (identical(smooth_type, "Annual Difference")) filter_width <- NA

    ## MCMC meta
    load(file.path(output_dir, "mcmc.meta.rda"))

    ## Marital group
    if (isTRUE(mcmc.meta$general$all.women.run.copy)) marital_group_long <- "all women"
    else if (identical(mcmc.meta$general$marital.group, "UWRA")) marital_group_long <- "unmarried"
    else marital_group_long <- "married"

    ## Denominators
    denom <- FPEMglobal.aux::get_csv_denominators(run_name = run_name, output_dir = output_dir,
                                   root_dir = root_dir,
                                   filename = denominator_count_filename,
                                   marital_group = marital_group_long) |>
        dplyr::filter(iso == iso_code)

    ## Trajectories
    if (identical(marital_group_long, "all women")) {
        tra <- FPEMglobal.aux::get_country_traj_aw(run_name = run_name, output_dir = output_dir,
                                   root_dir = root_dir, iso_code = iso_code)

        ## TESTING: Keep only 10 trajectories
        if (isTRUE(.testing)) tra <- tra[,,1:10]

        denom <- array(denom$count,
                       dim = c(nrow(denom), 1, dim(tra)[[3]]),
                       dimnames = list(denom$year, "count"))
        stopifnot(identical(as.numeric(dim(denom)[1]), as.numeric(dim(tra)[1])))
        for (j in seq_len(dim(tra)[2])) {
            tra[dimnames(denom)[[1]], j, ] <-
                tra[dimnames(denom)[[1]], j, , drop = FALSE] / denom * 1e3
        }
        tra <- aperm(array(
            c(aperm(tra, c(1,3,2)),
              aperm(tra[, "Total", , drop = FALSE] / tra[, "TotalPlusUnmet", , drop = FALSE],
                    c(1,3,2)),
              aperm(tra[, "Modern", , drop = FALSE] / tra[, "TotalPlusUnmet", , drop = FALSE],
                    c(1,3,2))),
            dim = c(dim(tra)[1], dim(tra)[3], dim(tra)[2] + 2),
            dimnames = list(dimnames(tra)[[1]], NULL,
                            c(dimnames(tra)[[2]], "Met Demand", "MetDemModMeth"))),
            c(1,3,2))

    } else {

        tra <- FPEMglobal.aux::get_country_traj_muw(run_name = run_name, output_dir = output_dir,
                                     root_dir = root_dir,
                                     iso_code = iso_code)

        ## TESTING: Keep only 10 trajectories
        if (isTRUE(.testing)) tra <- tra[,,1:10]

        tra <-
            aperm(
                array(c(aperm(tra, c(1,3,2)),
                        aperm(array(tra[, "Traditional", , drop = FALSE] +
                                    tra[, "Modern", , drop = FALSE],
                                    dim = c(dim(tra)[1], 1, dim(tra)[3]),
                                    dimnames = list(dimnames(tra)[[1]],
                                                    "Total",
                                                    dimnames(tra)[[3]])), c(1,3,2)),
                        aperm(array((tra[, "Traditional", , drop = FALSE] +
                                     tra[, "Modern", , drop = FALSE]) /
                                    (tra[, "Traditional", , drop = FALSE] +
                                     tra[, "Modern", , drop = FALSE] +
                                     tra[, "Unmet", , drop = FALSE]),
                                    dim = c(dim(tra)[1], 1, dim(tra)[3]),
                                    dimnames = list(dimnames(tra)[[1]],
                                                    "Met Demand",
                                                    dimnames(tra)[[3]])), c(1,3,2)),
                        aperm(array((tra[, "Modern", , drop = FALSE]) /
                                    (tra[, "Traditional", , drop = FALSE] +
                                     tra[, "Modern", , drop = FALSE] +
                                     tra[, "Unmet", , drop = FALSE]),
                                    dim = c(dim(tra)[1], 1, dim(tra)[3]),
                                    dimnames = list(dimnames(tra)[[1]],
                                                    "MetDemModMeth",
                                                    dimnames(tra)[[3]])), c(1,3,2)),
                        aperm(array(NA,
                                    dim = c(dim(tra)[1], 1, dim(tra)[3]),
                                    dimnames = list(dimnames(tra)[[1]],
                                                    "TotalPlusUnmet",
                                                    dimnames(tra)[[3]])), c(1,3,2)),
                        aperm(array(NA,
                                    dim = c(dim(tra)[1], 1, dim(tra)[3]),
                                    dimnames = list(dimnames(tra)[[1]],
                                                    "TradPlusUnmet",
                                                    dimnames(tra)[[3]])), c(1,3,2))),
                      dim = c(dim(tra)[1], dim(tra)[3], dim(tra)[2] + 5),
                      dimnames = list(dimnames(tra)[[1]],
                                      dimnames(tra)[[3]],
                                      c(dimnames(tra)[[2]], "Total", "Met Demand", "MetDemModMeth",
                                        "TotalPlusUnmet", "TradPlusUnmet"))),
                c(1,3,2))
    }


    if (.special_return_trajectories) return(tra)

    if (identical(smooth_type, "Annual Difference")) {

        ## Differences (annual change)
        tra_x <- apply(tra, 2:3, "diff", differences = differences)

    } else if (identical(smooth_type, "Moving Average")) {

        ## Differences (annual change) of MA smoothed trajectories
        tra_x <- apply(tra, 2:3, function(z, f) {
            diff(stats::filter(z, filter = f, sides = 2), differences = differences)
        }, f = rep(1 / filter_width, filter_width))
        dimnames(tra_x)[[1]] <- head(dimnames(tra)[[1]], -1)

    } else if (identical(smooth_type, "Local Linear Smooth")) {

        ## Local linear smooth
        tra_x <-
            aperm(array(apply(tra, MARGIN = 2, "lm_local_arr", bandwidth = filter_width, return_value = "beta",
                        simplify = TRUE),
                  dim = c(dim(tra)[1], dim(tra)[3], dim(tra)[2]),
                  dimnames = list(dimnames(tra)[[1]], dimnames(tra)[[3]], dimnames(tra)[[2]])),
                  c(1,3,2))

    }

    ## Indicators -> probabilities
    tra_x[, c("Total", "Modern", "Traditional", "Met Demand", "MetDemModMeth"), ] <-
        tra_x[, c("Total", "Modern", "Traditional", "Met Demand", "MetDemModMeth"), ] < change_condition
    tra_x[, c("Unmet"), ] <- -(tra_x[, c("Unmet"), ]) < (-change_condition)

    tra_x[, c("TotalPlusUnmet", "TradPlusUnmet"), ] <- NA
    tra_x <- as.data.frame(apply(tra_x, 1:2, "mean"))
    tra_x$year <- as.numeric(rownames(tra_x))
    tra_x <- tra_x |>
        tidyr::gather(!year, key = "indicator",
                      value = "stall_prob")
    if (add_iso_column) tra_x$iso <- iso_code

    ## Medians
    tra_med_df <-
        as.data.frame(aperm(
            apply(tra, 1:2, "quantile", probs = c(0.025, 0.1, 0.5, 0.9, 0.975), na.rm = TRUE),
            c(2, 3, 1)))
    colnames(tra_med_df) <- gsub(pattern = ".50%", "_median", colnames(tra_med_df), fixed = TRUE)
    tra_med_df$year <- rownames(tra_med_df)

    ## Merge and finish
    return(structure(data.frame(base::merge(tra_x, tra_med_df, by = c("year")),
                                check.names = FALSE,
                                row.names = NULL),
                     class = "data.frame",
                     differences = differences,
                     change_condition_as_proportion = change_condition,
                     filter_width = filter_width,
                     denominator_count_filename = denominator_count_filename))
}

##----------------------------------------------------------------------

##' @export
add_level_condition_indicators <- function(df, Level_condition_variant = c("v1 - MCP+SDG", "v1 - SDG Only", "v2 - SDG Only"),
                                                CP_range_condition_min,
                                                CP_range_condition_max,
                                                MDMM_range_condition_min,
                                                MDMM_range_condition_max) {

    ## This allows level condition to depend on multiple indicators,
    ## e.g., level condition for MetDemModMeth could be made to depend
    ## on Modern.

    ## NOTE: 'stall_prob_group' is already conditioned on the
    ## indicator levels. For MCP only on MCP, for SDG only on
    ## MetDemModMeth.

    Level_condition_variant <- match.arg(Level_condition_variant)

    df <- dplyr::mutate(df,
                        CP_in_range = Modern_median > CP_range_condition_min &
                            Modern_median <= CP_range_condition_max,
                        MDMM_in_range = MetDemModMeth_median > MDMM_range_condition_min &
                            MetDemModMeth_median <= MDMM_range_condition_max)

    Level_condition_met <- rep(NA, nrow(df))

    idx <- df$indicator %in% c("Modern", "Unmet")
    Level_condition_met[idx] <- df[idx, "CP_in_range"]

    idx <- df$indicator == "MetDemModMeth"
    if (identical(Level_condition_variant, "v1 - MCP+SDG")) {
        Level_condition_met[idx] <-
            df[idx, "CP_in_range"] & df[idx, "MDMM_in_range"]
    } else if (Level_condition_variant %in% c("v1 - SDG Only", "v2 - SDG Only")) {
        Level_condition_met[idx] <-
            df[idx, "CP_in_range"] & df[idx, "MDMM_in_range"]
    }

    df$Level_condition_met <- Level_condition_met

    if (identical(Level_condition_variant, "v2 - SDG Only")) {

        min_yr_tbl <- df |>
            dplyr::select(iso, indicator, year, Level_condition_met) |>
            dplyr::filter(Level_condition_met) |>
            dplyr::group_by(iso, indicator) |>
            dplyr::summarize(min_year = min(year))

        df <- dplyr::left_join(df, min_yr_tbl, by = c("iso", "indicator"))
        idx <- !is.na(df$Level_condition_met) & df$year >= df$min_year
        df[which(idx), ]$Level_condition_met <- TRUE
        df <- dplyr::select(df, -min_year)

    }

    row.names(df) <- NULL
    attr(df, "CP_range_condition_min") <- CP_range_condition_min
    attr(df, "CP_range_condition_max") <- CP_range_condition_max
    attr(df, "MDMM_range_condition_min") <- MDMM_range_condition_min
    attr(df, "MDMM_range_condition_max") <- MDMM_range_condition_max

    return(df)
}

##----------------------------------------------------------------------

##' @export
add_stall_indicators_probabilities <- function(df, stall_probability_thresholds) {
    idx <- !is.na(df$stall_prob) & !is.na(df$Level_condition_met) &
                 df$indicator %in% c("Modern", "Unmet", "MetDemModMeth")
    for (p in stall_probability_thresholds) {
        stall_year_colname <- paste0("stall_year_prob_", p)
        df[, stall_year_colname] <- FALSE
        df[which(idx & df$stall_prob >= p), stall_year_colname] <- TRUE
    }
    row.names(df) <- NULL
    attr(df, "stall_probability_thresholds") <- stall_probability_thresholds
    return(df)
}

##----------------------------------------------------------------------

##' @export
add_stall_lengths <- function(df, min_stall_length,
                              stall_probability_thresholds,
                              cores = 4) {

    ## -------* Checks

    stall_column_names <- paste0("stall_year_prob_", stall_probability_thresholds)

    stopifnot(all(c("iso", "indicator", "year") %in% colnames(df)))
    stopifnot(all(stall_column_names %in% colnames(df)))
    if (any(is.na(df[, stall_column_names])))
        stop("'df[, stall_column_names]' has missing values.")

    ## -------* Sub-functions

    ## Calculates the extra stall columns: stall start year, stall
    ## group, stall length, and again for "stall_periods", stall
    ## periods that are at least of length 'min_stall_length'. Returns a
    ## data frame with the new columns.
    stall_periods <- function(df, stall_col_name, min_stall_length) {
        ## Call routine
        stall_info_cols <- function(stall, stall_col_name = NULL) {
            nrx <- length(stall)
            nrx_1 <- nrx - 1

            stall_start <- rep(FALSE, nrx)
            stall_start[1] <- stall[1]
            stall_start[2:nrx] <- stall[2:nrx] & !stall[1:nrx_1]

            stall_group <- rep(NA, nrx)
            stall_group[stall] <- cumsum(stall_start)[stall]

            stall_len_tbl <- tapply(stall_group, stall_group, "length")
            stall_len <- rep(NA, nrx)
            for (i in as.numeric(names(stall_len_tbl))) stall_len[stall_group == i] <- stall_len_tbl[i]

            df <- data.frame(stall_start, stall_group, stall_len)
            if (!is.null(stall_col_name))
                colnames(df) <-
                    c(paste0(stall_col_name, c("_start", "_group", "_len")))
            return(df)
        }

        ## Apply twice
        df1 <- stall_info_cols(df[[stall_col_name]], stall_col_name)
        df2 <- stall_info_cols(df[[stall_col_name]] &
                               df1[[paste0(stall_col_name, "_len")]] >= min_stall_length,
                               stall_col_name)

        ## Rename columns
        colnames(df2) <- paste0(colnames(df2), "_period")

        return(data.frame(df1, df2, check.names = FALSE))
    }

    ## Applies 'stall_periods' to multiple stall columns. Returns the
    ## original data frame with new columns appended.
    stall_periods_bind <- function(df, stall_col_names, min_stall_length) {
        data.frame(lapply(stall_col_names, function(z) stall_periods(df, z, min_stall_length)),
                   stringsAsFactors = FALSE)
    }

    ## -------* Main Body

    ## -------** Prepare Inputs

    df <- df[order(df$iso, df$indicator, df$year),]

    ## -------** Add Lengths

    if (!is.null(cores) && length(unique(df$iso)) * length(unique(df$indicator)) > 4 * cores) {
        cl <- parallel::makeCluster(cores)
        parallel::clusterExport(cl, varlist = c("stall_column_names", "min_stall_length"),
                                envir = environment())
        on.exit(parallel::stopCluster(cl = cl), add = TRUE, after = FALSE)
    } else cl <- NULL # cl NULL means process in serial

    spl_f <- list(iso = df$iso, indicator = df$indicator)
    tmp <- unsplit(pbapply::pblapply(split(df, spl_f), function(z) {
                                data.frame(stall_periods_bind(z,
                                                              stall_col_names = stall_column_names,
                                                              min_stall_length = min_stall_length),
                                           row.names = rownames(z))
                            },
                            cl = cl),
                   f = spl_f)

    tmp <- data.frame(df, tmp, check.names = FALSE)
    tmp <- tmp[order(tmp$iso, tmp$indicator, tmp$year),]
    row.names(tmp) <- NULL
    attributes(tmp) <- c(attributes(tmp),
                         attributes(df)[!names(attributes(df)) %in% names(attributes(tmp))])
    attr(tmp, "min_stall_length") <- min_stall_length
    return(tmp)
}

##----------------------------------------------------------------------

##' @export
add_schoumaker_tfr_stalls <- function(x, iso_all) {
    x$stall_TFR <- FALSE
    x$stall_TFR[with(x, {
          iso == get_iso("Zimbabwe", iso_all = iso_all) & year >= 2005.5 & year <= 2015.5
      })] <- TRUE
    x$stall_TFR[with(x, {
          iso == get_iso("Namibia", iso_all = iso_all) & year >= 2007.5 & year <= 2013.5
      })] <- TRUE
    x$stall_TFR[with(x, {
          iso == get_iso("Congo", iso_all = iso_all) & year >= 2005.5 & year <= 2011.5
      })] <- TRUE
    x$stall_TFR[with(x, {
          iso == get_iso("Kenya", iso_all = iso_all) & year >= 1998.5 & year <= 2013.5
      })] <- TRUE
    x$stall_TFR[with(x, {
          iso == get_iso("Zambia", iso_all = iso_all) & year >= 2002.5 & year <= 2007.5
      })] <- TRUE
    x$stall_TFR[with(x, {
          iso == get_iso("Cameroon", iso_all = iso_all) & year >= 1998.5 & year <= 2004.5
      })] <- TRUE

    x$stall_TFR_moderate <- FALSE
    x$stall_TFR_moderate[with(x, {
          iso == get_iso("United Republic of Tanzania", iso_all = iso_all) &
              year >= 1999.5 & year <= 2004.5
      })] <- TRUE
    x$stall_TFR_moderate[with(x, {
          iso == get_iso("South Africa", iso_all = iso_all) &
              year >= 1998.5 & year <= 2016.5
      })] <- TRUE
    x$stall_TFR_moderate[with(x, {
          iso == get_iso("Nigeria", iso_all = iso_all) &
              year >= 1990.5 & year <= 2003.5
      })] <- TRUE
    x$stall_TFR_moderate[with(x, {
          iso == get_iso("Cote d'Ivoire", iso_all = iso_all) &
              year >= 1999.5 & year <= 2012.5
      })] <- TRUE
    x$stall_TFR_moderate[with(x, {
          iso == get_iso("Gabon", iso_all = iso_all) &
              year >= 2000.5 & year <= 2012.5
      })] <- TRUE
    x$stall_TFR_moderate[with(x, {
          iso == get_iso("Madagascar", iso_all = iso_all) &
              year >= 1992.5 & year <= 1997.5
      })] <- TRUE

    x$stall_TFR_weak <- FALSE
    x$stall_TFR_weak[with(x, {
          iso == get_iso("Ghana", iso_all = iso_all) &
              year >= 1998 & year <= 2003
      })] <- TRUE
    x$stall_TFR_weak[with(x, {
          iso == get_iso("Benin", iso_all = iso_all) &
              year >= 2001 & year <= 2006
      })] <- TRUE
    x$stall_TFR_weak[with(x, {
          iso == get_iso("Burkina Faso", iso_all = iso_all) &
              year >= 1993 & year <= 1999
      })] <- TRUE
    x$stall_TFR_weak[with(x, {
          iso == get_iso("Malawi", iso_all = iso_all) &
              year >= 2000 & year <= 2004
      })] <- TRUE
    x$stall_TFR_weak[with(x, {
          iso == get_iso("Rwanda", iso_all = iso_all) &
              year >= 2000 & year <= 2005
      })] <- TRUE
    x$stall_TFR_weak[with(x, {
          iso == get_iso("Cameroon", iso_all = iso_all) &
              year >= 2004 & year <= 2011
      })] <- TRUE
    x$stall_TFR_weak[with(x, {
          iso == get_iso("Zambia", iso_all = iso_all) &
              year >= 1992 & year <= 1996
      })] <- TRUE
    x$stall_TFR_weak[with(x, {
          iso == get_iso("Kenya", iso_all = iso_all) &
              year >= 1993 & year <= 1998
      })] <- TRUE
    x$stall_TFR_weak[with(x, {
          iso == get_iso("Senegal", iso_all = iso_all) &
              year >= 2005 & year <= 2011
      })] <- TRUE
    x$stall_TFR_weak[with(x, {
          iso == get_iso("Uganda", iso_all = iso_all) &
              year >= 2001 & year <= 2006
      })] <- TRUE
    x$stall_TFR_weak[with(x, {
          iso == get_iso("Ghana", iso_all = iso_all) &
              year >= 1988 & year <= 1993
      })] <- TRUE
    x$stall_TFR_weak[with(x, {
          iso == get_iso("Senegal", iso_all = iso_all) &
              year >= 1993 & year <= 1997
      })] <- TRUE

    x$stall_TFR_any <-
        with(x,
             stall_TFR | stall_TFR_moderate | stall_TFR_weak)

    return(x)
}

