###-----------------------------------------------------------------------------
### * Check Arguments

##' @export
check_min_stall_length <- function(min_stall_length, smoothing_method) {
    if (is.null(min_stall_length)) {
        if (identical(smoothing_method, "annual_difference"))
            min_stall_length <- 2
        else min_stall_length <- 1
    } else {
        if (!identical(as.double(min_stall_length), as.double(round(min_stall_length))))
            stop("'min_stall_length' must be a whole number.")
        if (min_stall_length <= 0)
            stop("'min_stall_length' must be positive, non-zero.")
        if (min_stall_length > 5)
            warning("'min_stall_length' > 5; is this OK?")
    }
    return(min_stall_length)
}

##' @export
check_change_condition_percent <- function(change_condition_percent) {
    stopifnot(is.numeric(change_condition_percent))
    stopifnot(change_condition_percent >= 0 &&
              change_condition_percent <= 100)
    return(change_condition_percent)
}


###-----------------------------------------------------------------------------
### * Generate Plateau Results

##' @export
make_all_results <- function(country_isos_to_process = NULL,
                             smoothing_method = c("annual_difference",
                                                  "moving_average",
                                                  "local_linear"),
                             min_stall_length = NULL,
                             change_condition_percent,
                             stall_probability_thresholds = c(0.5, 0.75, 0.8, 0.9, 0.95),
                             CP_range_condition_min = 0.1,
                             CP_range_condition_max = 0.6,
                             MDMM_range_condition_min = 0.05,
                             MDMM_range_condition_max = 0.85,
                             year_lim = c(1980, 2022),
                             project_dir = ".",
                             results_dir_name = "results",
                             FPEM_results_dir,
                             FPEM_results_subdir_names,
                             denominator_count_filename,
                             datestamp = NULL,
                             ncores = parallelly::availableCores(omit = 1),
                             .testing = FALSE,
                             Level_condition_variant = c("v1 - MCP+SDG", "v1 - SDG Only", "v2 - SDG Only")
                                # 'v1' means level condition only met in years when indicator(s) in range.
                                # 'v2' means level condition met from first year indicator in range, and forever thereafter.
                                # 'MCP+SDG' means SDG level condition depends on levels of MCP and SDG.
                                # 'SDG Only' means SDG level condition depends only on level of SDG.
                             ) {

    message("\n\n\nMaking all results")

    ## -------* Check Arguments

    smoothing_method <- match.arg(smoothing_method)
    min_stall_length <- check_min_stall_length(min_stall_length, smoothing_method)
    change_condition_percent <- check_change_condition_percent(change_condition_percent)

    stopifnot(is.numeric(year_lim))
    stopifnot(year_lim[2] > year_lim[1])
    stopifnot(year_lim[1] >= 1970)
    stopifnot(year_lim[2] <= 2030)

    Level_condition_variant <- match.arg(Level_condition_variant)


    ## -------* Constants

    if (is.null(datestamp)) datestamp <- format(Sys.time(), "%Y%m%d")

    filepaths_outputs <- make_filepaths_outputs(project_dir = project_dir,
                                                results_dir_name = results_dir_name,
                           datestamp = datestamp,
                           smoothing_method = smoothing_method,
                           min_stall_length = min_stall_length,
                           change_condition_percent = change_condition_percent)

    filepaths_inputs <- make_filepaths_inputs(FPEM_results_dir = FPEM_results_dir,
                                              FPEM_results_subdir_names = FPEM_results_subdir_names,
                                              denominator_count_filename)

    iso_all <- get_iso_all(output_dir = filepaths_inputs$res_dir_mwra)
    save_iso_all_to_results(iso_all, filepaths_outputs)

    if (is.null(country_isos_to_process)) {
        country_isos_to_process <- setNames(iso_all$iso, iso_all$iso)
    } else {
        stopifnot(is.numeric(country_isos_to_process))
        if (is.null(names(country_isos_to_process)))
            country_isos_to_process <- setNames(country_isos_to_process, country_isos_to_process)
    }

    smooth_type <- switch(smoothing_method,
                          annual_difference = "Annual Difference",
                          moving_average = "Moving Average",
                          local_linear = "Local Linear Smooth")
    change_condition <- change_condition_percent / 100


    ## -------* All Women (WRA)

    message("\n\n(1 of 2) .. All women (WRA)")

    ## -------** Create Stall Probabilities

    message("\n  (1 of 3) .. Creating stall probabilities")

    cl <- parallel::makeCluster(min(ncores, max(1, floor(length(country_isos_to_process) / 4))))
    parallel::clusterExport(cl, varlist = c("make_stall_prob_df", "lm_local_arr", "filepaths_inputs",
                                  "smooth_type", "smoothing_method", "min_stall_length",
                                  "denominator_count_filename",
                                  "change_condition", ".testing"),
                            envir = environment())
    stall_prob_wra_df <-
        pbapply::pblapply(country_isos_to_process,
                 function(z) {
            library(magrittr)
            library(FPEMglobal.aux)
            make_stall_prob_df(z, run_name = filepaths_inputs$FPEM_results_subdir_names$rn_wra,
                               output_dir = filepaths_inputs$res_dir_wra, add_iso_column = TRUE,
                               smooth_type = smooth_type,
                               differences = 1, # the order of the differences, hardcoded
                               change_condition = change_condition,
                               filter_width = 3, # the width of the window for moving average or ols, hardcoded
                               denominator_count_filename = denominator_count_filename,
                               .testing = .testing)
        },
        cl = cl)
    parallel::stopCluster(cl = cl)

    stall_prob_wra_df <- do.call("rbind", stall_prob_wra_df)

    stall_prob_wra_df <-
        within(stall_prob_wra_df,
               in_time_window <- year >= min(year_lim) & year <= max(year_lim))

    ## Add Single-year Stall Indicators

    stall_prob_wra_df <-
        add_stall_indicators_probabilities(stall_prob_wra_df,
                                           stall_probability_thresholds = stall_probability_thresholds,
                                           CP_range_condition_min = CP_range_condition_min,
                                           CP_range_condition_max = CP_range_condition_max,
                                           MDMM_range_condition_min = MDMM_range_condition_min,
                                           MDMM_range_condition_max = MDMM_range_condition_max)

    ## Add stall length info

    message("\n  (2 of 3) .. Calculating stall lengths")

    stall_prob_wra_df <-
        add_stall_lengths(stall_prob_wra_df,
                          min_stall_length = min_stall_length,
                          stall_probability_thresholds = attr(stall_prob_wra_df,
                                                              "stall_probability_thresholds"),
                          cores = ncores)

    ## Add level condition indicator

    stall_prob_wra_df <-
        add_level_condition_indicators(stall_prob_wra_df,
                                       Level_condition_variant = Level_condition_variant)

    ## Add Schoumaker's TFR stalls

    stall_prob_wra_df <- add_schoumaker_tfr_stalls(stall_prob_wra_df, iso_all = iso_all)


    ## -------** Create 'Annual Change' Data

    message("\n  (3 of 3) .. Creating annual change data")

    ## Extract and summarize posterior trajectories

    cl <- parallel::makeCluster(min(ncores, max(1, floor(length(country_isos_to_process) / 4))))
    parallel::clusterExport(cl, varlist = c("make_q_diff_df", "filepaths_inputs",
                                            "denominator_count_filename", ".testing"),
                            envir = environment())
    annual_cp_change_wra_df <-
        pbapply::pblapply(country_isos_to_process,
                 function(z) {
            library(magrittr)
            library(FPEMglobal.aux)
            make_q_diff_df(z, run_name = filepaths_inputs$FPEM_results_subdir_names$rn_wra,
                           output_dir = filepaths_inputs$res_dir_wra, add_iso_column = TRUE,
                           differences = 1,
                           denominator_count_filename = denominator_count_filename,
                           .testing = .testing)
        },
        cl = cl)
    parallel::stopCluster(cl = cl)

    annual_cp_change_wra_df <- do.call("rbind", annual_cp_change_wra_df)


    ## -------** Merge and Save

    ## Merge annual changes and probabilities

    wra_all_res_df <-
        dplyr::left_join(stall_prob_wra_df,
                         annual_cp_change_wra_df[, c("year", "indicator", "iso",
                                                     "annual_change_2.5%", "annual_change_10%",
                                                     "annual_change_50%", "annual_change_90%",
                                                     "annual_change_97.5%")],
                         by = c("iso", "indicator", "year"))

    ## Country info

    wra_all_res_df <- dplyr::left_join(wra_all_res_df, iso_all)

    ## Save

    save(wra_all_res_df, file = filepaths_outputs$wra_all_res_filepath)

    ## Tables / CSV Files
    ## THESE end up corrupt and unloadable, so don't do
    ## writexl::write_xlsx(wra_all_res_df, path = filepaths_outputs$wra_all_results_tables_filepath)



    ## -------* Married/In-Union Women (MWRA)

    message("\n(2 of 2) .. Married women (MWRA)")

    ## -------** Create Stall Probabilities

    message("\n  (1 of 3) .. Creating stall probabilities")

    ## Extract and summarize posterior trajectories

    cl <- parallel::makeCluster(min(ncores, max(1, floor(length(country_isos_to_process) / 4))))
    parallel::clusterExport(cl, varlist = c("make_stall_prob_df", "lm_local_arr", "filepaths_inputs",
                                  "smooth_type",
                                  "denominator_count_filename",
                                  "change_condition", ".testing"),
                            envir = environment())
    stall_prob_mwra_df <-
        pbapply::pblapply(country_isos_to_process,
                 function(z) {
            library(magrittr)
            library(FPEMglobal.aux)
            make_stall_prob_df(z, run_name = filepaths_inputs$FPEM_results_subdir_names$rn_mwra,
                               output_dir = filepaths_inputs$res_dir_mwra, add_iso_column = TRUE,
                               smooth_type = smooth_type,
                               differences = 1,
                               change_condition = change_condition,
                               filter_width = 3,
                               denominator_count_filename = denominator_count_filename,
                               .testing = .testing)
        },
        cl = cl)
    parallel::stopCluster(cl = cl)

    stall_prob_mwra_df <- do.call("rbind", stall_prob_mwra_df)

    stall_prob_mwra_df <-
        within(stall_prob_mwra_df,
               in_time_window <- year >= min(year_lim) & year <= max(year_lim))

    ## Add Single-year Stall Indicators

    stall_prob_mwra_df <-
        add_stall_indicators_probabilities(stall_prob_mwra_df,
                                           stall_probability_thresholds = stall_probability_thresholds,
                                           CP_range_condition_min = CP_range_condition_min,
                                           CP_range_condition_max = CP_range_condition_max,
                                           MDMM_range_condition_min = MDMM_range_condition_min,
                                           MDMM_range_condition_max = MDMM_range_condition_max)

    ## Add stall length info

    message("\n  (2 of 3) .. Calculating stall lengths")

    stall_prob_mwra_df <-
        add_stall_lengths(stall_prob_mwra_df,
                          min_stall_length = min_stall_length,
                          stall_probability_thresholds = attr(stall_prob_mwra_df,
                                                              "stall_probability_thresholds"),
                          cores = ncores)

    ## Add level condition indicator

    stall_prob_mwra_df <-
        add_level_condition_indicators(stall_prob_mwra_df,
                                       Level_condition_variant = Level_condition_variant)

    ## Add Schoumaker's TFR stalls

    stall_prob_mwra_df <- add_schoumaker_tfr_stalls(stall_prob_mwra_df, iso_all = iso_all)


    ## -------** Create Annual Change Data

    message("\n  (3 of 3) .. Creating annual change data")

    ## Extract and summarize posterior trajectories

    cl <- parallel::makeCluster(min(ncores, max(1, floor(length(country_isos_to_process) / 4))))
    parallel::clusterExport(cl, varlist = c("make_q_diff_df", "filepaths_inputs",
                                            "denominator_count_filename", ".testing"),
                            envir = environment())
    annual_cp_change_mwra_df <-
        pbapply::pblapply(country_isos_to_process,
                 function(z) {
            library(magrittr)
            library(FPEMglobal.aux)
            make_q_diff_df(z, run_name = filepaths_inputs$FPEM_results_subdir_names$rn_mwra,
                           output_dir = filepaths_inputs$res_dir_mwra, add_iso_column = TRUE,
                           differences = 1,
                           denominator_count_filename = denominator_count_filename,
                           .testing = .testing)
        },
        cl = cl)
    parallel::stopCluster(cl = cl)

    annual_cp_change_mwra_df <- do.call("rbind", annual_cp_change_mwra_df)


    ## -------** Merge and Save

    message("\nSaving results")

    ## Merge annual changes and probabilities

    mwra_all_res_df <-
        dplyr::left_join(stall_prob_mwra_df,
                         annual_cp_change_mwra_df[, c("year", "indicator", "iso",
                                                      "annual_change_2.5%", "annual_change_10%",
                                                      "annual_change_50%", "annual_change_90%",
                                                      "annual_change_97.5%")],
                         by = c("iso", "indicator", "year"))

    ## Country info

    mwra_all_res_df <- dplyr::left_join(mwra_all_res_df, iso_all)

    ## Save

    save(mwra_all_res_df, file = filepaths_outputs$mwra_all_res_filepath)

    ## Tables / CSV Files
    ## THESE end up corrupt and unloadable, so don't do
    ## writexl::write_xlsx(mwra_all_res_df, path = filepaths_outputs$mwra_all_results_tables_filepath)


    ## -------* Return

    ## Return output diretory
    return(filepaths_outputs$results_output_dir)

}


################################################################################


##' @export
make_all_plots <- function(results_output_dir,
                           project_dir = ".",
                           use_ggpattern = TRUE,
                           ncores = parallelly::availableCores(omit = 1),
                           .testing = FALSE) {

    message("\n\n\nMaking all plots")

    ## -------* Check Arguments

    parse_results_output_dir <- parse_results_output_dir(results_output_dir)

    min_stall_length <- check_min_stall_length(parse_results_output_dir$min_stall_length, smoothing_method)
    change_condition_percent <- check_change_condition_percent(parse_results_output_dir$change_condition_percent)


    ## -------* Constants

    filepaths_outputs <- make_filepaths_outputs(project_dir = project_dir,
                                                results_dir_name = parse_results_output_dir$results_dir_name,
                                                datestamp = parse_results_output_dir$datestamp,
                                                smoothing_method = parse_results_output_dir$smoothing_method,
                                                min_stall_length = parse_results_output_dir$min_stall_length,
                                                change_condition_percent = parse_results_output_dir$change_condition_percent)

    iso_all <- read_iso_all_from_results(results_output_dir)


    ## -------* Plots

    ## -------** Function to Parallelize Over

    ## This function makes all the plots -- all relevant code is
    ## here. It will be passed to 'lapply' below.

    plot_in_parallel <- function(PLOT_NUMBER, mar_group, use_ggpattern) {

        if (identical(PLOT_NUMBER, 1L)) {

            ## All FP Indicators Plots

            for (prob in stall_probability_thresholds) {
                for (yvar in c("stall_prob", "annual_change_50%")) {

                    fname <- switch(yvar,
                                    stall_prob = "all_indicators_plateau_probabilities.pdf",
                                    `annual_change_50%` = "all_indicators_annual_changes.pdf")

                    pdf(file = file.path(filepaths_outputs$results_output_plots_dir, mar_group,
                                         paste0("stall_prob_", prob * 100), fname),
                        width = 14, height = 12)
                    for(i in intersect(iso_all$iso, get(paste0(mar_group, "_all_res_df"))$iso)) {
                        plot_df <- dplyr::filter(get(paste0(mar_group, "_all_res_df")),
                                                 indicator %in% c("Met Demand", "MetDemModMeth",
                                                                  "Modern", "Traditional", "Total", "Unmet") &
                                                 iso == i)
                        if (nrow(plot_df)) {
                            gp <- stall_plot(plot_df, iso_all = iso_all,
                                             yvar = yvar,
                                             min_stall_length = attr(plot_df, "min_stall_length"),
                                             CP_range_condition_min = attr(plot_df, "CP_range_condition_min"),
                                             CP_range_condition_max = attr(plot_df, "CP_range_condition_max"),
                                             MDMM_range_condition_min = attr(plot_df, "MDMM_range_condition_min"),
                                             MDMM_range_condition_max = attr(plot_df, "MDMM_range_condition_max"),
                                             CP_abbrev = "FP Indicator",
                                             stall_probability_threshold = prob,
                                             use_ggpattern = use_ggpattern) +
                                ggplot2::labs(subtitle = paste0("Criterion: plateau probability exceeds ", prob * 100, "%"))
                            print(gp)
                        }
                    }
                    dev.off()
                }
            }

            message("'All FP Indicators' plots finished.")

        } else if (identical(PLOT_NUMBER, 2L)) {

            ## Just One Indicator All Countries

            for (prob in stall_probability_thresholds) {
                for (yvar in c("stall_prob", "annual_change_50%")) {
                    for (indicator in c("MetDemModMeth", "CP_Modern")) {

                        fname <- switch(yvar,
                                        stall_prob = paste0(indicator, "_plateau_probabilities.pdf"),
                                        `annual_change_50%` = paste0(indicator, "_annual_changes.pdf"))
                        indicator_abbrev <- switch(indicator,
                                                   MetDemModMeth = "Met Dem.\nMod. Meth.",
                                                   CP_Modern = "CP Modern")
                        indicator_range_abbrev <- switch(indicator,
                                                         MetDemModMeth = "MDMM",
                                                         CP_Modern = "MCP")
                        indicator_value <- switch(indicator,
                                                  MetDemModMeth = "MetDemModMeth",
                                                  CP_Modern = "Modern")

                        pl <- lapply(unique(get(paste0(mar_group, "_all_res_df"))$iso), function(z) {
                            plot_df <- dplyr::filter(get(paste0(mar_group, "_all_res_df")),
                                                     iso == z & indicator == indicator_value)
                            stall_plot(plot_df, iso_all = iso_all,
                                       CP_abbrev = indicator_abbrev,
                                       CP_not_in_range_abbrev = indicator_range_abbrev,
                                       facet_by_indicator = TRUE,
                                       yvar = yvar,
                                       min_stall_length = attr(plot_df, "min_stall_length"),
                                       CP_range_condition_min = attr(plot_df, "CP_range_condition_min"),
                                       CP_range_condition_max = attr(plot_df, "CP_range_condition_max"),
                                       MDMM_range_condition_min = attr(plot_df, "MDMM_range_condition_min"),
                                       MDMM_range_condition_max = attr(plot_df, "MDMM_range_condition_max"),
                                       stall_probability_threshold = prob,
                                       use_ggpattern = use_ggpattern) +
                                theme(text = element_text(size=8)) +
                                labs(subtitle = paste0("Criterion: plateau probability exceeds ", prob * 100, "%"))
                        })
                        op_device <- getOption("device"); options(device = pdf); dev.new()
                        ml <- gridExtra::marrangeGrob(pl, nrow = 3, ncol = 2, top = "")
                        dev.off(); file.remove("Rplots.pdf"); options(device = op_device)
                        ggplot2::ggsave(filename = file.path(filepaths_outputs$results_output_plots_dir, mar_group,
                                                             paste0("stall_prob_", prob * 100),
                                                             fname),
                                        ml,
                                        width = 8.5, height = 11)
                    }
                }
            }

            message("'Just One Indicator All Countries' plots finished.")

        } else if (identical(PLOT_NUMBER, 3L)) {

            ## SSA, only countries with stalls (CP or fertility)

            ssa_isos <- iso_all |>
                dplyr::filter(sub_saharanafrica == "Yes") |>
                dplyr::arrange(region)
            ssa_isos <- intersect(ssa_isos$iso, get(paste0(mar_group, "_all_res_df"))$iso)

            for (prob in stall_probability_thresholds) {
                for (yvar in c("stall_prob", "annual_change_50%")) {
                    for (indicator in c("MetDemModMeth", "CP_Modern")) {

                        fname <- switch(yvar,
                                        stall_prob = paste0(indicator, "_plateau_probabilities_ssa.pdf"),
                                        `annual_change_50%` = paste0(indicator, "_annual_changes_ssa.pdf"))
                        indicator_abbrev <- switch(indicator,
                                                   MetDemModMeth = "Met Dem.\nMod. Meth.",
                                                   CP_Modern = "CP Modern")
                        indicator_range_abbrev <- switch(indicator,
                                                         MetDemModMeth = "MDMM",
                                                         CP_Modern = "MCP")
                        indicator_value <- switch(indicator,
                                                  MetDemModMeth = "MetDemModMeth",
                                                  CP_Modern = "Modern")

                        pl <- lapply(ssa_isos, function(z) {
                            plot_df <- dplyr::filter(get(paste0(mar_group, "_all_res_df")),
                                                     iso == z & indicator == indicator_value)
                            stall_plot(plot_df, iso_all = iso_all,
                                       CP_abbrev = indicator_abbrev,
                                       CP_not_in_range_abbrev = indicator_range_abbrev,
                                       facet_by_indicator = TRUE,
                                       yvar = yvar,
                                       min_stall_length = attr(plot_df, "min_stall_length"),
                                       CP_range_condition_min = attr(plot_df, "CP_range_condition_min"),
                                       CP_range_condition_max = attr(plot_df, "CP_range_condition_max"),
                                       MDMM_range_condition_min = attr(plot_df, "MDMM_range_condition_min"),
                                       MDMM_range_condition_max = attr(plot_df, "MDMM_range_condition_max"),
                                       stall_probability_threshold = prob,
                                       use_ggpattern = use_ggpattern) +
                                ggplot2::theme(text = element_text(size=8)) +
                                ggplot2::labs(subtitle = paste0("Criterion: plateau probability exceeds ", prob * 100, "%"))
                        })
                        op_device <- getOption("device"); options(device = pdf); dev.new()
                        ml <- gridExtra::marrangeGrob(pl, nrow = 3, ncol = 2, top = "")
                        dev.off(); file.remove("Rplots.pdf"); options(device = op_device)
                        ggplot2::ggsave(filename = file.path(filepaths_outputs$results_output_plots_dir,
                                                             mar_group, paste0("stall_prob_", prob * 100),
                                                             fname),
                                        ml,
                                        width = 8.5, height = 11)
                    }
                }
            }

            message("'SSA Countries with cp or Fertility Stalls' plots finished.")

        } else if (identical(PLOT_NUMBER, 4L)) {

            ## Hybrid Stalls / Indicators Plots (Country 'Profiles')

            message("\nCountry profile plots")

            for (prob in stall_probability_thresholds) {
                pdf(file = file.path(filepaths_outputs$results_output_plots_dir, mar_group,
                                     paste0("stall_prob_", prob * 100), "country_profiles_all.pdf"),
                    width = 14, height = 10)
                for(i in unique(get(paste0(mar_group, "_all_res_df"))$iso)) {
                    plot_df <- dplyr::filter(get(paste0(mar_group, "_all_res_df")), iso == i)
                    print(country_profile_plot(plot_df, iso_all = iso_all,
                                               stall_probability_threshold = prob,
                                               use_ggpattern = use_ggpattern))
                }
                dev.off()
            }

            message("'Country Profile' plots finished.")

        } else if (identical(PLOT_NUMBER, 5L)) {

            ## Hybrid Stalls / Indicators Plots (Country 'Profiles'), SSA Only

            ssa_isos <- iso_all |>
                dplyr::filter(sub_saharanafrica == "Yes") |>
                dplyr::arrange(region)
            ssa_isos <- intersect(ssa_isos$iso, get(paste0(mar_group, "_all_res_df"))$iso)

            for (prob in stall_probability_thresholds) {
                pdf(file = file.path(filepaths_outputs$results_output_plots_dir, mar_group,
                                     paste0("stall_prob_", prob * 100), "country_profiles_all_ssa.pdf"),
                    width = 14, height = 10)
                for(i in ssa_isos) {
                    plot_df <- dplyr::filter(get(paste0(mar_group, "_all_res_df")), iso == i)
                    print(country_profile_plot(plot_df, iso_all = iso_all,
                                               stall_probability_threshold = prob,
                                               use_ggpattern = use_ggpattern))
                }
                dev.off()
            }
            message("'Country Profile, SSA Only' plots finished.")
        }
    }

    ## -------** Make the Plots

    mar_groups <- c("All women (WRA)" = "wra", "Married women (MWRA)" = "mwra")

    for (i in seq_along(mar_groups)) {

        this_mar_group <- mar_groups[i]

        message("\n\n(", i, " of ", length(mar_groups), ") .. ", names(mar_groups)[i])

        ## -------*** Inputs

        this_use_ggpattern <- use_ggpattern

        res_filepath <- filepaths_outputs[[paste0(this_mar_group, "_all_res_filepath")]]
        res_obj_name <- load(res_filepath)

        stall_probability_thresholds <-
            attr(get(res_obj_name), "stall_probability_thresholds")

        if (isTRUE(.testing)) stall_probability_thresholds <- stall_probability_thresholds[1]

        for (x in file.path(filepaths_outputs$results_output_plots_dir, this_mar_group,
                            paste0("stall_prob_",
                                   as.numeric(stall_probability_thresholds) * 100))) {
            ensure_new_dir(x)
        }

        ## -------*** Plots

        nplots <- 5L # MUST match the actual number of plots in
                     # 'plot_in_parallel()'.

        if (is.null(ncores)) {
            for (i in seq_len(nplots)) {
                plot_in_parallel(i, mar_group = this_mar_group, use_ggpattern = this_use_ggpattern)
            }
        } else {
            cl <- parallel::makeCluster(min(ncores, nplots))
            parallel::clusterExport(cl, varlist = c("this_mar_group", "stall_probability_thresholds",
                                                    "filepaths_outputs", "iso_all", "use_ggpattern"),
                                    envir = environment())

            dump <-
                pbapply::pblapply(X = 1L:nplots,
                                  FUN = "plot_in_parallel",
                                  mar_group = this_mar_group, use_ggpattern = this_use_ggpattern,
                                  cl = cl)
            parallel::stopCluster(cl = cl)
        }
    }
}

