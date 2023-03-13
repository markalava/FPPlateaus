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
                             marital_group = c("wra", "mwra"),
                             smoothing_method = c("annual_difference", "moving_average", "local_linear"),
                             min_stall_length = NULL,
                             change_condition_percent,
                             stall_probability_thresholds = c(0.5, 0.75, 0.8, 0.9, 0.95),
                             CP_range_condition_min = 0.1,
                             CP_range_condition_max = 0.6,
                             MDMM_range_condition_min = 0.2,
                             MDMM_range_condition_max = 0.8,
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

    message("\nMaking all results")

    ## -------* Check Arguments

    marital_group <- match.arg(marital_group)
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


    ## -------* By Marital Group

    for (this_mar in c("wra", "mwra")) {

        if (identical(this_mar, "wra")) {
            this_message_txt <- "(1 of 2) .. All women (WRA)"
            this_run_name <- filepaths_inputs$FPEM_results_subdir_names$rn_wra
            this_output_dir <- filepaths_inputs$res_dir_wra
            this_all_res_filepath <- filepaths_outputs$wra_all_res_filepath
        } else {
            this_message_txt <- "(2 of 2) .. Married women (MWRA)"
            this_run_name <- filepaths_inputs$FPEM_results_subdir_names$rn_mwra
            this_output_dir <- filepaths_inputs$res_dir_mwra
            this_all_res_filepath <- filepaths_outputs$mwra_all_res_filepath
        }

        message("\n", this_message_txt)

        ## -------** Create Stall Probabilities

        message("  (1 of 3) .. Creating stall probabilities")

        ## Extract and summarize posterior trajectories

        if (!is.null(ncores)) {
            cl <- parallel::makeCluster(min(ncores, length(country_isos_to_process)))
            parallel::clusterExport(cl, varlist = c("make_stall_prob_df", "lm_local_arr", "filepaths_inputs",
                                                    "this_run_name", "this_output_dir",
                                                    "smooth_type", "smoothing_method", "min_stall_length",
                                                    "denominator_count_filename",
                                                    "change_condition", ".testing"),
                                    envir = environment())
        } else cl <- NULL # cl NULL means process in serial
        stall_prob_df <-
            pbapply::pblapply(country_isos_to_process,
                              function(z) {
                         make_stall_prob_df(z, run_name = this_run_name,
                                            output_dir = this_output_dir,
                                            add_iso_column = TRUE,
                                            smooth_type = smooth_type,
                                            differences = 1, # the order of the differences, hardcoded
                                            change_condition = change_condition,
                                            filter_width = 3, # the width of the window for moving average or ols, hardcoded
                                            denominator_count_filename = denominator_count_filename,
                                            .testing = .testing)
                     },
                     cl = cl)
        if (!is.null(ncores)) parallel::stopCluster(cl = cl)

        stall_prob_df <- do.call("rbind", stall_prob_df)

        stall_prob_df$in_time_window <-
            stall_prob_df$year >= min(year_lim) & stall_prob_df$year <= max(year_lim)

        ## Add level condition indicator

        stall_prob_df <-
            add_level_condition_indicators(stall_prob_df,
                                           Level_condition_variant = Level_condition_variant,
                                           CP_range_condition_min = CP_range_condition_min,
                                           CP_range_condition_max = CP_range_condition_max,
                                           MDMM_range_condition_min = MDMM_range_condition_min,
                                           MDMM_range_condition_max = MDMM_range_condition_max)

        ## Add Single-year Stall Indicators

        stall_prob_df <-
            add_stall_indicators_probabilities(stall_prob_df,
                                               stall_probability_thresholds = stall_probability_thresholds)

        ## Add stall length info

        message("  (2 of 3) .. Calculating plateau lengths")

    ######## NOTE !!!!!!!!!!!!!!!!!!!!!!!!!!
    ########
    ######## `add_plateau_lengths()` calculates lengths of plateau
    ######## periods, but does *not* take any account of the level
    ######## condition. Plateaus start as soon as the plateau
    ######## probability exceeds the threshold, regardless of the
    ######## level condition.

        stall_prob_df <-
            add_plateau_lengths(stall_prob_df,
                              min_stall_length = min_stall_length,
                              stall_probability_thresholds = attr(stall_prob_df,
                                                                  "stall_probability_thresholds"),
                              cores = ncores)

        ## Add Schoumaker's TFR stalls

        stall_prob_df <- add_schoumaker_tfr_stalls(stall_prob_df, iso_all = iso_all)


        ## -------** Create 'Annual Change' Data

        message("  (3 of 3) .. Creating annual change data")

        ## Extract and summarize posterior trajectories

        if (!is.null(ncores)) {
            cl <- parallel::makeCluster(min(ncores, length(country_isos_to_process)))
            parallel::clusterExport(cl, varlist = c("make_q_diff_df", "filepaths_inputs",
                                                    "denominator_count_filename", ".testing"),
                                    envir = environment())
        } else cl <- NULL
        annual_cp_change_df <-
            pbapply::pblapply(country_isos_to_process,
                              function(z) {
                         make_q_diff_df(z,
                                        run_name = this_run_name,
                                        output_dir = this_output_dir,
                                        add_iso_column = TRUE,
                                        differences = 1,
                                        denominator_count_filename = denominator_count_filename,
                                        .testing = .testing)
                     },
                     cl = cl)
        if (!is.null(ncores)) parallel::stopCluster(cl = cl)

        annual_cp_change_df <- do.call("rbind", annual_cp_change_df)


        ## -------** Merge and Save

        ## Merge annual changes and probabilities
        all_res_df <-
            dplyr::left_join(stall_prob_df,
                             annual_cp_change_df[, c("year", "indicator", "iso",
                                                     "annual_change_2.5%", "annual_change_10%",
                                                     "annual_change_50%", "annual_change_90%",
                                                     "annual_change_97.5%")],
                             by = c("iso", "indicator", "year"))

        ## Country info
        all_res_df <- dplyr::left_join(all_res_df, iso_all, by = "iso")

        ## Class
        attr(all_res_df, "marital_group") <- marital_group
        attr(all_res_df, "FP_plateau_types") <- NA_character_
        all_res_df <- as_fpplateaus_data_frame(all_res_df)

        ## Internal check
        for (n in get_fpplateaus_attr_names()) {
            if (!length(attr(all_res_df, n)))
                stop("Attribute '", n, "' has not been set; something is wrong.")
        }

        ## Save
        assign(paste0(this_mar, "_all_res_df"), all_res_df)
        save(list = paste0(this_mar, "_all_res_df"), file = this_all_res_filepath)

        ## Tables / CSV Files
        ## THESE end up corrupt and unloadable, so don't do
        ## writexl::write_xlsx(wra_all_res_df, path = filepaths_outputs$wra_all_results_tables_filepath)

    }

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

    message("\nMaking all plots")

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

            message("  'All FP Indicators' plots.")

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
                            gp <- stall_plot(plot_df,
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

        } else if (identical(PLOT_NUMBER, 2L)) {

            ## Just One Indicator All Countries

            message("  'Just One Indicator All Countries' plots.")

            for (prob in stall_probability_thresholds) {
                for (yvar in c("stall_prob", "annual_change_50%")) {
                    for (indicator in c("MetDemModMeth", "CP_Modern")) {
                        fname <- switch(yvar,
                                        stall_prob = paste0(indicator, "_plateau_probabilities.pdf"),
                                        `annual_change_50%` = paste0(indicator, "_annual_changes.pdf"))
                        indicator_abbrev <- switch(indicator,
                                                   MetDemModMeth = "NSMM",
                                                   CP_Modern = "MCP")
                        indicator_range_abbrev <- switch(indicator,
                                                         MetDemModMeth = "MDMM",
                                                         CP_Modern = "MCP")
                        indicator_value <- switch(indicator,
                                                  MetDemModMeth = "MetDemModMeth",
                                                  CP_Modern = "Modern")
                        pl <- lapply(unique(get(paste0(mar_group, "_all_res_df"))$iso), function(z) {
                            plot_df <- dplyr::filter(get(paste0(mar_group, "_all_res_df")),
                                                     iso == z & indicator == indicator_value)
                            stall_plot(plot_df,
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
                        ## OR:
                        ## temp_filename <- tempfile(pattern = "temp_plot_", tmpdir = ".", fileext = ".pdf")
                        ## dev(file = temp_filename)
                        ## ...
                        ## file.remove(temp_filename)
                        owd <- setwd(tempdir())
                        op_device <- getOption("device")
                        options(device = pdf); suppressMessages(dev.new())
                        ml <- gridExtra::marrangeGrob(pl, nrow = 3, ncol = 2, top = "")
                        dev.off(); options(device = op_device)
                        setwd(owd)
                        ggplot2::ggsave(filename = file.path(filepaths_outputs$results_output_plots_dir, mar_group,
                                                             paste0("stall_prob_", prob * 100),
                                                             fname),
                                        ml,
                                        width = 8.5, height = 11)
                    }
                }
            }

        } else if (identical(PLOT_NUMBER, 3L)) {

            ## SSA, only countries with stalls (CP or fertility)

            message("  'SSA Countries with cp or Fertility Stalls' plots.")

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
                                                   MetDemModMeth = "NSMM",
                                                   CP_Modern = "MCP")
                        indicator_range_abbrev <- switch(indicator,
                                                         MetDemModMeth = "MDMM",
                                                         CP_Modern = "MCP")
                        indicator_value <- switch(indicator,
                                                  MetDemModMeth = "MetDemModMeth",
                                                  CP_Modern = "Modern")

                        pl <- lapply(ssa_isos, function(z) {
                            plot_df <- dplyr::filter(get(paste0(mar_group, "_all_res_df")),
                                                     iso == z & indicator == indicator_value)
                            stall_plot(plot_df,
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
                        owd <- setwd(tempdir())
                        op_device <- getOption("device");
                        options(device = pdf); suppressMessages(dev.new())
                        ml <- gridExtra::marrangeGrob(pl, nrow = 3, ncol = 2, top = "")
                        dev.off(); options(device = op_device)
                        setwd(owd)
                        ggplot2::ggsave(filename = file.path(filepaths_outputs$results_output_plots_dir,
                                                             mar_group, paste0("stall_prob_", prob * 100),
                                                             fname),
                                        ml,
                                        width = 8.5, height = 11)
                    }
                }
            }

        } else if (identical(PLOT_NUMBER, 4L)) {

            ## Hybrid Stalls / Indicators Plots (Country 'Profiles')

            message("  'Country Profile' plots.")

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

        } else if (identical(PLOT_NUMBER, 5L)) {

            ## Hybrid Stalls / Indicators Plots (Country 'Profiles'), SSA Only

            message("  'Country Profile, SSA Only' plots.")

            ssa_isos <- iso_all |>
                dplyr::filter(sub_saharanafrica == "Yes") |>
                dplyr::arrange(region)
            ssa_isos <- intersect(ssa_isos$iso, get(paste0(mar_group, "_all_res_df"))$iso)

            for (prob in stall_probability_thresholds) {
                pdf(file = file.path(filepaths_outputs$results_output_plots_dir, mar_group,
                                     paste0("stall_prob_", prob * 100), "country_profiles_ssa.pdf"),
                    width = 14, height = 10)
                for(i in ssa_isos) {
                    plot_df <- dplyr::filter(get(paste0(mar_group, "_all_res_df")), iso == i)
                    print(country_profile_plot(plot_df, iso_all = iso_all,
                                               stall_probability_threshold = prob,
                                               use_ggpattern = use_ggpattern))
                }
                dev.off()
            }
        }
    }

    ## -------** Make the Plots

    mar_groups <- c("All women (WRA)" = "wra", "Married women (MWRA)" = "mwra")

    for (i in seq_along(mar_groups)) {

        this_mar_group <- mar_groups[i]

        message("\n(", i, " of ", length(mar_groups), ") .. ", names(mar_groups)[i])

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

