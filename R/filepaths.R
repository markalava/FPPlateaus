##' @export
make_filepaths_inputs <- function(FPEM_results_dir,
                                  FPEM_results_subdir_names,
                                  denominator_count_filename) {

    stopifnot(dir.exists(
        res_dir_wra <- file.path(FPEM_results_dir, FPEM_results_subdir_names$wra)))
    stopifnot(dir.exists(
        res_dir_mwra <- file.path(FPEM_results_dir, FPEM_results_subdir_names$mwra)))
    stopifnot(dir.exists(
        res_dir_uwra <- file.path(FPEM_results_dir, FPEM_results_subdir_names$uwra)))

    return(
        list(FPEM_results_dir = FPEM_results_dir,
             res_dir_wra = res_dir_wra,
             res_dir_mwra = res_dir_mwra,
             res_dir_uwra = res_dir_uwra,
             denominator_count_filename = denominator_count_filename,
             FPEM_results_subdir_names = FPEM_results_subdir_names))
}

##' @export
make_filepaths_outputs <- function(project_dir = ".",
                                   results_dir_name = "results",
                                   datestamp,
                                   smoothing_method = c("annual_difference",
                                                        "moving_average",
                                                        "local_linear"),
                                   min_stall_length = NULL,
                                   change_condition_percent) {

    ## -------* Check Arguments

    smoothing_method <- match.arg(smoothing_method)

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

    stopifnot(is.numeric(change_condition_percent))
    stopifnot(change_condition_percent >= 0 &&
              change_condition_percent <= 100)

    ## -------* Paths

    results_output_dir <-
                 ensure_new_dir(file.path(project_dir, results_dir_name,
                                          paste0("output_", datestamp,
                                                 "_", smoothing_method,
                                                 "_", min_stall_length, "_yr",
                                                 "_", change_condition_percent, "pc")))
    results_output_rda_dir <- ensure_new_dir(file.path(results_output_dir, "rda"))
    results_output_plots_dir <- ensure_new_dir(file.path(results_output_dir, "plots"))

    return(
        list(results_output_dir = results_output_dir,
             results_output_rda_dir = results_output_rda_dir,
             wra_all_res_filepath = file.path(results_output_rda_dir, "wra_all_res_df.rda"),
             mwra_all_res_filepath = file.path(results_output_rda_dir, "mwra_all_res_df.rda"),
             uwra_all_res_filepath = file.path(results_output_rda_dir, "uwra_all_res_df.rda"),
             results_output_plots_dir = results_output_plots_dir
             ))
}

##' @export
parse_results_output_dir <- function(results_output_dir) {
    spl_path <- strsplit(results_output_dir, "/")[[1]]
    spl_base <- strsplit(basename(results_output_dir), "_")[[1]]
    list(results_dir_name = spl_path[length(spl_path) - 1],
        datestamp = spl_base[2],
         smoothing_method = paste0(spl_base[3], "_", spl_base[4]),
         min_stall_length = as.numeric(spl_base[5]),
         change_condition_percent = as.numeric(gsub("pc", "", spl_base[7])))
    }
