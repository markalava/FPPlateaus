################################################################################
###
### DATE CREATED: 2023-01-25
###
### AUTHOR: Mark Wheldon
###
### PROJECT: Making Family Planning Count: FP Plateaus
###
### DESCRIPTION: Main file that runs plateau identification, generates
### results files and plots.
###
################################################################################

library(FPPlateaus)

###-----------------------------------------------------------------------------
### * Directories, Filepaths

stopifnot(dir.exists(
    FPEM_results_dir <- file.path(stop("SPECIFY location of FPEM results"))))

mar_dir_name_wra <- "15-49_wra"
mar_dir_name_mwra <- "15-49_mwra"
mar_dir_name_uwra <- "15-49_uwra"

stopifnot(dir.exists(
    FPEM_results_wra_dir <- file.path(FPEM_results_dir, mar_dir_name_wra)))
stopifnot(dir.exists(
    FPEM_results_mwra_dir <- file.path(FPEM_results_dir, mar_dir_name_mwra)))
stopifnot(dir.exists(
    FPEM_results_uwra_dir <- file.path(FPEM_results_dir, mar_dir_name_uwra)))

denominator_count_filename <- "number_of_women_15-49_20220608.csv"


###-----------------------------------------------------------------------------
### * Runs

for (this_smooth in c("local_linear", "annual_difference", "moving_average")) {
    message("\n\n\n\n======================================================================\n",
            " ", toupper(this_smooth),
            "\n======================================================================\n")

    for (this_change_pc in c(0.5, 0.3, 0.1)) {
        message("\n\n\n----------------------------------------------------------------------\n",
                " ", toupper(this_change_pc), " percent threshold",
                "\n----------------------------------------------------------------------\n")
        results_output_dir <-
            make_all_results(smoothing_method = this_smooth,
                             change_condition_percent = this_change_pc,
                             results_dir_name = "results_v2_new_level_cond",
                                 FPEM_results_dir = FPEM_results_dir,
                                 FPEM_results_subdir_names = list(wra = mar_dir_name_wra,
                                                                  mwra = mar_dir_name_mwra,
                                                                  uwra = mar_dir_name_uwra),
                             denominator_count_filename = denominator_count_filename,
                             Level_condition_variant = "v2 - SDG Only")
        make_all_plots(results_output_dir, use_ggpattern = FALSE)
    }
}
