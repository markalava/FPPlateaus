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
    S0_FPEM_results_dir <- file.path(Sys.getenv("SHAREPOINT_PDU_FPEM_RESULTS"), "2022")))

S0_mar_dir_name_wra <- "15-49_wra"
S0_mar_dir_name_mwra <- "15-49_mwra"
S0_mar_dir_name_uwra <- "15-49_uwra"

stopifnot(dir.exists(
    S0_FPEM_results_wra_dir <- file.path(S0_FPEM_results_dir, S0_mar_dir_name_wra)))
stopifnot(dir.exists(
    S0_FPEM_results_mwra_dir <- file.path(S0_FPEM_results_dir, S0_mar_dir_name_mwra)))
stopifnot(dir.exists(
    S0_FPEM_results_uwra_dir <- file.path(S0_FPEM_results_dir, S0_mar_dir_name_uwra)))

S0_denominator_count_filename <- "number_of_women_15-49_20220608.csv"


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
                             results_dir_name = "results_v2_new_level_cond", # Once in, forever in.
                                 FPEM_results_dir = S0_FPEM_results_dir,
                                 FPEM_results_subdir_names = list(wra = S0_mar_dir_name_wra,
                                                                  mwra = S0_mar_dir_name_mwra,
                                                                  uwra = S0_mar_dir_name_uwra),
                             denominator_count_filename = S0_denominator_count_filename,
                             Level_condition_variant = "v2 - SDG Only")
        make_all_plots(results_output_dir, use_ggpattern = FALSE)
    }
}
