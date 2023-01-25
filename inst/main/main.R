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

## `make_all_results()` will look for these object names by default.

if (nchar(Sys.getenv("SHAREPOINT_PATH_UN"))) {
    S0_sp_dir <- verify_dir(Sys.getenv("SHAREPOINT_PATH_UN"))
} else S0_sp_dir <- verify_dir(file.path(Sys.getenv("HOME"), "United Nations"))
S0_PDU_dir <- verify_dir(file.path(S0_sp_dir, "DESA-POP - PDU"))

stopifnot(dir.exists(
    S0_FPEM_results_dir <- file.path(S0_PDU_dir, "FPEM", "Results", "Released", "2022")))

S0_mar_dir_name_wra <- "15-49_wra"
S0_mar_dir_name_mwra <- "15-49_mwra"
S0_mar_dir_name_uwra <- "15-49_uwra"

stopifnot(dir.exists(
    S0_FPEM_results_wra_dir <- file.path(S0_FPEM_results_dir, S0_mar_dir_name_wra)))
stopifnot(dir.exists(
    S0_FPEM_results_mwra_dir <- file.path(S0_FPEM_results_dir, S0_mar_dir_name_mwra)))

S0_rn_wra <- "2022_15-49_wra"
S0_rn_mwra <- "2022_15-49_mwra"
S0_rn_uwra <- "2022_15-49_uwra"

S0_denominator_count_filename <- "number_of_women_15-49_20220608.csv"


###-----------------------------------------------------------------------------
### * Runs

for (this_smooth in c("annual_difference", "moving_average", "local_linear")) {
    message("\n\n\n\n======================================================================\n",
            " ", toupper(this_smooth),
            "\n======================================================================\n")

    for (this_change_pc in c(0.25, 0.5, 1)) {
        message("\n\n\n----------------------------------------------------------------------\n",
                " ", toupper(this_change_pc), " percent threshold",
                "\n----------------------------------------------------------------------\n")
        results_output_dir <-
            make_all_results(smoothing_method = this_smooth,
                             change_condition_percent = this_change_pc)

        make_all_plots(results_output_dir)
    }
}
