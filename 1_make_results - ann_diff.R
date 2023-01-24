################################################################################
###
### DATE CREATED: 2022-06-21
###
### AUTHOR: Mark Wheldon
###
### PROJECT: Making Family Planning Count: CP Stalls
###
### DESCRIPTION:
###
###     Create FP plateau results from FPEM output.
###
###-----------------------------------------------------------------------------
###
################################################################################

source("0_setup.R")

## If testing, can use this subset, otherwise don't specify
## test_countries <- unreg::code(c("Uganda", "Comoros", "Kenya", "South Africa"))

for (this_smooth in c("annual_difference"## , "moving_average", "local_linear"
                      )) {
    message("\n\n\n\n======================================================================\n",
            " ", toupper(this_smooth),
            "\n======================================================================")

    for (this_change_pc in c(0.1, 0.25, 0.3, ## 0.5, (already did this interactively)
                             1)) {
        message("\n\n\n----------------------------------------------------------------------\n",
                " ", toupper(this_change_pc), " percent threshold",
                "\n----------------------------------------------------------------------")
        results_output_dir <-
            make_all_results(country_isos_to_process = NULL,#test_countries,
                             smoothing_method = this_smooth,
                             change_condition_percent = this_change_pc,
                             min_stall_length = 1 #<<<<<<<<<<<<<<<<<<<<< FOR EXAMPLES
                             )

        make_all_plots(results_output_dir)
    }
}


