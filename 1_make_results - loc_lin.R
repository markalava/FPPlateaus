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

for (this_smooth in c(## "annual_difference", "moving_average",
                      "local_linear")) {
    for (this_change_pc in c(0.1, 0.3## 0.25, 0.5, 1
                             )) {
        results_output_dir <-
            make_all_results(country_isos_to_process = NULL,
                             smoothing_method = this_smooth,
                             change_condition_percent = this_change_pc)

        make_all_plots(results_output_dir)
    }
}
