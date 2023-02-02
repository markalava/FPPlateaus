################################################################################
###
### LOCAL ONLY:
###
### This file contains hard-coded file paths and directory names;
### don't include as part of the package build.
###
################################################################################


test_that("all outputs can be created without errors (short run, local only)", {
    ##-----------------------------------------------------------------------------
    ## * Directories, Filepaths

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

    ##-----------------------------------------------------------------------------
    ## * Constants

    ## Countries to use in testing.
    ##
    ## *NOTE*: This is hard-coded in other places, including other
    ## data-raw files and tests/testthat files. Do *not* change here
    ## without checking other places.

    isos_test_countries <- c(566, 270, 716)
                                #c("Nigeria", "Gambia", "Zimbabwe")

    ##-----------------------------------------------------------------------------
    ## * Runs

    ## RUN IN TEMP DIR
    withr::local_dir(tempdir())

    for (this_smooth in c("annual_difference", "moving_average", "local_linear")) {
        message("\n\n\n\n======================================================================\n",
                " ", toupper(this_smooth),
                "\n======================================================================\n")

        for (this_change_pc in c(0.5)) { #only one change condition for testing
            message("\n\n\n----------------------------------------------------------------------\n",
                    " ", toupper(this_change_pc), " percent threshold",
                    "\n----------------------------------------------------------------------\n")
            results_output_dir <-
                expect_error(
                    make_all_results(country_isos_to_process = isos_test_countries,
                                     smoothing_method = this_smooth,
                                     change_condition_percent = this_change_pc,
                                     results_dir_name = "results_TEST",
                                     FPEM_results_dir = S0_FPEM_results_dir,
                                     FPEM_results_subdir_names = list(wra = S0_mar_dir_name_wra,
                                                                      mwra = S0_mar_dir_name_mwra,
                                                                      uwra = S0_mar_dir_name_uwra),
                                     denominator_count_filename = S0_denominator_count_filename,
                                     .testing = TRUE,
                                     Level_condition_variant = "v1 - SDG Only"),
                    NA)
        }
    }
    ## Just test plots on one of the smoothing methods
    expect_error(make_all_plots(results_output_dir, .testing = TRUE), NA)
})

