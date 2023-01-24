################################################################################
###
### DATE CREATED: 2022-06-22
###
### AUTHOR: Mark Wheldon
###
### PROJECT: Making Family Planning Count: FP Plateaus
###
### DESCRIPTION:
###
###     Set up paths, functions, etc.
###     Use 2022 model run.
###
###-----------------------------------------------------------------------------
###
###  ** NOTE **
###
###  Any objects created here will have names beginning with "S0_" to
###  identify them throughout the rest of the code.
###
################################################################################

library(ggplot2)

###-----------------------------------------------------------------------------
### * Source Files

sourceDir <- function(path, trace = TRUE, ...) {
         op <- options(); on.exit(options(op)) # to reset after each
         for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
            if(trace) cat(nm,":")
            source(file.path(path, nm), ...)
            if(trace) cat("\n")
            options(op)
         }
      }

sourceDir("R") # Directory with all the scripts defining functions


###-----------------------------------------------------------------------------
### * Directories, Filepaths

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
