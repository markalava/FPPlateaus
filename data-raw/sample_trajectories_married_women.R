################################################################################
###
### __Married/In-union Women__ trajectories for TESTING ONLY
###
### This is *not* a random sample and is *not* representative of the
### whole trajectory set.
###
################################################################################

###-----------------------------------------------------------------------------
### * Paths

stopifnot(dir.exists(
    S0_FPEM_results_dir <- file.path(Sys.getenv("SHAREPOINT_PDU_FPEM_RESULTS"), "Released", "2022")))

S0_mar_dir_name_mwra <- "15-49_mwra"

stopifnot(dir.exists(
    S0_FPEM_results_mwra_dir <- file.path(S0_FPEM_results_dir, S0_mar_dir_name_mwra)))

###-----------------------------------------------------------------------------
### * Constants

## Countries to use in testing.
##
## *NOTE*: This is hard-coded in other places, including other
## data-raw files and tests/testthat files. Do *not* change here
## without checking other places.

isos_test_countries <- c(566, 270, 716)
                                #c("Nigeria", "Gambia", "Zimbabwe")

###-----------------------------------------------------------------------------
### * Trajectories

## Load
sample_trajectories_married_women <- lapply(setNames(isos_test_countries, isos_test_countries),
                 function(z) {
    tra <- FPEMglobal.aux::get_country_traj_muw(output_dir = S0_FPEM_results_mwra_dir,
                                               iso_code = z)
    ## Crop
    return(tra[,,1:10])
    })

###-----------------------------------------------------------------------------
### * Save

usethis::use_data(sample_trajectories_married_women, overwrite = TRUE)
