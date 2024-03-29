################################################################################
###
### __All Women__ trajectories for TESTING ONLY
###
### This is *not* a random sample and is *not* representative of the
### whole trajectory set.
###
################################################################################

###-----------------------------------------------------------------------------
### * Paths

stopifnot(dir.exists(
    S0_FPEM_results_dir <- file.path(Sys.getenv("SHAREPOINT_PDU_FPEM_RESULTS"), "Released", "2022")))

S0_mar_dir_name_wra <- "15-49_wra"

stopifnot(dir.exists(
    S0_FPEM_results_wra_dir <- file.path(S0_FPEM_results_dir, S0_mar_dir_name_wra)))

###-----------------------------------------------------------------------------
### * Constants

## Countries to use in testing.
source(here::here("data-raw", "isos_test_countries.R"))

###-----------------------------------------------------------------------------
### * Trajectories

## Load
sample_trajectories_all_women <- lapply(setNames(isos_test_countries, isos_test_countries),
                 function(z) {
    tra <- FPEMglobal.aux::get_country_traj_aw(output_dir = S0_FPEM_results_wra_dir,
                                               iso_code = z)
    ## Crop
    return(tra[,,1:10])
    })

###-----------------------------------------------------------------------------
### * Save

usethis::use_data(sample_trajectories_all_women, overwrite = TRUE)
