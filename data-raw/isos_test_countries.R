################################################################################
###
### ISOs of countries to use in package testing
###
################################################################################

isos_test_countries <- c(566, 270, 716, 124)
                                #c("Nigeria", "Gambia", "Zimbabwe", "Canada")

usethis::use_data(isos_test_countries, overwrite = TRUE)
