###-----------------------------------------------------------------------------
### * Directories

verify_dir <- function(path) {
    stopifnot(dir.exists(path))
    return(invisible(path))
}

soft_make_new_dir <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    return(invisible(path))
}

ensure_new_dir <- function(path) {
    return(verify_dir(soft_make_new_dir(path)))
}


###-----------------------------------------------------------------------------
### * Locations

### Regions

get_un_reg <- function(output_dir = S0_FPEM_results_mwra_dir,
                       run_name = S0_mar_dir_name_mwra) {
    FPEMglobal.aux::get_used_unpd_regions(run_name = run_name,
                                          output_dir = output_dir)
}

### ISO Codes for 195

get_iso_all <- function() {
    iso_all <- base::merge(get_un_reg(), FPEMglobal.aux::get_195_countries()[, "iso", drop = FALSE],
                       by = "iso",
                       all.y = TRUE, all.x = FALSE)
    iso_all$name[grep("^C.+te d.+Ivoire$", iso_all$name)] <- "Cote d'Ivoire"
    iso_all$name[grep("^R.+union$", iso_all$name)] <- "Reunion"
    return(iso_all)
}

get_iso <- function(name, iso_all = NULL) {
    if (is.null(iso_all)) iso_all <- get_iso_all()
    if (identical(length(name), 1L))
        return(iso_all[iso_all$name == name, "iso"])
    else
        return(iso_all[iso_all$name %in% name, "iso"])
}

get_name <- function(iso, iso_all = NULL) {
    if (is.null(iso_all)) iso_all <- get_iso_all()
    if (identical(length(iso), 1L))
        return(iso_all[iso_all$iso == iso, "name"])
    else
        return(iso_all[iso_all$iso %in% iso, "name"])
}

### Sub-Saharan Africa

get_ssa_regions_ordered <- function() c("Western Africa", "Eastern Africa", "Middle Africa", "Southern Africa")

get_ssa_countries_ordered_df <- function() {
    iso_all <- get_iso_all()
    iso_all[iso_all$sub_saharanafrica == "Yes", c("name", "region")]
    ssa_countries_ordered_df$region <-
        factor(ssa_countries_ordered_df$region, levels = ssa_regions_ordered,
               ordered = TRUE)
    ssa_countries_ordered_df <-
        ssa_countries_ordered_df[order(ssa_countries_ordered_df$region,
                                   ssa_countries_ordered_df$name), ]
    ssa_countries_ordered_df$name <-
        factor(ssa_countries_ordered_df$name,
               labels = ssa_countries_ordered_df$name, ordered = TRUE)
    return(ssa_countries_ordered_df)
}

get_ssa_countries_ordered <- function() get_ssa_countries_ordered_df()$name


###-----------------------------------------------------------------------------
### * Results

## FPEMglobal.aux updated so need this hack to keep working
get_FPEMglobal_csv_res <- function(...) {
    x <- FPEMglobal.aux::get_csv_res(...)
    colnames(x) <- tolower(colnames(x))
    colnames(x)[colnames(x) == "quantile"] <- "percentile"
    return(x)
}
