###-----------------------------------------------------------------------------
### * Directories

##' @export
verify_dir <- function(path) {
    stopifnot(dir.exists(path))
    return(invisible(path))
}

##' @export
soft_make_new_dir <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    return(invisible(path))
}

##' @export
ensure_new_dir <- function(path) {
    return(verify_dir(soft_make_new_dir(path)))
}


###-----------------------------------------------------------------------------
### * Locations

### Regions

### ISO Codes for 195

##' @export
get_iso_all <- function(output_dir) {
    iso_all <- base::merge(FPEMglobal.aux::get_used_unpd_regions(output_dir = output_dir),
                           FPEMglobal.aux::get_195_countries(output_dir = output_dir)[, "iso", drop = FALSE],
                       by = "iso",
                       all.y = TRUE, all.x = FALSE)
    iso_all$name[grep("^C.+te d.+Ivoire$", iso_all$name)] <- "Cote d'Ivoire"
    iso_all$name[grep("^R.+union$", iso_all$name)] <- "Reunion"
    return(iso_all)
}

##' @export
save_iso_all_to_results <- function(iso_all, filepaths_outputs) {
    saveRDS(iso_all, file = file.path(filepaths_outputs$results_output_dir, "iso_all.rds"))
}

##' @export
read_iso_all_from_results <- function(results_output_dir) {
    readRDS(file = file.path(results_output_dir, "iso_all.rds"))
}

##' @export
get_iso <- function(name, iso_all = NULL, output_dir = NULL) {
    if (is.null(iso_all)) iso_all <- get_iso_all(output_dir = output_dir)
    if (identical(length(name), 1L))
        return(iso_all[iso_all$name == name, "iso"])
    else
        return(iso_all[iso_all$name %in% name, "iso"])
}

##' @export
get_name <- function(iso, iso_all = NULL, output_dir = NULL) {
    if (is.null(iso_all)) iso_all <- get_iso_all(output_dir = output_dir)
    if (identical(length(iso), 1L))
        return(iso_all[iso_all$iso == iso, "name"])
    else
        return(iso_all[iso_all$iso %in% iso, "name"])
}

### Sub-Saharan Africa

##' @export
get_ssa_regions_ordered <- function() c("Western Africa", "Eastern Africa", "Middle Africa", "Southern Africa")

##' @export
get_ssa_countries_ordered_df <- function(output_dir) {
    iso_all <- get_iso_all(output_dir = output_dir)
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

##' @export
get_ssa_countries_ordered <- function(output_dir) get_ssa_countries_ordered_df(output_dir = output_dir)$name


###-----------------------------------------------------------------------------
### * Results

## FPEMglobal.aux updated so need this hack to keep working

##' @export
get_FPEMglobal_csv_res <- function(...) {
    x <- FPEMglobal.aux::get_csv_res(...)
    colnames(x) <- tolower(colnames(x))
    colnames(x)[colnames(x) == "quantile"] <- "percentile"
    return(x)
}
