### Functions that interact with FPEMglobal outputs. These are copies
### of functions in the FPEMglobal.aux package
### (https://github.com/FPcounts/FPEMglobal.aux) as at 2023-01-24.
###
### 'FPEMglobal.aux' is released under the Apache License Version 2.0,
### January 2004, available at <http://www.apache.org/licenses/>.
###
### FPEMglobal.aux, Copyright 2013, FPEMglobal.aux authors.

##' Get denominator counts from csv files
##'
##' @description
##' Reads the '.csv' file containing the married and unmarried
##' denominator counts used in the run. These are in units of the
##' individual.
##'
##' The data frame returned has columns \code{"iso"}, \code{"name"},
##' \code{"year"}, \code{"count"}. The \code{"year"} column will have
##' years in \dQuote{1st Jan} format (i.e., whole number years, 1970,
##' 1971 etc.) because this is the format in which they are stored in
##' the \file{csv} files. This differs from
##' \code{\link{get_used_denominators}}. There is no option to return
##' \dQuote{mid-year} format years from \code{get_csv_denominators}.
##'
##' @details
##' One or more marital groups can be requested via argument
##' \code{marital_group}. If more than one, a column
##' \dQuote{\code{marital_group}} will be present in the output to
##' identify records accordingly (regardless the value of
##' \code{add_marital_group}). Value \code{"default"} will read the
##' the meta data (see \code{\link{get_global_mcmc_args}}) and ensure
##' the marital group matching that of the output is included. If
##' \code{"all women"} is included, values will be constructed by
##' summing the married and unmarried denominators.
##'
##' If \code{table_format} is \code{"long"}, the counts are in a
##' single column called \dQuote{\code{value}}, with the necessary
##' identifying columns added. Otherwise the result is in the same
##' format as the original counts files, which have one column per
##' year.
##'
##' The counts appear in the \file{.csv} files in units of 1 and, by
##' default, are returned as such (\code{units = "unit"}). Use the
##' \code{units} argument to return counts in multiples of 1000.
##'
##' @section Technical Note:
##' The poulation denominators are stored in \file{.csv} files in the
##' output directory. These are read in using an unexported function
##' from \pkg{FPEMglobal}, which is a thin wrapper to \code{read.csv}.
##'
##' @param filename Name of file with the counts (including
##'     extension). If \code{NULL}, this will be inferred from the
##'     meta data (see \code{\link{get_global_mcmc_args}}.
##' @param age_group Age group of the counts for the output data
##'     frame. If the column names are of the form
##'     'U/MW_\[aabb\]_year' this is ignored and the age group is
##'     taken from the column names.
##' @param marital_group Marital group to load denominators for. Can
##'     be more than one; see \dQuote{Details}. The default is the
##'     length 1 vector \code{"default"}.
##' @param add_marital_group Logical. Should a column
##'     \dQuote{\code{marital_group}} be added to the output to
##'     indicate the marital group? Such a column is always added if
##'     \code{marital_group} has more than one element.
##' @param units Units in which to return the counts.
##' @inheritParams get_csv_res
##' @inheritParams get_output_dir
##'
##' @return A \code{\link[tibble]{tibble}} with the denominators; see
##'     \dQuote{Details}.
##' @author Mark Wheldon
##'
##' @family input_data_functions
##'
##' @export
get_csv_denominators <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                  filename = NULL,
                                  marital_group = c("default", "married", "unmarried", "all women"),
                                  add_marital_group = length(marital_group) > 1L,
                                  age_group = NULL,
                                  add_age_group = TRUE,
                                  clean_col_names = TRUE,
                                 units = c("units", "thousands"),
                                 years_as_midyear = TRUE,
                                 table_format = c("long", "raw"),
                                 verbose = FALSE, ...) {

    ## -------* Sub-Functions

    ## MUST use this _inside_ this function.
    get_value_cols_colnames <- function(x) {

        ## Determine column name format

        ## There are three formats in use for the value columns:
        ## 1. E.g., MW_1549_1979 (i.e., marital group, age group, year)
        ## 2. E.g., 1549_1979 (i.e., age group, year)
        ## 3. E.g., 1979 (i.e., year)

        col_fmt1_regexp <- "^(M|U)W_[0-9]{4}_(19|20|21)[0-9]{2}$"
        col_fmt2_regexp <- "^X?[0-9]{4}_(19|20|21)[0-9]{2}$"
        col_fmt3_regexp <- "^X?(19|20|21)[0-9]{2}$"

        check_fmt1 <- grep(col_fmt1_regexp, colnames(x), value = TRUE)
        check_fmt2 <- grep(col_fmt2_regexp, colnames(x), value = TRUE)
        check_fmt3 <- grep(col_fmt3_regexp, colnames(x), value = TRUE)

        is_fmt1 <- length(check_fmt1)
        is_fmt2 <- length(check_fmt2)
        is_fmt3 <- length(check_fmt3)

        if (is_fmt1) value_cols <- check_fmt1
        else if (is_fmt2) value_cols <- check_fmt2
        else if (is_fmt3) value_cols <- check_fmt3
        else stop("Column format of '", fpath, "' cannot be determined.")

        if (is_fmt1 || is_fmt2) {
            if (is_fmt1) {
                age_group_from_cols <-
                    sapply(strsplit(gsub("X", "", value_cols), "_", fixed = TRUE), "[[", 2)
            } else if (is_fmt2) {
                age_group_from_cols <-
                    sapply(strsplit(gsub("X", "", value_cols), "_", fixed = TRUE), "[[", 1)
            }
            age_group_from_cols <- paste(substr(age_group_from_cols, 1, 2),
                                         substr(age_group_from_cols, 3,4), sep = "-")
            if (!identical(as.character(rep(age_group_from_cols[1], length(age_group_from_cols))),
                           as.character(age_group_from_cols)))
                stop("Age groups implied in column names of '", fpath, "' are not all the same: ",
                     toString(age_group_from_cols))
            if (!identical(as.character(age_group_from_cols[1]), as.character(age_group)))
                stop("Age group implied in column names is '", as.character(age_group_from_cols[1]),
                     "' but 'age_group' argument is '", age_group,
                     "'. Note: if you didn't specify 'age_group' it was read from the meta data.")
        }
        return(value_cols)
    }

    ## -------* Set-up

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = is.null(filename), countrytrajectories = FALSE,
                           made_results = FALSE,
                           assert_valid = FALSE #<<<<<<<<<<<< IF THIS IS TRUE TESTS WILL PROBABLY FAIL
                           )

    output_age_group <- get_age_group(output_dir = output_dir)

    if (missing(marital_group)) marital_group <- "default"
    else marital_group <- match.arg(marital_group, several.ok = TRUE)
    if ("default" %in% marital_group) {
        marital_group[marital_group == "default"] <- get_marital_group(output_dir = output_dir)
        marital_group <- unique(marital_group)
    }

    table_format <- match.arg(table_format)

    units <- match.arg(units)
    if (identical(units, "units")) unit_mult <- 1
    else unit_mult <- 1/1000

    data_dir_name <- "data"
    data_dir <- file.path(output_dir, data_dir_name)

    if (is.null(filename)) {
        if (is_all_women_run(output_dir = output_dir))
            denom_file_name_meta <- get_combine_runs_args(output_dir = output_dir)$denominator_counts_csv_filename
        else
            denom_file_name_meta <- get_global_post_process_args(output_dir = output_dir)$denominator_counts_csv_filename
        if (length(denom_file_name_meta) && nchar(denom_file_name_meta)) {
            filename <- basename(denom_file_name_meta)

            ## Since filename auto-determined, read the age group as well.
            age_group_from_args <- output_age_group
            if (!is.null(age_group) && !identical(as.character(age_group), as.character(age_group_from_args)))
                warning("'age_group' = '", age_group, "', but output directory is a run for age group '",
                        age_group_from_args, "'. Argument 'age_group' reset to the latter.\n\nIf you want to load an age group different from the age group of the output directory, you must specify 'filename'.")
            age_group <- age_group_from_args
        }
        else stop("'filename' could not be determined from meta data; you must be supply it via the 'filename' argument.")
    }

    fpath <- file.path(data_dir, filename)

    ## If 'age_group' still 'NULL', use mcmc args
    if (is.null(age_group)) age_group <- output_age_group

    ## -------* Load .csv

    if (any(marital_group %in% c("married", "all women"))) {
        denom_counts_m <-
            ## NEXT LINE MODIFIED from original. 'FPEMglobal:::extractDenominators' -> 'extractDenominators
            extractDenominators(fpath, in_union = 1)
    }
    if (any(marital_group %in% c("unmarried", "all women"))) {
        denom_counts_u <-
            ## NEXT LINE MODIFIED from original. 'FPEMglobal:::extractDenominators' -> 'extractDenominators
            extractDenominators(fpath, in_union = 0)
    }
    if ("all women" %in% marital_group) {
        if (!(identical(dim(denom_counts_m), dim(denom_counts_u)) &&
              identical(colnames(denom_counts_m), colnames(denom_counts_u)) &&
              identical(sort(denom_counts_m$ISO.code), sort(denom_counts_u$ISO.code))))
            stop("Cannot create all women denominators. The married and unmarried tables have different ISOs or different columns.")
        value_cols <- get_value_cols_colnames(denom_counts_m)
        denom_counts_a <- denom_counts_m
        denom_counts_a[, value_cols] <-
            denom_counts_m[, value_cols] +
            denom_counts_u[match(denom_counts_m$ISO.code, denom_counts_u$ISO.code), value_cols]
        if (all(grepl("^MW_", value_cols))) {
            value_cols_idx <- match(value_cols, colnames(denom_counts_a))
            colnames(denom_counts_a)[value_cols_idx] <-
                make.names(gsub("^MW_", "", colnames(denom_counts_a)[value_cols_idx]))
        }
    }

    denom_counts <- data.frame()
    if ("married" %in% marital_group) {
        denom_counts <- dplyr::bind_rows(denom_counts, denom_counts_m)
        if (add_marital_group) denom_counts$marital_group <- "married"
    }
    if ("unmarried" %in% marital_group) {
        denom_counts <- dplyr::bind_rows(denom_counts, denom_counts_u)
        if (add_marital_group) denom_counts$marital_group <- "unmarried"
    }
    if ("all women" %in% marital_group) {
        denom_counts <- dplyr::bind_rows(denom_counts, denom_counts_a)
        if (add_marital_group) denom_counts$marital_group <- "all women"
    }

    value_cols <- get_value_cols_colnames(denom_counts)
    value_cols_idx <- match(value_cols, colnames(denom_counts))

    denom_counts <- tibble::as_tibble(denom_counts)

    ## -------* Cleaning, Additional Columns, etc.

    denom_counts[, value_cols] <- denom_counts[, value_cols] * unit_mult

    if (years_as_midyear) {
        colnames(denom_counts)[value_cols_idx] <-
            paste0(colnames(denom_counts)[value_cols_idx], ".5")
        value_cols <- paste0(value_cols, ".5")
        }

    if (identical(table_format, "long")) {
        value_cols <- sapply(strsplit(value_cols, "_"), function(z) z[[length(z)]])
        colnames(denom_counts)[value_cols_idx] <- value_cols
        denom_counts <-
            tidyr::gather(denom_counts, tidyselect::all_of(value_cols),
                          key = "year", value = "count") %>%
        dplyr::mutate(year = gsub("X|x", "", year)) %>%
            dplyr::mutate(year = as.numeric(year))
        if (any(is.na(denom_counts$year))) stop("'year' is 'NA' for some rows.")
    }

    if (add_age_group && !("age_group" %in% colnames(denom_counts)) && identical(length(age_group), 1L))
        denom_counts$age_group <- age_group

    if (add_marital_group && !("marital_group" %in% colnames(denom_counts)) && identical(length(marital_group), 1L))
        denom_counts$marital_group <- marital_group

    if (clean_col_names) {
        denom_counts <- clean_col_names(denom_counts)
    }

    ## -------* END

   return(tibble::as_tibble(denom_counts))
}


##' Load and return country-level trajectories for married and unmarried women
##'
##' This function \code{\link{load}}s the MCMC sample from the
##' posterior sample for married or unmarried for a single country and returns it as an R
##' object. Trajectories are loaded from \file{.rda} files found in
##' the subdirectory \file{countrytrajectories} of the results
##' directory (see below). The filename for the given country is
##' determined by reference to an index which must be in the file
##' \code{file.path(output_dir, "iso.Ptp3s.key.csv")}.
##'
##' Country trajectories are 3D matrices:
##' \preformatted{str(...)
##' num [1:61, 1:3, 1:13344] 0.0622 0.0725 0.0879 0.0633 0.1078 ...
##' - attr(*, "dimnames")=List of 3
##'  ..$ : chr [1:61] "1970.5" "1971.5" "1972.5" "1973.5" ...
##'  ..$ : chr [1:3] "Traditional" "Modern" "Unmet"
##'  ..$ : NULL}
##'
##' \subsection{Married/unmarried vs all women trajectories}{
##' The country trajectories for all women are stored differently. Do
##' not use this function for all women country trajectories. Use
##' \code{\link{get_country_traj_aw}} instead.
##'
##' The country trajectories for all women are \emph{counts}. Those
##' for married and unmarried are \emph{proportions}.}
##'
##' @family trajectory functions
##'
##' @section Specifying results directory:
##' See the section in \code{\link{get_csv_res}}.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @param iso_code Numeric ISO code of country to get trajectories
##'     for.
##' @return The loaded country trajectory object.
##' @author Mark Wheldon
##' @export
get_country_traj_muw <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                 iso_code,
                                 verbose = FALSE) {

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = TRUE, countrytrajectories = TRUE)

    if (is_all_women_run(output_dir = output_dir))
        stop("'", output_dir, "' is an all women output directory. Use 'get_country_traj_aw' instead. NOTE that all women trajectories are in a different format from married and unmarried trajectories.")

    stopifnot(identical(length(iso_code), 1L))
    iso_code <- as.character(iso_code)

    traj_index <-
        get_country_index(run_name = run_name, output_dir = output_dir,
                          root_dir = root_dir, verbose = verbose)
    if (!(iso_code %in% traj_index$iso.c)) stop("'iso_code' not found in trajectory index (see '?get_country_index').")
    traj_fname <-
        traj_index$filename[traj_index$iso.c == iso_code]

    traj_full_path <- file.path(output_dir, "countrytrajectories", traj_fname)
    tmp_env <- new.env()
    if (verbose) message("Reading '", traj_full_path, "'.")
    obj <- get(load(file = traj_full_path, envir = tmp_env), envir = tmp_env)

    return(obj)
}


##' Load and return country-level trajectories for all women
##'
##' This function \code{\link{load}}s the MCMC sample from the
##' posterior sample for all women for a single country and returns it
##' as an R object. Trajectories are loaded from \file{.rda} files
##' found in the subdirectory \file{countrytrajectories} of the
##' results directory (see below). The filename for the given country
##' is \code{paste0("aw_ISO_", iso_code, "_counts.rda")}.
##'
##' Country trajectories are 3D matrices:
##' \preformatted{str(...)
##' num [1:61, 1:6, 1:15000] 37.2 37.3 39.2 43.6 51.9 ...
##' - attr(*, "dimnames")=List of 3
##'  ..$ : chr [1:61] "1970.5" "1971.5" "1972.5" "1973.5" ...
##'  ..$ : chr [1:6] "Total" "Modern" "Traditional" "Unmet" ...
##'  ..$ : NULL}
##'
##' \subsection{Married/unmarried vs all women trajectories}{
##' The country trajectories for married and unmarried women are
##' stored differently. Do not use this function for married and
##' unmarried women country trajectories. Use
##' \code{\link{get_country_traj_muw}} instead.
##'
##' The country trajectories for all women are \emph{counts}. Those
##' for married and unmarried are \emph{proportions}.}
##'
##' @family trajectory functions
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @inheritParams get_country_traj_muw
##' @return The loaded country trajectory object.
##' @author Mark Wheldon
##' @export
get_country_traj_aw <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                iso_code,
                                verbose = FALSE) {
    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = TRUE)

    if (!is_all_women_run(output_dir = output_dir))
        stop("'", output_dir, "' is not an all women output directory. Use 'get_country_traj_muw' instead. NOTE that married/unmarried trajectories are in a different format from all women trajectories.")

    stopifnot(identical(length(iso_code), 1L))

    traj_fname <- paste0("aw_ISO_", iso_code, "_counts.rda")
    traj_full_path <- file.path(output_dir, "countrytrajectories", traj_fname)

    tmp_env <- new.env()
    if (verbose) message("Reading '", traj_full_path, "'.")
    obj <- get(load(file = traj_full_path, envir = tmp_env), envir = tmp_env)

    return(obj)
}


##' Get UNPD aggregate country classifications
##'
##' Loads the country classifications actually used in a model
##' run. The files are read using \code{\link[readr]{read_csv}}.
##'
##' @family countries, regions and aggregates functions
##'
##' @param UNlocations_names Logical; should
##'     \code{\link{match_UNlocations}(..., return_names = "UNlocations")} be
##'     run to standardize country and area names?
##'
##' @inheritParams get_used_input_data
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##'
##' @family country_aggregates
##' @export
get_used_unpd_regions <-
    function(run_name = NULL, output_dir = NULL, root_dir = NULL,
             clean_col_names = TRUE, UNlocations_names = TRUE,
             verbose = FALSE) {

        if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
        on.exit(options(op), add = TRUE, after = FALSE) }

        output_dir <-
            output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                               root_dir = root_dir, verbose = verbose)
        data_dir_name <- "data"
        data_dir <- file.path(output_dir, data_dir_name)

        fname <- file.path(data_dir, "country_and_area_classification.csv")
        if (verbose) message("Reading '", file.path(tbl_dir, fname), "'.")
        out <- readr::read_csv(fname)
        if (UNlocations_names)
            out[, "Country or area"] <-
                match_UNlocations(out[, "Country or area"], "UNlocations")
        if (clean_col_names) out <- clean_col_names(out)

        return(out)
    }


##' Read table of the \dQuote{195} countries
##'
##' These are the 195 countries in the original married women
##' model. The file is read using \code{\link[readr]{read_csv}}.
##'
##' @section Note:
##' These are taken from the \pkg{FPEMglobal} package \emph{installed
##' on your system}. As such, they \emph{may} not correspond to the
##' aggregates used in a specific model run (although, as of
##' 2022-08-25, this is unlikely).
##'
##' @family countries, regions and aggregates functions
##'
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}} with the countries.
##' @author Mark Wheldon
##'
##' @family country_aggregates
##'
##' @export
get_195_countries <- function(clean_col_names = TRUE, verbose = FALSE) {
    if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
        on.exit(options(op), add = TRUE, after = FALSE) }
    fname <- system.file("extdata/countries_mwra_195.csv", package = "FPEMglobal")
    if (verbose) message("Reading '", fname, "'.")
    out <- readr::read_csv(fname)
    if (clean_col_names) out <- clean_col_names(out)
    return(out)
}


##' Read results csv files containing percentages, counts, and ratios
##'
##' Reads \emph{all} results \file{.csv} files from a results
##' directory for a given country or region that contain proportions,
##' counts, and ratios. The files are read using
##' \code{\link[readr]{read_csv}}.
##'
##' The \code{stat} argument specifies the type of results to
##' return. Results are stored separately for prevalence proportions,
##' ratio indicators, and number of users. These can be requested by,
##' respectively, \code{"prop"}, \code{"ratio"}, \code{"count"}. Note
##' that prevalence \emph{proportions} are stored in \file{.csv} files
##' with \dQuote{perc} in their names.
##'
##' \code{clean_col_names} applies \code{\link{clean_col_names}} to
##' the column names. This tidies up and standardizes column
##' names. \emph{Note:} An additional step is performed after calling
##' \code{\link{clean_col_names}} the function: the column name
##' \dQuote{\code{percentile}} is changed to
##' \dQuote{\code{quantile}}. This makes the column name match the
##' actual values in the column. This column is misnamed in the raw
##' \file{.csv} output files.
##'
##' Country classifications are loaded via
##' \code{\link{get_used_unpd_regions}},
##' \code{\link{get_used_world_bank_regions}}, and
##' \code{\link{get_used_special_aggregates}}.
##'
##' \code{table_format} determines the \dQuote{shape} of the output
##' table. It takes values in c("long", "wide", "raw"),
##' which have the following effects:
##' \describe{
##' \item{\code{"raw"}}{No reshaping of the input file is done. This
##' format has a column for each year and an ID column for indicator
##' (\code{"indicator"}).}
##' \item{\code{"wide"}}{The output has a column for each indicator
##' and an ID column for years (\code{"year"}).}
##' \item{\code{"long"}}{Like \code{"wide"} but the indicators are
##' also transposed. It has a single \code{"value"} colum for all
##' indicators and an ID column for indicator (\code{"indicator"})}
##' The default is \code{"long"}.
##' }
##'
##' @param output_dir Path to directory containing outputs. See
##'     Section \dQuote{Specifying results directory} in the help file
##'     for \code{\link{get_output_dir}}. Note that \code{root_dir} is
##'     ignored if \code{output_dir} is supplied.
##' @param verbose Logical; report the path, filename, and object name
##'     in a message?
##' @param aggregate Name of the 'aggregate' to load. Note: use
##'     \code{aggregate = "country"} to load country results.
##' @param stat Which statistics should be loaded? Allowable values
##'     are \code{c("orig", "adj", "sub_adj")}.
##' @param add_stat_column Logical. Add a column \dQuote{\code{stat}}
##'     with value \code{stat} to the result?
##' @param adjusted Loads original results (\dQuote{orig}), adjusted
##'     medians only (\dQuote{adj}), or original results with medians
##'     substituted with adjusted medians (\dQuote{sub_adj}).
##' @param add_adjusted_column Add a column \dQuote{\code{adjusted}}?
##'     Cells indicate which rows are adjusted (\code{TRUE}) and which
##'     are not (\code{FALSE}).
##' @param years_as_midyear Logical; should years be labelled in
##'     \dQuote{mid-year} format, e.g., 1970.5, 1971.5, etc.?
##' @param clean_col_names Logical; when \code{TRUE}, the column names
##'     of the result are \sQuote{cleaned} by applying
##'     \code{\link{clean_col_names}}. See \dQuote{Details} for a note
##'     about \dQuote{percentile} vs. \dQuote{quantile}.
##' @param add_country_classifications Logical; add on columns with
##'     geographic country classifications? Only has an effect if
##'     \code{aggregate} is \code{"country"}. See also
##'     \dQuote{Details}.
##' @param table_format Logical or character. Should the table be
##'     returned in long format? See \dQuote{Details}.
##' @param sort Logical. Sort by stat, name, year, percentile?
##' @param verbose Logical. Print lots of messages? See
##'     \code{link{FPEMglobal.aux}} for a note about \pkg{readr}
##'     messages.
##' @inheritParams get_output_dir
##'
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##'
##' @family csv results functions
##'
##' @seealso \code{\link{get_output_dir}} for instructions on how to
##'     specify output directories and run
##'     names. \code{link{FPEMglobal.aux}} for a note about
##'     \pkg{readr} messages.
##'
##' @author Mark Wheldon
##' @export
get_csv_res <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                        aggregate = "country",
                        add_aggregate_column = length(aggregate) > 1L,
                        stat = c("prop", "count", "ratio"),  #Could add 'age_ratio' here later
                        add_stat_column = FALSE,
                        adjusted = c("orig", "adj", "sub_adj"),
                        add_adjusted_column = identical(adjusted, "sub_adj"),
                        clean_col_names = TRUE,
                        years_as_midyear = TRUE,
                        add_country_classifications = FALSE,
                        table_format = c("long", "wide", "raw"),
                        sort = TRUE,
                        verbose = FALSE) {

    if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
    on.exit(options(op), add = TRUE, after = FALSE) }

    stat <- match.arg(stat)

    if (add_country_classifications && !identical(aggregate, "country")) {
        warning("'add_country_classifications' only has an effect if 'aggregate' == '\"country\"'. No classifications will be added.")
        add_country_classifications <- FALSE
    }

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = TRUE, made_results = TRUE,
                           age_ratios = FALSE,
                           adjusted_medians = any(c("adj", "sub_adj") %in% adjusted))

    data_dir_name <- "data"
    table_dir_name <- "table"

    tbl_dir0 <- file.path(output_dir, table_dir_name)

    if (is.null(run_name)) run_name <- get_run_name(output_dir = output_dir, verbose = verbose)
    fname <- paste(run_name, aggregate, sep = "_")

    adjusted <- match.arg(adjusted)

    table_format <- match.arg(table_format)

    ## -------* Load the 'orig' results

    if (adjusted %in% c("orig", "sub_adj")) {

        tbl_dir <-
            table_orig_adj_dir(tbl_dir = tbl_dir0, adj = FALSE,
                               verbose = verbose)

        if (stat %in% c("prop", "count")) {

            if (identical(stat, "prop")) stat_in_fname <- "perc"
            else stat_in_fname <- stat

            ## Read the '_Total' file first
            fname1 <- paste0(fname, "_", stat_in_fname, "_Total.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname1), "'.")
            res <- readr::read_csv(file.path(tbl_dir, fname1))
            res$stat <- stat
            res$indicator <- "Total"

            ## Read in rest of indicators
            for (indicator in c("Modern", "TotalPlusUnmet", "Traditional", "TradPlusUnmet",
                               "Unmet")) {
                fname_ind <- paste0(fname, "_", stat_in_fname, "_", indicator, ".csv")
                if (verbose) message("Reading '", file.path(tbl_dir, fname_ind), "'.")
                res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind))
                res_ind$stat <- stat
                res_ind$indicator <- indicator
                res <- dplyr::bind_rows(res, res_ind)
            }

        } else if (identical(stat, "ratio")) {

            ## Read in 'ratio'

            ## Read the '_Met Demand' file first
            fname1 <- paste0(fname, "_", stat, "_Met Demand.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname1), "'.")
            res <- readr::read_csv(file.path(tbl_dir, fname1))
            res$stat <- stat
            res$indicator <- "Met Demand"

            ind_values <- c("MetDemModMeth", "ModernOverTotal")
            if (adjusted == "orig") ind_values <- c(ind_values, "Z")
            for (indicator in ind_values) {
                fname_ind <- paste0(fname, "_", "ratio", "_", indicator, ".csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname_ind), "'.")
                res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind))
                res_ind$stat <- stat
                res_ind$indicator <- indicator
                res <- dplyr::bind_rows(res, res_ind)
            }
        }
    }

    ## -------* Load adjusted results

    if (adjusted %in% c("adj", "sub_adj")) {

        tbl_dir <-
            table_orig_adj_dir(tbl_dir = tbl_dir0, adj = TRUE,
                               verbose = verbose)

        if (stat %in% c("prop", "count")) {

            if (identical(stat, "prop")) stat_in_fname <- "perc"
            else stat_in_fname <- stat

            ## Read the '_Total' file first
            fname1 <- paste0(fname, "_", stat_in_fname, "_Total_Adj.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname1), "'.")
            res_adj <- readr::read_csv(file.path(tbl_dir, fname1))
            res_adj$stat <- stat
            res_adj$indicator <- "Total"

            ## Read in rest of indicators
            for (indicator in c("Modern", "TotalPlusUnmet", "Traditional", "TradPlusUnmet",
                               "Unmet")) {
                fname_ind <- paste0(fname, "_", stat_in_fname, "_", indicator, "_Adj.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname_ind), "'.")
                res_adj_ind <- readr::read_csv(file.path(tbl_dir, fname_ind))
                res_adj_ind$stat <- stat
                res_adj_ind$indicator <- indicator
                res_adj <- dplyr::bind_rows(res_adj, res_adj_ind)
            }

        } else if (identical(stat, "ratio")) {

            ## Read the '_Met Demand' file first
            fname1 <- paste0(fname, "_", stat, "_Met Demand_Adj.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname1), "'.")
            res_adj <- readr::read_csv(file.path(tbl_dir, fname1))
            res_adj$stat <- stat
            res_adj$indicator <- "Met Demand"

            ## Read in 'ratio'
            ind_values <- c("MetDemModMeth", "ModernOverTotal")
            for (indicator in ind_values) {
                fname_ind <- paste0(fname, "_", "ratio", "_", indicator, "_Adj.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname_ind), "'.")
                res_adj_ind <- readr::read_csv(file.path(tbl_dir, fname_ind))
                res_adj_ind$stat <- stat
                res_adj_ind$indicator <- indicator
                res_adj <- dplyr::bind_rows(res_adj, res_adj_ind)
            }
        }
    }

    ## -------* Modify, Merge, Etc.

    ## Years columns
    if (!years_as_midyear) {
        if (adjusted %in% c("orig", "sub_adj")) {
            yr_cols_idx <- !colnames(res) %in% c("Name", "Iso", "Percentile", "indicator", "stat")
            colnames(res)[yr_cols_idx] <- round_down_years(colnames(res)[yr_cols_idx])
        }
        if (adjusted %in% c("adj", "sub_adj")) {
            yr_cols_idx <- !colnames(res_adj) %in% c("Name", "Iso", "Percentile", "indicator", "stat")
            colnames(res_adj)[yr_cols_idx] <- round_down_years(colnames(res_adj)[yr_cols_idx])
        }
    }

    ## Reshape
    if (adjusted == "orig") {
        if (table_format %in% c("wide", "long")) {
            res <- tidyr::gather(res, -Name, -Iso, -Percentile, -stat, -indicator,
                                 key = "Year", value = "Value")
            res$Year <- as.numeric(res$Year)
        }
        if (add_adjusted_column) res$adjusted <- FALSE
    }

    if (adjusted == "adj") {
        ## Only output res_adj
        res <- res_adj
        if (table_format %in% c("wide", "long")) {
            res <- tidyr::gather(res, -Name, -Iso, -Percentile, -stat, -indicator,
                                 key = "Year", value = "Value")
            res$Year <- as.numeric(res$Year)
        }
        if (add_adjusted_column) res$adjusted <- TRUE

    } else if (adjusted == "sub_adj") {
        if (table_format %in% c("wide", "long")) {
            res <- tidyr::gather(res, -Name, -Iso, -Percentile, -stat, -indicator,
                                 key = "Year", value = "Value")
            res_adj <- tidyr::gather(res_adj, -Name, -Iso, -Percentile, -stat, -indicator,
                                     key = "Year", value = "Value")
            res <- dplyr::left_join(res, res_adj,
                                    by = c("Name", "Iso", "Percentile", "stat",
                                           "indicator", "Year"))
            res[res$Percentile == 0.5,]$Value.x <- res[res$Percentile == 0.5,]$Value.y
            res <- dplyr::select(res, -Value.y) %>% dplyr::rename(Value = Value.x)

            ## Substitute medians for adjusted medians
            res_med <- res$Percentile == 0.5
            if (add_adjusted_column) {
                res$adjusted <- FALSE
                res$adjusted[res_med] <- TRUE
            }
            res$Year <- as.numeric(res$Year)
        } else {
            if (!identical(sort(colnames(res)), sort(colnames(res_adj))))
                stop("Column names in 'orig' and 'adj' results are not the same; cannot concatenate.")
            if (add_adjusted_column) {
                res$adjusted <- FALSE
                res_adj$adjusted <- TRUE
            }
            res <- dplyr::bind_rows(res[res$Percentile != 0.5,], res_adj)
        }
    }

    if (identical(table_format, "wide")) {
        ind_vals <- unique(res$indicator)
        res <- res %>%
            tidyr::spread(key = c("indicator"), value = "Value")
        if ("adj" %in% colnames(res))
            res <- res[,c("stat", "Name", "Iso", "Year", "Percentile", "adjusted", ind_vals)]
        else res <- res[,c("stat", "Name", "Iso", "Year", "Percentile", ind_vals)]
    }

    ## add classifications?
    if (add_country_classifications) {
        class_unpd_agg <-
            get_used_unpd_regions(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                                  clean_col_names = FALSE,
                                  verbose = verbose)

        class_spec <-
            get_used_special_aggregates(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                                        clean_col_names = FALSE,
                                        verbose = verbose)

        res <- res %>%
            dplyr::left_join(dplyr::select(class_unpd_agg, -`Country or area`),
                             by = c("Iso" = "ISO Code")) %>%
            dplyr::left_join(class_spec, by = c("Iso" = "iso.country"))
    }

    ## Stat column
    if (add_stat_column && !("stat" %in% colnames(res))) res$stat <- stat
    else if (!add_stat_column && "stat" %in% colnames(res))
        res <- res[, colnames(res) != "stat"]

    ## Sort
    if (sort) {
        if (identical(table_format, "raw"))
            res <- res %>% dplyr::arrange(indicator, Iso, Percentile)
        else if (identical(table_format, "wide"))
            res <- res %>% dplyr::arrange(Iso, Year, Percentile)
        else if (identical(table_format, "long"))
            res <- res %>% dplyr::arrange(indicator, Iso, Year, Percentile)
    }

    ## Clean column names
    if (clean_col_names) {
        res <- clean_col_names(res)
        ## Fix the 'percentile' / 'quantile' mess (only if 'clean_col_names' = TRUE
        colnames(res)[colnames(res) == "percentile"] <- "quantile"
    }

    ## END
    return(tibble::as_tibble(res))
}
