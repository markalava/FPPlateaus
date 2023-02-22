### Need S3 class because used for data frames with attributes.   :/


## Attribute names
get_fpplateaus_attr_names_double <- function() {
    c("differences", "change_condition_as_proportion",
                    "filter_width", "stall_probability_thresholds",
                    "CP_range_condition_min", "CP_range_condition_max",
                    "MDMM_range_condition_min", "MDMM_range_condition_max",
      "min_stall_length")
}

get_fpplateaus_attr_names_character <- function() { c("denominator_count_filename") }

get_fpplateaus_attr_names <- function() {
    c(get_fpplateaus_attr_names_double(), get_fpplateaus_attr_names_character())
}


##' Low-level constructor for class \code{fpplateaus_data_frame}.
##'
##' @description
##' Creates an object of class \code{fpplateaus_data_frame}. Minimal
##' checks are done; for interactive use see
##' \code{\link{fpplateaus_data_frame}}.
##'
##' This function is not exported. The user-level constructor is
##' \code{\link{fpplateaus_data_frame}}.
##'
##' @seealso fpplateaus_data_frame
##'
##' @author Mark Wheldon
new_fpplateaus_data_frame <-
    function(x,
             differences = double(),
             change_condition_as_proportion = double(),
             filter_width = double(),
             denominator_count_filename = character(),
             stall_probability_thresholds = double(),
             CP_range_condition_min = double(),
             CP_range_condition_max = double(),
             MDMM_range_condition_min = double(),
             MDMM_range_condition_max = double(),
             min_stall_length = double(),
             ..., class = character()) {
        if (missing(x)) x <- data.frame()
        stopifnot(is.data.frame(x))
        stopifnot(is.character(class))
        structure(x,
                  differences = differences,
                  change_condition_as_proportion = change_condition_as_proportion,
                  filter_width = filter_width,
                  denominator_count_filename = denominator_count_filename,
                  stall_probability_thresholds = stall_probability_thresholds,
                  CP_range_condition_min = CP_range_condition_min,
                  CP_range_condition_max = CP_range_condition_max,
                  MDMM_range_condition_min = MDMM_range_condition_min,
                  MDMM_range_condition_max = MDMM_range_condition_max,
                  min_stall_length = min_stall_length,
                  ...,
                  class = c(class, "fpplateaus_data_frame", "data.frame"))
    }


##' Validate objects of class fpplateaus_data_frame.
##'
##' Validator function for objects of class \code{fpplateaus_data_frame}.
##'
##' @param x Object to validate.
##' @return If valid, \code{x} is returned unchanged.
##' @author Mark Wheldon
validate_fpplateaus_data_frame <- function(x) {
    stopifnot(inherits(x, "data.frame"))
    attr_names_double <- get_fpplateaus_attr_names_double()
    attr_names_character <- get_fpplateaus_attr_names_character()

    ## Missing attributes
    missing <- c(attr_names_double[!attr_names_double %in% names(attributes(x))],
                 attr_names_character[!attr_names_character %in% names(attributes(x))])
    if (length(missing)) stop("The following attributes are missing: ",
                              toString(missing))

    ## Attribute Classes
    for (n in attr_names_double)
        if (!inherits(attr(x, n), "numeric")) stop(n, " is not 'numeric'.")
    for (n in attr_names_character)
        if (!inherits(attr(x, n), "character")) stop(n, " is not 'character'.")

    return(x)
}


##' Constructor for class \code{fpplateaus_data_frame}
##'
##' \description{
##' Creates objects of class \code{fpplateaus_data_frame}. Objects of
##' this class have class attribute
##' \code{c("demog_change_component_df", "data_frame")}.
##'
##' \code{is_demog_change_component_df} is a simple wrapper for
##' \code{\link{inherits}}.}
##'
##' @return An object of class \code{fpplateaus_data_frame}.
##' @author Mark Wheldon
##'
##' @export
fpplateaus_data_frame <- function(x,
                                  differences = double(),
                                  change_condition_as_proportion = double(),
                                  filter_width = double(),
                                  denominator_count_filename = character(),
                                  stall_probability_thresholds = double(),
                                  CP_range_condition_min = double(),
                                  CP_range_condition_max = double(),
                                  MDMM_range_condition_min = double(),
                                  MDMM_range_condition_max = double(),
                                  min_stall_length = double()) {

    validate_fpplateaus_data_frame(
        new_fpplateaus_data_frame(x = x,
                                  differences = differences,
                                  change_condition_as_proportion = change_condition_as_proportion,
                                  filter_width = filter_width,
                                  denominator_count_filename = denominator_count_filename,
                                  stall_probability_thresholds = stall_probability_thresholds,
                                  CP_range_condition_min = CP_range_condition_min,
                                  CP_range_condition_max = CP_range_condition_max,
                                  MDMM_range_condition_min = MDMM_range_condition_min,
                                  MDMM_range_condition_max = MDMM_range_condition_max,
                                  min_stall_length = min_stall_length))
}


##' @rdname fpplateaus_data_frame
##' @export
is_demog_change_component_df <- function(x) {
    inherits(x, "fpplateaus_data_frame")
}

