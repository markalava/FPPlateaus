### Need S3 class because used for data frames with attributes.   :/


## Attribute names
get_fpplateaus_attr_names_numeric <- function() {
    c("differences", "change_condition_as_proportion",
      "filter_width", "stall_probability_thresholds",
      "CP_range_condition_min", "CP_range_condition_max",
      "MDMM_range_condition_min", "MDMM_range_condition_max",
      "min_stall_length")
}

get_fpplateaus_attr_names_character <- function() { c("denominator_count_filename", "Level_condition_variant") }

get_fpplateaus_attr_names <- function() {
    c(get_fpplateaus_attr_names_numeric(), get_fpplateaus_attr_names_character())
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
    function(x = data.frame(),
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
             Level_condition_variant = character(),
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
                  Level_condition_variant = Level_condition_variant,
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
    stopifnot(inherits(x, "fpplateaus_data_frame"))

    attr_names_double <- get_fpplateaus_attr_names_numeric()
    attr_names_character <- get_fpplateaus_attr_names_character()

    ## Missing attributes
    missing <- c(attr_names_double[!attr_names_double %in% names(attributes(x))],
                 attr_names_character[!attr_names_character %in% names(attributes(x))])
    if (length(missing)) stop("The following attributes are missing: ",
                              toString(missing))

    ## Attribute Classes
    for (n in attr_names_double)
        if (!is.numeric(attr(x, n))) stop("Attribute '", n, "' is not 'numeric'.")
    for (n in attr_names_character)
        if (!is.character(attr(x, n))) stop("Attribute '", n, "' is not 'character'.")

    return(x)
}


##' Constructor for class \code{fpplateaus_data_frame}
##'
##' @description
##' Creates objects of class \code{fpplateaus_data_frame}. Objects of
##' this class have class attribute
##' \code{c("fpplateaus_data_frame", "data_frame")}.
##'
##' \code{is_fpplateaus_data_frame} is a simple wrapper for
##' \code{\link{inherits}}.
##'
##' @return An object of class \code{fpplateaus_data_frame}.
##' @author Mark Wheldon
##'
##' @export
fpplateaus_data_frame <- function(x,
                                  differences = attr(x, "differences"),
                                  change_condition_as_proportion = attr(x, "change_condition_as_proportion"),
                                  filter_width = attr(x, "filter_width"),
                                  denominator_count_filename = attr(x, "denominator_count_filename"),
                                  stall_probability_thresholds = attr(x, "stall_probability_thresholds"),
                                  CP_range_condition_min = attr(x, "CP_range_condition_min"),
                                  CP_range_condition_max = attr(x, "CP_range_condition_max"),
                                  MDMM_range_condition_min = attr(x, "MDMM_range_condition_min"),
                                  MDMM_range_condition_max = attr(x, "MDMM_range_condition_max"),
                                  min_stall_length = attr(x, "min_stall_length"),
                                  Level_condition_variant = attr(x, "Level_condition_variant")) {

    stopifnot(is.data.frame(x))

    ## Some attributes might not be in 'x' if this is called during
    ## results creation. Set them to defaults so validation passes.
    for (k in get_fpplateaus_attr_names_numeric()) {
        if (is.null(get(k))) assign(k, double())
    }
    for (k in get_fpplateaus_attr_names_character()) {
        if (is.null(get(k))) assign(k, character())
    }

    validate_fpplateaus_data_frame(
        new_fpplateaus_data_frame(x = as.data.frame(x),
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
                                  Level_condition_variant = Level_condition_variant))
}


##' Coerce to a \code{fpplateaus_data_frame}
##'
##' These functions coerce an object to a \code{fpplateaus_data_frame}
##' if possible, or check if it is one. The method for
##' \code{as.data.frame} will strip \code{x} of all its special
##' attributes and set the class to \code{"data.frame"}.
##'
##' @param x An object to coerce or check.
##' @param ... Further arguments passed to specific methods.
##' @return A coerced object in the case of the \code{as_...}
##'     functions; a logical for the \code{is_...} functions.
##' @author Mark Wheldon
##'
##' @name coerce_fpplateaus_data_frame
##' @export
as_fpplateaus_data_frame <- function(x, ...) {
    UseMethod("as_fpplateaus_data_frame")
}

##' @rdname coerce_fpplateaus_data_frame
##' @export
as_fpplateaus_data_frame.default <- function(x, ...) {
    if (is_fpplateaus_data_frame(x)) return(x)
    stop("Cannot coerce 'x' to 'fpplateaus_data_frame'.")
}

##' @rdname coerce_fpplateaus_data_frame
##' @export
as_fpplateaus_data_frame.data.frame <- function(x) {
    fpplateaus_data_frame(x)
}

##' @rdname coerce_fpplateaus_data_frame
##' @export
as_fpplateaus_data_frame.fpplateaus_data_frame <- function(x) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("fpplateaus_data_frame", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(validate_fpplateaus_data_frame(x))
}

##' @rdname coerce_fpplateaus_data_frame
##' @export
is_fpplateaus_data_frame <- function(x) {
    inherits(x, "fpplateaus_data_frame")
}
