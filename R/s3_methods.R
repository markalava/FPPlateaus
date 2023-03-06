##' Extract or Replace Parts of a \code{fpplateaus_data_frame}
##'
##' These are methods for the subset operators and their companion
##' replacement functions for objects of class
##' \code{fpplateaus_data_frame}.
##'
##' @section Note:
##' R's usual simplification rules apply, namely, if you select a
##' single column from an \code{fpplateaus_data_frame} it will be
##' \dQuote{simplified} to a vector.
##'
##' @seealso \code{\link{fpplateaus_data_frame}}.
##'
##' @inheritParams base::`[.data.frame`
##' @inheritParams base::`[<-.data.frame`
##' @return An object of class inheriting from
##'     \code{fpplateaus_data_frame} \emph{if} the result has at least
##'     two columns and two rows.
##' @author Mark Wheldon
##'
##' @name subset_replace
NULL


##' @rdname subset_replace
##' @export
`[.fpplateaus_data_frame` <- function(x, i, j, drop) {
    get_att <- attributes(x)[get_fpplateaus_attr_names()]
    x <- NextMethod()
    ## If the subset results in a 'data.frame', make sure the
    ## 'fpplateaus_data_frame' attributes and class are retained.
    if (is.data.frame(x))
        return(do.call("fpplateaus_data_frame", c(list(x = x), get_att)))
    ## If the subset results in a vector, follow standard R behaviour
    ## and return a vector without all the extra attributes.
    else return(x)
}


##' @rdname subset_replace
##' @export
subset.fpplateaus_data_frame <- function(x, subset, select, drop = FALSE, ...) {
    get_att <- attributes(x)[get_fpplateaus_attr_names()]
    x <- NextMethod()
    ## If the subset results in a 'data.frame', make sure the
    ## 'fpplateaus_data_frame' attributes and class are retained.
    if (is.data.frame(x))
        return(do.call("fpplateaus_data_frame", c(list(x = x), get_att)))
    ## If the subset results in a vector, follow standard R behaviour
    ## and return a vector without all the extra attributes.
    else return(x)
}


##' @rdname coerce_fpplateaus_data_frame
##' @export
as.data.frame.fpplateaus_data_frame <- function(x, row.names = NULL, ...) {
    return(data.frame(NextMethod()))
}
