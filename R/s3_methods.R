##' Extract or Replace Parts of a \code{fpplateaus_data_frame}
##'
##' These are methods for the subset operators and their companion
##' replacement functions for objects of class
##' \code{fpplateaus_data_frame}.
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
    do.call("fpplateaus_data_frame", c(list(x = NextMethod()), get_att))
}
