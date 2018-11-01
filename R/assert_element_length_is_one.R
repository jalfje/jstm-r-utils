#' Assert that each element in the list is of length 1.
#'
#' Returns `TRUE` if each element in `x` has length 1. Otherwise it will produce
#' an error with [`stop`].
#'
#' @param x the list to examine.
#'
#' @return \code{\link[=invisible]{invisible(TRUE)}}
#'
#' @seealso [stop()]
#'
#' @export

assert_element_length_is_one <- function(x) {
    for (element in x) {
        if (length(element) != 1) {
            # Use non-standard evaluation to give better debug output
            original_x <- deparse(substitute(x))
            stop(pastec("Element found in", original_x, "with length not 1:", element))
        }
    }
    return(invisible(TRUE))
}