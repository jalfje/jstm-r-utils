#' Add library path
#'
#' `add_lib_path` adds a path to the front of `.libPaths()`, making the added
#' path be the first path checked for installed packages.
#'
#' Does nothing if the passed library path is already the first path in
#' `.libPaths()`.
#'
#' @param libPath The path to add, as a character string.
#'
#' @return Resulting value of `.libPaths()`
#'
#' @export

add_lib_path <- function(libPath) {
    curPaths <- .libPaths()
    if (length(curPaths) > 0 && curPaths[[1]] != libPath) {
        .libPaths(c(libPath, curPaths))
    }
    return(.libPaths())
}
