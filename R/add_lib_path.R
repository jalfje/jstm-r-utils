#' Add library path
#'
#' `add_lib_path` adds a path to the front of [`.libPaths`], making the added
#' path be the first path checked for installed packages.
#'
#' Does nothing if the passed library path is already the first path in
#' [`.libPaths`].
#'
#' @param lib_path the path to add, as a character string.
#'
#' @return Resulting value of [`.libPaths`].
#'
#' @seealso [set_repo()] [.libPaths()] [install.packages()]
#'
#' @export

add_lib_path <- function(lib_path) {
    cur_paths <- .libPaths()
    if (length(cur_paths) > 0 && cur_paths[[1]] != lib_path) {
        .libPaths(c(lib_path, cur_paths))
    }
    return(.libPaths())
}
