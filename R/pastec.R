#' Concatenate strings sequentially
#'
#' `pastec` and `pastec0` recursively combine their variable arguments with
#' [`c`] before passing it to [`paste`] or [`paste0`], respectively.
#'
#' `pastec` collapses and separates objects with a space by default but can be
#' customized, while `pastec0` always collapses with the empty string.
#'
#' @param ... one or more `R` objects, to be converted to character vectors.
#' @param sep a character string to separate the terms. Not [`NA_character_`].
#' @param collapse an optional character string to separate the results. Not
#'   [`NA_character_`].
#'
#' @seealso [paste()] [paste0()]

## Jamie St Martin
## 2018-11-01

#' @export
pastec <- function(..., sep = " ", collapse = " ") {
    paste(c(..., recursive = TRUE), sep = sep, collapse = collapse)
}

#' @export
#' @rdname pastec
pastec0 <- function(...) {
    paste0(c(..., recursive = TRUE), collapse = "")
}
