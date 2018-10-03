#' Set CRAN repository
#'
#' `set_repo` sets the URL of the repository to download new packages from.
#'
#' This sets `options(repos)`, overwriting the value. The previous value is
#' returned, which can be helpful if you want to set the repository only
#' temporarily.
#'
#' @param repo_url The URL of the CRAN repository to download packages from
#'
#' @return The previously repository URL, invisibly.
#'
#' @export

set_repo <- function(repo_url) {
    r <- getOption("repos")
    orig <- r["CRAN"]
    r["CRAN"] <- repo_url
    options(repos = r)
    return(invisible(orig))
}
