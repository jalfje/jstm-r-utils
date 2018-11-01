#' Set CRAN repository
#'
#' `set_repo` sets the URL of the repository to download new packages from.
#'
#' This sets \code{\link[=options]{options(repos)}}, overwriting the current
#' value. The previous value is returned, which can be helpful if you want to
#' set the repository only temporarily.
#'
#' @param repo_url the URL of the CRAN repository to download packages from.
#'
#' @return The previously repository URL, invisibly.
#'
#' @seealso [add_lib_path()] [options()] [install.packages()]
#'
#' @examples
#' current_repo <- set_repo("http://cran.stat.ucla.edu/")
#' set_repo(current_repo)
#'
#' @export

set_repo <- function(repo_url) {
    r <- getOption("repos")
    orig <- r["CRAN"]
    r["CRAN"] <- repo_url
    options(repos = r)
    return(invisible(orig))
}
