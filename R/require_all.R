#' Install and load packages
#'
#' `require_all` installs and loads a list of packages.
#'
#' `require_all` takes a list of packages, installs from CRAN those that are not
#' installed, and loads them all. It produces an error if it could not
#' successfully load all packages.
#'
#' By default, `require_all` will print logging information to the console with
#' [`print`], but this can be changed either with `silent = TRUE` to disable all
#' output, or with `logger = mylogger` to pass the logging output to `mylogger`.
#'
#' The repository location for downloading new packages should be set, which can
#' be done with [`set_repo`]. If no such repository is set, a prompt will open
#' asking to choose the repository. A valid library location for installing new
#' packages should also be set, which can be done with [`add_lib_path`]. If no
#' such library is available, a prompt may ask for permission to create one in
#' the current user's home directory.
#'
#' @param ... packages to load. Can be individual character vectors, or lists,
#'   or vectors, or any combination of them. Duplicate elements are ignored.
#' @param logger function taking a single character string. The function to call
#'   when printing output.
#' @param silent logical. If `TRUE`, this function will produce no output except
#'   errors. If `FALSE`,
#'
#' @return \code{\link[=invisible]{invisible(TRUE)}}
#'
#' @seealso [set_repo()] [add_lib_path()] [install.packages()] [library()]
#'
#' @examples
#' require_all(c("base", "stats"))
#'
#' \dontrun{
#' # Custom logging function
#' logit <- function(s) { sprintf("[%s] %s\n", Sys.time(), s) }
#' require_all(c("rgdal", "raster", "doMPI"), logger = logit)
#' }
#'
#' @export

## Dev notes:
##
## See the following for how we install missing:
## https://stackoverflow.com/a/4090208
## See the following for how to set the package library to a
## writable location:
## https://stackoverflow.com/a/15170774
## See the following for how we suppress output:
## https://stackoverflow.com/q/14834841
##
## TODO: Super-silent switch, catching errors and returning FALSE instead.
##        i.e. use "require" instead of "library"
##
## Jamie St Martin
## 2018-10-03

require_all <- function(..., logger = print, silent = FALSE) {
    packages <- unique(c(..., recursive = TRUE))
    if (length(packages) > 0) {
        logger_func <- make_logger_func(logger, silent)
        install_missing(packages, logger_func)
        load_packages(packages, logger_func)
    }
    return(invisible(TRUE))
}


# Non-exported function. Convenience function used internally to create a
# function for logging a message. Uses a given logging function, and only
# does anything if silent is FALSE.
make_logger_func <- function(logger, silent) {
    function(...) {
        if (!silent) {
            msg <- paste(c(...), collapse = " ")
            logger(msg)
        }
    }
}

# Non-exported function. Installs packages that are not installed.
install_missing <- function(packages, logger_func) {
    logger_func("Installing missing packages from:", packages)
    new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
    if (length(new_packages) > 0) {
        logger_func("Installing new packages:", new_packages)
        try(install.packages(new.packages), silent = TRUE)
        logger_func("Installed new packages:", new_packages)
    }
    logger_func("All packages installed:", packages)
    return(invisible(TRUE))
}

# Non-exported function. Loads packages; errors if they cannot be loaded.
load_packages <- function(packages, logger_func) {
    logger_func("Loading packages:", packages)
    for (p in packages) {
        suppressPackageStartupMessages(library(p, character.only = TRUE, quietly = TRUE))
    }
    logger_func("All packages loaded:", packages)
    return(invisible(TRUE))
}
