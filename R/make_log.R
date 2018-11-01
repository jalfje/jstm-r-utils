#' Create a logging function
#'
#' `make_log` returns a function ready for logging to a file and/or stdout.
#'
#' `make_log` returns a closure which formats and logs an arbitrary number of
#' arguments. It prepends the current time to each line, and concatenates the
#' arguments together to create a single line of output. The output line is then
#' output to a file and/or stdout (i.e. the console) via `cat`.
#'
#' For the purpose of clarity throughout this documentation, we will use
#' `logit()` to represent the function returned from `make_log`, i.e. `logit <-
#' make_log()`.
#'
#' After creating `logit`, you can log your data by using `logit("my", vector,
#' list, "or other data")`.
#'
#' The `newline` option in `logit` allows for logging multiple times on the same
#' output line. The idea behind this is to allow for logging within a loop to
#' log in each iteration as they occur, somewhat like a progress bar. As shown
#' in the example, this is quite straightforward. Note that the timestamp will
#' show only the time of the first such call.
#'
#' In each call to `logit`, if the parameter `echo` is `TRUE`, then the output
#' will be printed to stdout. If the parameter `save` is `TRUE`, then the output
#' will be appended to the file specified by the parameter `file` passed to
#' `make_log`. The default value for `echo` is the value of `to_stdout`, and the
#' default value for `save` is `to_file`. Specifying values for `echo` or `save`
#' will override these defaults. Note that if `to_file` is `FALSE`, then setting
#' `save` will have no effect; if a logging file is ever desired, it must be
#' specified when `make_log` is called.
#'
#' @param to_stdout logical. If `TRUE`, output is printed to console at each
#'   call to `logit` by default. The `echo` parameter can override this.
#' @param to_file logical. If `TRUE`, output is printed to `file` at each call
#'   to `logit` by default. The `save` parameter can override this, but only if
#'   `to_file` is `TRUE`.
#' @param file a character vector naming the logging file. If `to_file` is
#'   `FALSE`, this parameter is ignored.
#'
#' @return Logging function
#'
#' @examples
#' logit <- make_log(TRUE, TRUE, "output.log")
#' logit("Hello world!")
#' logit("Starting for loop. 12 iterations.")
#' for(m in 1:12) { logit(m, newline = FALSE) }
#' logit("Ended for loop.")
#' # [12:50:42] Hello world!
#' # [12:50:42] Starting for loop. 12 iterations.
#' # [12:50:42] 1 2 3 4 5 6 7 8 9 10 11 12
#' # [12:50:42] Ended for loop.
#'
#' @export

## Dev notes:
##
## make_log is a closure around the logging function.
## See the following for an intro to closures:
## https://stackoverflow.com/a/1088800
## http://adv-r.had.co.nz/Functional-programming.html#closures
##
## Writing to file could perhaps be faster using file(), open(), and close(),
## but I figured that won't be necessary because it's not geared towards
## performance.
##
## TODO: Add functionality to customize the time format string
##
## Jamie St Martin
## 2018-10-03

make_log <- function(to_stdout = TRUE, to_file = FALSE, file = NA) {
    # Create file, if requested; error if file creation fails.
    if (to_file) {
        tryCatch({
            file.create(file)
        }, warning = function(s) {
            stop(c("Error in creating logging file: ", conditionMessage(w)))
        }, error = function(e) {
            stop(c("Error in creating logging file: ", conditionMessage(e)))
        })
    }

    prev_newline <- TRUE
    cur_newline <- TRUE
    # Create logging function
    log_func <- function(..., echo = to_stdout, save = to_file, newline = TRUE, sep = " ") {
        # Deal with newlines & formatting
        prev_newline <<- cur_newline
        cur_newline <<- newline
        if (cur_newline && prev_newline) {
            out <- sprintf("[%s] %s\n", format(Sys.time(), "%T"), pastec(..., collapse = sep))
        } else if (cur_newline && !prev_newline) {
            out <- sprintf("\n[%s] %s\n", format(Sys.time(), "%T"), pastec(..., collapse = sep))
        } else if (!cur_newline && prev_newline) {
            out <- sprintf("[%s] %s", format(Sys.time(), "%T"), pastec(..., collapse = sep))
        } else { #(!cur_newline && !prev_newline)
            out <- sprintf("%s%s", sep, pastec(..., collapse = sep))
        }
        # Print output & return invisibly
        if (echo) cat(out)
        if (save && to_file) cat(out, file = file, append = TRUE)
        return(invisible(TRUE))
    }
    logit <- log_func
    logit("Logging initiated.")
    return(logit)
}

