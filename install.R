#!/usr/local/bin/Rscript

# install.R
# Installs the `devtools` package if it isn't already installed, then
# uses `devtools` to installs the jstm package. Note that this may
# open a prompt requesting the user to choose which CRAN mirror to
# use, and may also prompt for a library location. Use in automated
# scripts at your own risk!
# Execute with `Rscript install.R`

if (!suppressWarnings(suppressMessages(require("devtools")))) {
    install.packages("devtools")
    library("devtools")
}

devtools::document()
devtools::install(".")

