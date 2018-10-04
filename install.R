#!/usr/local/bin/Rscript

if (!suppressWarnings(suppressMessages(require("devtools")))) {
    install.packages("devtools")
    library("devtools")
}

document()
install(".")

