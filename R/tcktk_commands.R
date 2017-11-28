tclvalue_lgl <- function(x) {
    as.logical(as.integer(tclvalue(x)))
}

tclvalue_int <- function(x) {
    sapply(unlist(strsplit(tclvalue(x), " ")), as.integer)
}
