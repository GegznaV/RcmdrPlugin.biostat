tclvalue_lgl <- function(x) {
    # as.logical(as.integer(tclvalue(x)))
    as.logical(tclvalue_int(x))
}

tclvalue_int <- function(x) {
    sapply(unlist(strsplit(tclvalue(x), " ")), as.integer)
}

tclvalue_chr <- function(x) {
    sapply(unlist(strsplit(tclvalue(x), " ")), as.character)
}

s2u <- function(str) {
    gsub(" ", "_", str)
}

u2s <- function(str) {
    gsub("_", " ", str)
}