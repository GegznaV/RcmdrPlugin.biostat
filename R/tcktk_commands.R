# state_disabled <- function(widget) {
#     tkconfigure(widget, state = "disabled")
# }
#
# active <- function(widget) {
#     tkconfigure(widget, state = "active")
# }
#
# normal <- function(widget) {
#     tkconfigure(widget, state = "normal")
# }

tclvalue_lgl <- function(x) {
    # as.logical(as.integer(tclvalue(x)))
    as.logical(tclvalue_int(x))
}

tclvalue_int <- function(x) {
    x <- sapply(unlist(strsplit(tclvalue(x), " ")), as.integer)
    names(x) <- NULL
    x
}

tclvalue_chr <- function(x) {
    sapply(unlist(strsplit(tclvalue(x), " ")), as.character)
    names(x) <- NULL
    x
}

s2u <- function(str) {
    gsub(" ", "_", str)
}

u2s <- function(str) {
    gsub("_", " ", str)
}