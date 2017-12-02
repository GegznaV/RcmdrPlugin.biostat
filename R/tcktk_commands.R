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