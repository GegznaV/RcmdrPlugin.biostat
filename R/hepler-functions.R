list_summaries_Models <- function(envir = .GlobalEnv, ...) {
    objects <- ls(envir = envir, ...)
    if (length(objects) == 0)
        NULL
    else objects[sapply(objects,
                        function(.x) "summaries_model" == (class(get(.x, envir = envir))[1]))]
}

# ------------------------------------------------------------------------------
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
function_not_implemented <- function(x = NULL) {

    doItAndPrint("# ~~~ Not implemented yet! ~~~\n")

    if (is.null(x)) {
        x <- "This function"
    }

    text <- glue("# ~~~ {x} will be implemented  \n ",
                 "# ~~~ in the future versions of package `RcmdrPlugin.BioStat`! ")

    msg <- glue("{x} will be implemented in the future versions of package",
                " `RcmdrPlugin.BioStat`! ")

    doItAndPrint(text)
    Message(msg, type = "warning")
}
# ------------------------------------------------------------------------------
spaces <- function(n) {
    paste0(rep(" ", length = n), collapse = "")
}
# ------------------------------------------------------------------------------
nonFactorsP <- function(n = 1) {
    #  n - number of non-factors.
    activeDataSetP() && length(setdiff(listVariables(), listFactors())) >= n
}
# ------------------------------------------------------------------------------
glue <- glue::glue
# ------------------------------------------------------------------------------
eval_glue <- function(..., envir = parent.frame(),
                      .sep = "", .open = "{", .close = "}") {

    x2 <- glue::glue(..., .envir = envir, .open = .open, .close = .close)
    eval(parse(text = x2), envir = envir)
}
# ------------------------------------------------------------------------------
eval_ <- function(x, envir = parent.frame(), ...) {
    eval(parse(text = x), envir = envir, ...)
}
# ------------------------------------------------------------------------------
gettext_Bio <- function(...) {
    gettext(..., domain = "R-RcmdrPlugin.BioStat")
}
# ------------------------------------------------------------------------------
#' Does data contain characters?
#'
#' Return TRUE, if at least n character variables exist in the active dataset.
#'
#' @param n Minimum number of character variables
#'
#' @keywords internal
#' @export
characterP <- function(n = 1) {
    activeDataSetP() &&
        (sum(eval_glue("mapply(is.character, {activeDataSet()})")) >= n)
}
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#' Is the first class "data.frame"
#'
#' Check if the first class of active Rcmdr dataset is "data.frame"
#' @keywords internal
#' @export
first_class_is_dataframeP <- function() {
    activeDataSetP() &&
        (eval_glue("class({activeDataSet()})[1]") == "data.frame")
}
# ------------------------------------------------------------------------------
#' Chech the class of the active model in Rcmdr
#'
#' @param class_ (string) a character vector of length 1. The name of class.
#'
#' @keywords internal
#' @export
modelClassP <- function(class_) {
    activeModelP() && (inherits(
        x = get(ActiveModel(), envir = .GlobalEnv),
        what = class_))
}
# ------------------------------------------------------------------------------
#' Make path to relative
#'
#' Make absolute path into relative one in respect to current working directory.
#'
#' @param str (character) Sting (or vector of strings) with absolute path.
#'
#' @keywords internal
#' @export
make_relative_path <- function(str) {
    sub(paste0(getwd(), "/?"), "", str)
}

#' Extract file parts.
#' @name extract-fileparts
#' @param str (character) Path to file (with filename and extension).
#'
#' @keywords internal
#' @export
extract_filename <- function(str) {
    sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", str)
}
#' @rdname extract-fileparts
#' @keywords internal
#' @export
extract_path <- function(str) {
    sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\1", str)
}
#' @rdname extract-fileparts
#' @keywords internal
#' @export
extract_extension <- function(str) {
    sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\3", str)
}

