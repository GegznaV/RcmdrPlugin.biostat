# Workspace and Settings menu functions ======================================

# Working directory ----------------------------------------------------------
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_getwd <- function() {
    Rcmdr::doItAndPrint(paste(
        '# You are working in folder:',
        'getwd()',
        sep = " \n"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_setwd <- function() {
    new_wd <- tclvalue(tkchooseDirectory(initialdir = getwd(),
                                         parent = CommanderWindow()))
    if (new_wd != "") {
        Rcmdr::doItAndPrint(str_glue('setwd("{new_wd}")'))
    }
}


# Workspace ------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TODO:
#  Convert to window
#
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_list_objects <- function() {

    # Inputs (not implemented) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    show_hidden <- FALSE

    if (show_hidden) {
        all_names <- "all.names = TRUE"
    } else {
        all_names <- ""
    }

    # Code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    command <-  str_glue(
        "## List objects in R workspace \n",
        "objects({all_names})"
    )

    doItAndPrint(command)

}

# Session information --------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_session_info_devtools <- function() {
    Rcmdr::doItAndPrint(paste("# R session information \n",
                              "devtools::session_info()"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_session_info_utils <- function() {
    Rcmdr::doItAndPrint(paste("# R session information \n",
                              "sessionInfo()"))
}
