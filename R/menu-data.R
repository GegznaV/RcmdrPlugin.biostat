#' @name Menu-window-functions
#' @title RcmdrPlugin.biostat functions for menus and windows.
#' @description Functions that open Rcmdr menus and windows windows.
#' @keywords internal
NULL

# ============================================================================
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_list_objects <- function() {

    # Inputs (not implemented) -----------------------------------------------
    show_hidden <- FALSE

    if (show_hidden) {
        all_names <- "all.names = TRUE"
    } else {
        all_names <- ""
    }

    # Code -------------------------------------------------------------------
    command <-  str_glue(
        "## List objects in R workspace \n",
        "objects({all_names})"
        )

    doItAndPrint(command)

}