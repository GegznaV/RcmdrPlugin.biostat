# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Variable" menu related functions ===========================================

# Manage variables -----------------------------------------------------------


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_all_chr_to_fctr <- function() {
    Library("biostat")

    doItAndPrint(glue::glue(
        "{ ActiveDataSet()} <- biostat::all_chr_to_factor({ActiveDataSet()})"))

    command_dataset_refresh()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
