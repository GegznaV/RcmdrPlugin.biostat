# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Variable" menu related functions ===========================================

# Manage variables -----------------------------------------------------------


#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
command_all_chr_to_fctr <- function() {
    Library("BioStat")

    doItAndPrint(glue::glue(
        "{ ActiveDataSet()} <- BioStat::all_chr_to_factor({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
