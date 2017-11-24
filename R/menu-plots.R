# "Plots" menu related functions ===========================================

# Create new window for plots
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
open_new_plots_window <- function() {
    if (.Platform$OS.type == "windows") {
        grDevices::windows()
    } else {
        grDevices::X11()
    }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

