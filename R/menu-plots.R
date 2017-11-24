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
# Create new window for plots
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_histogram <- function() {
    function_not_implemented("Function for Histograms")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create new window for plots
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_density_plot <- function() {
    function_not_implemented("Function for Density plots")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create new window for plots
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_plot_dot_err <- function() {
    function_not_implemented("Function for these plots")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create new window for plots
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_plot_bar_err <- function() {
    function_not_implemented("Function for these plots")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

