# "Plots" menu related functions ===========================================

# Create new window for plots
#' @rdname Menu-window-functions
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
# Visualize color names/codes as colors
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_show_colors <- function() {
    # [!!!] This function should be updated for more interactive use.
    Library("scales")

    if (exists("my_palette") && is.character(my_palette)) {
        color_names <- my_palette
    } else {
        color_names <- c(5, "tomato3", "#45CC3B", "#6495ED")
    }

    # Code:
    color_names <- paste0('"', color_names, '"', collapse = ", ")

    command <- style_cmd(str_glue(
        "## This is just an example of code to show colors. \n",
        "## Please write either color names or color codes of inerest: \n\n",
        'scales::show_col(c({color_names}))'))

    doItAndPrint(command)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_plots_image_digitizer <- function() {
    # digitizeR::wpd.launch()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_plots_ggplotly <- function() {
    command <- str_c(
        "## Convert the last ggplot to an interactive plot.\n",
        'plotly::ggplotly()')

    doItAndPrint(command)
}
