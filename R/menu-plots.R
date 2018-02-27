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
window_easyPlot <- function() {
    library("easyPlot")

    command <- glue::glue('easyPlot::easyPlot("{ActiveDataSet()}")')
    logger(paste("#", command))
    justDoIt(command)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create new window for plots
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_ggplotgui <- function() {
    library("ggplotgui")

    command <- glue::glue("ggplotgui::ggplot_shiny({ActiveDataSet()})")
    logger(paste("#", command))
    justDoIt(command)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Visualize color names/codes as colors
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_show_colors <- function() {
    Library("scales")


    if (exists("my_palette") && is.character(my_palette)) {
        color_names <- my_palette
    } else {
        color_names <- c(5, "tomato3", "#45CC3B", "#6495ED")
    }

    # Code:
    color_names <- paste0('"', color_names, '"', collapse = ", ")
    command <- style_cmd(glue::glue('scales::show_col(c({color_names}))'))

    # logger("# This is just an example of code")
    # logger(command)
    logger(paste("# This is just an example of code:",
                 command,
                 sep = "\n"))

    justDoIt(command)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create color palette
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_colourPicker <- function() {
    library("colourpicker")

    color_names <- NULL
    color_names <- colourpicker::colourPicker()

    # If cancelled
    if (is.null(color_names)) {
        return()
    }

    # Code:
    color_names2 <- paste0('"', color_names, '"', collapse = ", ")

    # [!!!] Name should be changeable:
    palette_name <- "my_palette"

    command <- style_cmd(glue::glue('{palette_name} <- c({color_names2})'))

    logger(paste("# Your color palette:",
                 command,
                 sep = "\n"))

    justDoIt(command)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create ...
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_histogram <- function() {
    function_not_implemented("Function for Histograms")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create ...
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_density_plot <- function() {
    function_not_implemented("Function for Density plots")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create ...
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_plot_dot_err <- function() {
    function_not_implemented("Function for these plots")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create ...
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_plot_bar_err <- function() {
    function_not_implemented("Function for these plots")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

