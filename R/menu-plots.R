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
# Create new window for plots
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_easyPlot <- function() {
    Library("easyPlot")

    command <- str_glue(
        '## To call tool "easyPlot" for making plots (the app),  \n',
        "## run the following code in either R or \n",
        "## RStudio console (and NOT in Rcmdr windows):\n\n",
        "# ", 'easyPlot::easyPlot("{ActiveDataSet()}")')
    logger(command)
    # justDoIt(command)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create new window for plots
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_ggplotgui <- function() {
    Library("ggplotgui")

    # system.file(package = "ggplotgui") %>% dir()
    # system.file(package = "descriptr", "application")
    # shiny::runApp(system.file(package = "descriptr", "application"),
    #               launch.browser = TRUE)

    command <- str_glue(
        '## To call tool "ggplotgui" for making plots (the app),  \n',
        "## run the following code in either R or RStudio console\n",
        "## (and NOT in Rcmdr windows).\n",
        '## In the code you copy form the app, change `ds`\n',
        '## into the name of your dataset (e.g., `{ActiveDataSet()}`)"\n\n',
        "# ", 'ggplotgui::ggplot_shiny({ActiveDataSet()})')

    logger(command)
    # justDoIt(command)
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
# Create color palette
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_colourPicker <- function() {
    # [!!!] This function should be updated for more interactive use.
    Library("colourpicker")

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

    command <- style_cmd(str_glue(
        "## The color palette you created:\n",
        '{palette_name} <- c({color_names2})'
    ))

    doItAndPrint(command)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create ...
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_histogram <- function() {
    function_not_implemented("Function for Histograms")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create ...
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_density_plot <- function() {
    function_not_implemented("Function for Density plots")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create ...
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_plot_dot_err <- function() {
    function_not_implemented("Function for these plots")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create ...
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_plot_bar_err <- function() {
    function_not_implemented("Function for these plots")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

