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

# New plot is drawn in a separare R window for plots
set_plots_to_separate_window <- function() {
    if (.Platform$OS.type == "windows") {
        options(device = "windows")
    } else {
        options(device = "X11")
        # options(device = "quartz") # For Mac
    }
}

# New plot is drawn in RStudio plots window
set_plots_to_rstudio_window <- function() {
    if (.Platform$GUI == "RStudio") {
        options(device = "RStudioGD")
    }

}

which_graphical_device <- function() {
    switch(
        options("device")$device,
        "windows"   = ,
        "X11"       = ,
        "quartz"    = "separate_window",
        "RStudioGD" = "RStudioGD",
        "unidentified"
    )
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
