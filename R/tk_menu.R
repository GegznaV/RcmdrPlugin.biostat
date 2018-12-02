# replacement for standard tkmenu() to play better with ttk themes
#  courtesy of Philippe Grosjean

tk_menu <- function(parent, activebackground, activeforeground, ...) {
    if (!is.ttk()) {
        stop("Tcl/Tk >= 8.5 is required")
    }
    w <- tkwidget(parent, "menu", ...)
    if (missing(activebackground))
        activebackground <- tk2style("tk2button", "selectbackground")

    if (activebackground == "")
        activebackground <- "darkblue" # Default value

    if (missing(activeforeground))
        activeforeground <- tk2style("tk2button", "selectforeground")

    if (activeforeground == "")
        activeforeground <- "white" # Default value

    tkconfigure(w,
                activebackground = activebackground,
                activeforeground = activeforeground)

    class(w) <- c("tk2menu", "tk2widget", class(w))
    return(w)
}