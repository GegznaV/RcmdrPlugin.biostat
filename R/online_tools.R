#' @rdname Menu-window-functions
#' @export
#' @keywords internal
copy_active_ds_to_clipboard <- function() {
    try(
        clipr::write_clip(get(ActiveDataSet(), envir = .GlobalEnv)),
        silent = TRUE)
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
open_online_tool <- function(url = NULL, copy_to_clipboard = FALSE) {
    checkmate::assert_string(url, null.ok = TRUE)
    if (!pingr::is_online()) {
        open_browser <- tclvalue_chr(
            RcmdrTkmessageBox(
                str_c(
                    "You are offline now, but this feature requires \n",
                    "an Internet connection. Open the tool in a web \n",
                    "browser anyway?"
                ),

                icon  = "warning",
                title = "No Internet Connection",
                type  = "yesno")
        )
    } else {
        choice <- "yes"
    }

    if (choice == "yes") {
        copy_active_ds_to_clipboard()
        browseURL(url = url)
    }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Online apps ------------ ---------------------------------------------------

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_geogebra_probability  <- function() {
    open_online_tool("https://www.geogebra.org/probability")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_mvn  <- function() {
    open_online_tool("http://www.biosoft.hacettepe.edu.tr/MVN/", TRUE)
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_easyROC  <- function() {
    open_online_tool("http://www.biosoft.hacettepe.edu.tr/easyROC/", TRUE)
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_tdROC  <- function() {
    open_online_tool("http://www.biosoft.hacettepe.edu.tr/tdROC/")
}


# Download ------------ ------------------------------------------------------

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_gpower  <- function() {
    open_online_tool("http://www.gpower.hhu.de/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_orange  <- function() {
    open_online_tool("https://orange.biolab.si//")
}


# Other ------------ ---------------------------------------------------------

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_plotly  <- function() {
    open_online_tool("https://plot.ly/ggplot2/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_ggplot2_ext <- function() {
    open_online_tool("http://www.ggplot2-exts.org/gallery/")
}

