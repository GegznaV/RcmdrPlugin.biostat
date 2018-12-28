

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
open_online_tool <- function(url = NULL, copy_to_clipboard = FALSE) {
    checkmate::assert_string(url, null.ok = TRUE)
    checkmate::assert_logical(copy_to_clipboard)

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
        if (isTRUE(copy_to_clipboard)) {
            export_to_clipboard_active_ds_tab()
        }
        browseURL(url = url)
    }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Online apps ------------ ---------------------------------------------------

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_geogebra_probability  <- function() {
    open_online_tool("https://www.geogebra.org/probability/")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_image_digitizer  <- function(variables) {
    open_online_tool("https://apps.automeris.io/wpd/")
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
    open_online_tool("https://orange.biolab.si/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_geogebra_download  <- function() {
    open_online_tool("https://www.geogebra.org/download/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_past_download  <- function() {
    open_online_tool("https://folk.uio.no/ohammer/past/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_bert <- function() {
    open_online_tool("https://bert-toolkit.com/")
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

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_tidyverse <- function() {
    open_online_tool("https://www.tidyverse.org/")
}


# News and tutorials ------------ ---------------------------------------------

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_sthda  <- function() {
    open_online_tool("http://www.sthda.com/english/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_bookdown <- function() {
    open_online_tool("https://bookdown.org/")
}


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_rweekly  <- function() {
    open_online_tool("https://rweekly.org/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_r_bloggers  <- function() {
    open_online_tool("https://www.r-bloggers.com/")
}


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_r_project  <- function() {
    open_online_tool("https://www.r-project.org/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_rstudio_learning  <- function() {
    open_online_tool("https://www.rstudio.com/online-learning/")
}





#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_stackoverflow  <- function() {
    open_online_tool("https://stackoverflow.com/questions/tagged/r")
}


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_crossvalidated  <- function() {
    open_online_tool("https://stats.stackexchange.com/questions/tagged/r")
}
