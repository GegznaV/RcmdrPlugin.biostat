

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
open_online_tool <- function(url = NULL, copy_to_clipboard = FALSE,
                             parent = CommanderWindow()) {
    checkmate::assert_string(url, null.ok = TRUE)
    checkmate::assert_logical(copy_to_clipboard)

    if (!pingr::is_online()) {
        open_browser <-
            tk_messageBox(
                parent = parent,
                message = str_c(
                    "This feature requires an Internet connection but your\n",
                    "computer is offline now. Do you want to open the tool\n",
                    "or website in a web browser anyway?"
                ),
                icon  = "warning",
                caption = "No Internet Connection",
                type  = "yesno",
                default = "no")

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

# Online apps ----------------------------------------------------------------

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_geogebra_probability  <- function() {
    open_online_tool("https://www.geogebra.org/probability")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_statpages  <- function(variables) {
    open_online_tool("http://statpages.info/")
}

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
    open_online_tool("http://www.biosoft.hacettepe.edu.tr/MVN/")                 # broken
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_easyROC  <- function() {
    open_online_tool("http://www.biosoft.hacettepe.edu.tr/easyROC/")             # broken
}


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_tdROC  <- function() {
    open_online_tool("http://www.biosoft.hacettepe.edu.tr/tdROC/")               # broken
}





# Download -------------------------------------------------------------------

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_r_project  <- function() {
    open_online_tool("https://www.r-project.org/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_mro  <- function() {
    open_online_tool("https://mran.microsoft.com/download/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_rstudio  <- function() {
    open_online_tool("https://www.rstudio.com/products/rstudio/download/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_bert <- function() {
    open_online_tool("https://bert-toolkit.com/")
}

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
    open_online_tool("https://www.geogebra.org/download") # must be without /
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_past_download  <- function() {
    open_online_tool("https://folk.uio.no/ohammer/past/")
}


# R extensions ---------------------------------------------------------------


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_tidyverse <- function() {
    open_online_tool("https://www.tidyverse.org/packages/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_broom <- function() {
    open_online_tool("https://broom.tidyverse.org/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_dplyr <- function() {
    open_online_tool("https://dplyr.tidyverse.org/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_forcats <- function() {
    open_online_tool("https://forcats.tidyverse.org/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_magrittr <- function() {
    open_online_tool("https://magrittr.tidyverse.org/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_purrr <- function() {
    open_online_tool("https://purrr.tidyverse.org/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_readr <- function() {
    open_online_tool("https://readr.tidyverse.org/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_readxl <- function() {
    open_online_tool("https://readxl.tidyverse.org/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_stringr <- function() {
    open_online_tool("https://stringr.tidyverse.org/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_tidyr <- function() {
    open_online_tool("https://tidyr.tidyverse.org/")
}

# Plotting -------------------------------------------------------------------

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_ggplot2 <- function() {
    open_online_tool("https://ggplot2.tidyverse.org/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_ggstatsplot  <- function() {
    open_online_tool("https://indrajeetpatil.github.io/ggstatsplot/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_ggpubr  <- function() {
    open_online_tool("https://rpkgs.datanovia.com/ggpubr/")
}

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

# News and tutorials ---------------------------------------------------------

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_r_chearsheets  <- function() {
    open_online_tool("https://www.rstudio.com/resources/cheatsheets/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_sthda  <- function() {
    open_online_tool("http://www.sthda.com/english/")
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
window_online_rstudio_learning  <- function() {
    open_online_tool("https://www.rstudio.com/online-learning/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_isl  <- function() {
    open_online_tool("http://www-bcf.usc.edu/~gareth/ISL/")
}
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_esl  <- function() {
    open_online_tool("https://web.stanford.edu/~hastie/ElemStatLearn/")
}


# Books ----------------------------------------------------------------------

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_bookdown <- function() {
    open_online_tool("https://bookdown.org/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_biostathandbook <- function() {
    open_online_tool("http://www.biostathandbook.com/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_rcompanion <- function() {
    open_online_tool("http://rcompanion.org/rcompanion/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_rcompanion_handbook <- function() {
    open_online_tool("http://rcompanion.org/handbook/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_r4ds <- function() {
    open_online_tool("https://r4ds.had.co.nz/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_advanced_r_2ed <- function() {
    open_online_tool("https://adv-r.hadley.nz/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_rmd <- function() {
    open_online_tool("https://bookdown.org/yihui/rmarkdown/")
}


# Forums ---------------------------------------------------------------------

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


# About ----------------------------------------------------------------------

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_homepage <- function() {
    open_online_tool("https://gegznav.github.io/RcmdrPlugin.biostat/")
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_online_bug_report <- function() {
    open_online_tool("https://github.com/GegznaV/RcmdrPlugin.biostat/issues")
}


