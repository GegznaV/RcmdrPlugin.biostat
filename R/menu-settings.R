# Create new window for plots
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_load_packages <- function() {
    Rcmdr::doItAndPrint(paste0(
        "library(tidyverse) \n",
        "library(biostat)   \n",
        "library(magrittr)  \n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_getwd <- function() {
    Rcmdr::doItAndPrint(paste(
        '# You are working in folder:',
        'getwd()',
        sep = " \n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_get_locale <- function() {
    Rcmdr::doItAndPrint(paste(
        '# Current locale:  \n',
        'locale_info <- Sys.getlocale()',
        'writeLines(gsub(";", "\\n", locale_info))',
        sep = " \n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_set_locale_lt <- function() {
    # [!!!] Check if works in non-Windows.

    locale <- if (.Platform$OS.type == "windows") {
        "Lithuanian"
    } else {
        "lt_LT"
    }

    Rcmdr::doItAndPrint(glue::glue(
        '# Set locale to Lithuanian \n',
        'locale_info <- Sys.setlocale(locale = "{locale}")\n',
        'writeLines(gsub(";", "\\n", locale_info))\n'))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_set_locale_en <- function() {
    # [!!!] Check if works in non-Windows.
    locale <- if (.Platform$OS.type == "windows") {
        "English"
    } else {
        "en_US"
    }

    Rcmdr::doItAndPrint(glue::glue(
        '# Set locale to English \n',
        'locale_info <- Sys.setlocale(locale = "{locale}")\n',
        'writeLines(gsub(";", "\\n", locale_info))\n'))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_set_locale_ru <- function() {
    # [!!!] Check if works in non-Windows.
    locale <- if (.Platform$OS.type == "windows") {
        "Russian"
    } else {
        "ru_RU"
    }

    Rcmdr::doItAndPrint(glue::glue(
        '# Set locale to Russian \n',
        'locale_info <- Sys.setlocale(locale = "{locale}")\n',
        'writeLines(gsub(";", "\\n", locale_info))\n'))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_set_locale_default <- function() {
    Rcmdr::doItAndPrint(paste(
        '# Set locale to system default \n',
        'locale_info <- Sys.setlocale(locale = "")',
        'writeLines(gsub(";", "\\n", locale_info))',
        sep = " \n"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_session_info_devtools <- function() {
    Rcmdr::doItAndPrint(paste("# R session information \n",
                              "devtools::session_info()"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_session_info_utils <- function() {
    Rcmdr::doItAndPrint(paste("# R session information \n",
                              "sessionInfo()"))
}
