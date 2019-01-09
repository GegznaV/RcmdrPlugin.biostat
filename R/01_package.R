#' @name RcmdrPlugin.biostat
#' @docType package
#'
#' @title R Commander Plug-in for Basic Data Management and Analysis Tasks
#'
#' @description
#' \pkg{RcmdrPlugin.biostat} is an
#' \href{\pkg{Rcmdr}}{https://cran.r-project.org/web/packages/Rcmdr/index.html}
#' plug-in for the most common data wrangling, visualisation and analysis tasks
#' using "tidyverse" family functions as well as functions from other packages.
#'
#' @rawNamespace import(tcltk, except = c(tkfocus, tclvalue, ttkentry))
#' @import Rcmdr
#' @import tcltk2
#' @import stringr
#' @import purrr
#' @import nortest
#' @importFrom glue glue
#' @importFrom dplyr glimpse is_grouped_df
#' @importFrom magrittr "%>%"  "%<>%"  "%T>%"  "%$%"
#' @importFrom graphics plot
#' @importFrom stats p.adjust.methods C
#' @importFrom utils browseURL packageVersion data installed.packages modifyList

# @importFrom Rcmdr activeDataSet
# @import biostat
# @import RcmdrPlugin.EZR

NULL


biostat_env <- new.env()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.onAttach <- function(libname, pkgname) {
    if (!interactive()) {
        return()
    }

    # Current options
    Rcmdr_opts <- options()$Rcmdr

    # If empty, convert to named list
    if (is.null(Rcmdr_opts)) {
        Rcmdr_opts <- list(plugins = NULL)
    }

    # TODO: getRcmdr("messages.height")  <----------- [???]

    # Plugins to add
    add_plugins <- "RcmdrPlugin.biostat"

    # Add plugins in certain order
    plugins <- c(
        setdiff(Rcmdr_opts$plugins, add_plugins),
        rev(sort(add_plugins))
    )

    # Open 3-window Rcmdr, if options is not defined
    if (is.null(Rcmdr_opts$console.output)) {
        console.output <- FALSE

    } else {
        console.output <- Rcmdr_opts$console.output
    }

    updated_opts <-
        utils::modifyList(
            Rcmdr_opts,
            list(plugins = plugins, console.output = console.output)
        )

    if (!identical(Rcmdr_opts, updated_opts)) {
        # Set new options and restart R Commander
        options(Rcmdr = updated_opts)

        if (!"package:Rcmdr" %in% search()) {
            Rcmdr::Commander()
        } else {
            if (!Rcmdr::getRcmdr("autoRestart")) {
                Rcmdr::closeCommander(ask = FALSE, ask.save = TRUE)
                Rcmdr::Commander()
            }
        }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create icons

    bs_tkimage_create <- function(name, file, package = "RcmdrPlugin.biostat") {
        tcltk::tkimage.create(
            "photo", name,
            file = system.file("etc", file, package = package))
    }

    bs_tkimage_create("::image::bs_dataset"  , "icons/list.png")
    bs_tkimage_create("::image::bs_model"    , "icons/model.png")

    bs_tkimage_create("::image::bs_locale"   , "icons/locale.png")
    bs_tkimage_create("::image::bs_down"     , "icons/down-blue.png")
    bs_tkimage_create("::image::bs_delete"   , "icons/delete.png")
    bs_tkimage_create("::image::bs_paste"    , "icons/paste.png")
    bs_tkimage_create("::image::bs_refresh"  , "icons/refresh.png")
    bs_tkimage_create("::image::bs_file_open", "icons/document-open.png")

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
restart_commander <- function() {
    Rcmdr::closeCommander(ask = FALSE, ask.save = TRUE)
    Rcmdr::Commander()
}

