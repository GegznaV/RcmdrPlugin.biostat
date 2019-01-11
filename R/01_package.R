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

    bs_tkimage_create("::image::bs_r_logo_g"  , "icons/R-logo-g.png")
    bs_tkimage_create("::image::bs_r_logo_g2" , "icons/R-logo-g.gif")
    bs_tkimage_create("::image::bs_green"     , "icons/user-online-2.png")
    bs_tkimage_create("::image::bs_green_r_24", "icons/speech-balloon-green-r-icon-24.png")
    bs_tkimage_create("::image::bs_green_r_32", "icons/speech-balloon-green-r-icon-32.png")
    bs_tkimage_create("::image::bs_green_r_64", "icons/speech-balloon-green-r-icon-64.png")
    bs_tkimage_create("::image::bs_green_r_48", "icons/speech-balloon-green-r-icon-48.png")
    bs_tkimage_create("::image::bs_green_r_72", "icons/speech-balloon-green-r-icon-72.png")

    bs_tkimage_create("::image::bs_dataset"  , "icons/list.png")
    bs_tkimage_create("::image::bs_model"    , "icons/model.png")

    bs_tkimage_create("::image::bs_locale"   , "icons/locale.png")
    bs_tkimage_create("::image::bs_down"     , "icons/down-blue.png")
    bs_tkimage_create("::image::bs_delete"   , "icons/delete.png")
    bs_tkimage_create("::image::bs_paste"    , "icons/edit-paste-7.png")
    bs_tkimage_create("::image::bs_copy"     , "icons/edit-copy-7.png")
    bs_tkimage_create("::image::bs_cut"      , "icons/edit-cut-7.png")
    bs_tkimage_create("::image::bs_refresh"  , "icons/view-refresh-6.png")
    bs_tkimage_create("::image::bs_open_file", "icons/document-open.png")
    bs_tkimage_create("::image::bs_open_dir",  "icons/document-open-folder.png")
    bs_tkimage_create("::image::bs_open",      "icons/document-open-2.png")

    bs_tkimage_create("::image::bs_import",    "icons/document-import-2.png")
    bs_tkimage_create("::image::bs_export",    "icons/document-export-4.png")
    bs_tkimage_create("::image::bs_new",       "icons/window-new.png")
    bs_tkimage_create("::image::bs_new_doc",   "icons/document-new-3.png")
    bs_tkimage_create("::image::bs_to_console","icons/transform-shear-down.png")
    bs_tkimage_create("::image::bs_preview",   "icons/document-preview.png")

    # bs_tkimage_create("::image::bs_analyse",   "icons/calculator-edit.png")
    bs_tkimage_create("::image::bs_analyse",   "icons/application-view-tile.png")
    bs_tkimage_create("::image::bs_summary",   "icons/view-statistics.png")
    bs_tkimage_create("::image::bs_summary2",  "icons/insert-numbers.png")
    bs_tkimage_create("::image::bs_summary3",  "icons/kdb_table.png")
    bs_tkimage_create("::image::bs_plot",      "icons/office-chart-pie.png")
    bs_tkimage_create("::image::bs_plot2",     "icons/insert-chart.png")
    bs_tkimage_create("::image::bs_plot3",     "icons/insert-chart-bar.png")

}

# "::image::bs_locale"
# "::image::bs_down"
# "::image::bs_delete"
# "::image::bs_paste"
# "::image::bs_copy"
# "::image::bs_cut"
# "::image::bs_open_file"
# "::image::bs_open"
#
# "::image::bs_refresh"
# "::image::bs_import"
# "::image::bs_export"
#
# "::image::bs_new"
#
# "::image::bs_preview"
# "::image::bs_plot"
#
# "::image::bs_summary"
# "::image::bs_chart"
# "::image::bs_analyse"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
restart_commander <- function() {
    Rcmdr::closeCommander(ask = FALSE, ask.save = TRUE)
    Rcmdr::Commander()
}

