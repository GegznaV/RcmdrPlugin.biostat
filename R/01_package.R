#' @name RcmdrPlugin.biostat
#' @docType package
#'
#' @title R Commander Plug-in for Basic Data Management and Analysis Tasks
#'
#' @description
#' \pkg{RcmdrPlugin.biostat} is an
#' \href{\pkg{Rcmdr}}{https://CRAN.R-project.org/package=Rcmdr}
#' plug-in for the most common data wrangling, visualisation and analysis tasks
#' using "tidyverse" family functions as well as functions from other packages.
#'
#' @rawNamespace import(tcltk, except = c(tkfocus, tclvalue, ttkentry))
#' @import tcltk2
#' @import Rcmdr
#' @import stringr
#' @import purrr
#' @import nortest
#' @import ggplot2
#' @importFrom dplyr glimpse is_grouped_df
#' @importFrom magrittr "%>%"  "%<>%"  "%T>%"  "%$%"
#' @importFrom graphics plot
#' @importFrom stats p.adjust.methods C setNames
#' @importFrom utils browseURL packageVersion data installed.packages modifyList
#'                   capture.output tail
#' @importFrom RcmdrPlugin.EZR
#'    StatMedCorrelation
#'    StatMedSpearman
#'    StatMedSingleSampleTTest
#'    StatMedWilSign
#'    StatMedPairedTtest
#'    StatMedRepANOVA
#'    StatMedFriedman
#'    StatMedTtest
#'    StatMedMannW
#'    StatMedANOVA
#'    StatMedKruWalli
#'    StatMedJT
#'    StatMedMultiANOVA
#'    StatMedANCOVA
#'    StatMedFTest
#'    StatMedBartlett
#'    StatMedLinearRegression
#' @importFrom grDevices devAskNewPage


NULL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variables
biostat_env <- new.env()
biostat_env$use_relative_path <- TRUE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions
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

        # if (!"package:Rcmdr" %in% search()) {
        #     Rcmdr::Commander()
        #
        # } else {
        #     if (!isTRUE(Rcmdr::getRcmdr("autoRestart", fail = FALSE))) {
        #         Rcmdr::closeCommander(ask = FALSE, ask.save = TRUE)
        #         Rcmdr::Commander()
        #     }
        # }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create icons

    bs_tkimage_create <- function(name, file, package = "RcmdrPlugin.biostat") {
        tcltk::tkimage.create(
            "photo", name,
            file = system.file("etc", file, package = package))
    }

    bs_tkimage_create("::image::bs_r_logo_g"  , "icons/R-logo-g.png")
    bs_tkimage_create("::image::bs_r"         , "icons/r-logo-16x16.png")

    bs_tkimage_create("::image::bs_dataset"  , "icons/list.png")
    bs_tkimage_create("::image::bs_model"    , "icons/model.png")
    bs_tkimage_create("::image::bs_locale"   , "icons/locale.png")

    bs_tkimage_create("::image::bs_down"     , "icons/down-blue.png")
    bs_tkimage_create("::image::bs_delete"   , "icons/delete.png")
    bs_tkimage_create("::image::bs_paste"    , "icons/edit-paste-7.png")
    bs_tkimage_create("::image::bs_copy"     , "icons/edit-copy-7.png")
    bs_tkimage_create("::image::bs_rename"   , "icons/textfield-rename.png")
    bs_tkimage_create("::image::bs_cut"      , "icons/edit-cut-7.png")
    bs_tkimage_create("::image::bs_undo"     , "icons/edit-undo-7.png")
    bs_tkimage_create("::image::bs_redo"     , "icons/edit-redo-7.png")
    bs_tkimage_create("::image::bs_find"     , "icons/edit-find-7.png")
    bs_tkimage_create("::image::bs_refresh"  , "icons/refresh.png")

    bs_tkimage_create("::image::bs_chk_pkgs" , "icons/refresh.png")

    bs_tkimage_create("::image::bs_open_file", "icons/document-open.png")
    bs_tkimage_create("::image::bs_open_dir" , "icons/document-open-folder.png")
    bs_tkimage_create("::image::bs_columns"  , "icons/bs_cols.png")
    bs_tkimage_create("::image::bs_rows"     , "icons/bs_rows.png")

    bs_tkimage_create("::image::bs_import",    "icons/bs_import.png")
    bs_tkimage_create("::image::bs_export",    "icons/bs_export.png")
    bs_tkimage_create("::image::bs_object",    "icons/page-white.png")
    bs_tkimage_create("::image::bs_new_doc",   "icons/document-new-3.png")
    bs_tkimage_create("::image::bs_preview",   "icons/document-preview.png")
    bs_tkimage_create("::image::bs_new_window","icons/window-new.png")

    bs_tkimage_create("::image::bs_num_list",   "icons/insert-numbers.png")
    bs_tkimage_create("::image::bs_table",      "icons/kdb_table.png")
    bs_tkimage_create("::image::bs_sort_asc",   "icons/view-sort-ascending.png")
    bs_tkimage_create("::image::bs_sort_desc",  "icons/view-sort-descending.png")
    bs_tkimage_create("::image::bs_rectangle",  "icons/draw-rectangle.png")


    bs_tkimage_create("::image::bs_package",   "icons/package-green.png")
    bs_tkimage_create("::image::bs_text",      "icons/bs_t.png")
    bs_tkimage_create("::image::bs_pptx",      "icons/page-white_powerpoint.png")
    bs_tkimage_create("::image::bs_excel",     "icons/page-white_excel.png")
    bs_tkimage_create("::image::bs_word",      "icons/page-white_word.png")
    bs_tkimage_create("::image::bs_md",        "icons/md.png")

    bs_tkimage_create("::image::bs_settings",  "icons/system-2.png")

    bs_tkimage_create("::image::bs_open_wd",   "icons/bs_folder_open.png")
    bs_tkimage_create("::image::bs_path_to_wd","icons/bs_folder_show.png")
    bs_tkimage_create("::image::bs_set_wd",    "icons/bs_folder_change.png")
    bs_tkimage_create("::image::bs_folder",    "icons/bs_folder.png")
    bs_tkimage_create("::image::bs_workspace", "icons/user-desktop.png")

    bs_tkimage_create("::image::bs_analyze",   "icons/bs_analyze.png")
    bs_tkimage_create("::image::bs_summary",   "icons/bs_enumlist.png")
    bs_tkimage_create("::image::bs_plot",      "icons/office-chart-pie.png")
    bs_tkimage_create("::image::bs_chart",     "icons/insert-chart-bar.png")

    bs_tkimage_create("::image::bs_tick",      "icons/dialog-ok-apply-5.png")
    bs_tkimage_create("::image::bs_bug",       "icons/bug.png")
    bs_tkimage_create("::image::bs_home",      "icons/go-home-6.png")
    bs_tkimage_create("::image::bs_about",     "icons/help-about-3.png")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


