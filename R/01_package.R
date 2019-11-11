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
#' @importFrom graphics plot par
#' @importFrom stats p.adjust.methods C setNames
#' @importFrom utils browseURL packageVersion data installed.packages modifyList
#'                   capture.output tail vignette help globalVariables
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

    # Use (un)sorted vector of variable names
    if (is.null(Rcmdr_opts$sort.names)) {
        sort.names <- FALSE

    } else {
        sort.names <- Rcmdr_opts$sort.names
    }

    updated_opts <-
        utils::modifyList(
            Rcmdr_opts,
            list(
              plugins = plugins,
              console.output = console.output,
              sort.names = sort.names
            )
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

    bs_tkimage_create("::image::dot-black",     "icons/oth/dot-black.png")
    bs_tkimage_create("::image::dot-red",       "icons/oth/dot-red.png")
    bs_tkimage_create("::image::dot-blue",      "icons/oth/dot-blue.png")
    bs_tkimage_create("::image::dot-lblue",     "icons/oth/dot-lblue.png")
    bs_tkimage_create("::image::dot-green",     "icons/oth/dot-green.png")
    bs_tkimage_create("::image::dot-lgreen",    "icons/oth/dot-lgreen.png")
    bs_tkimage_create("::image::dot-yellow",    "icons/oth/dot-yellow.png")
    bs_tkimage_create("::image::dot-grey",      "icons/oth/dot-grey.png")
    bs_tkimage_create("::image::dot-violet",    "icons/oth/dot-violet.png")

    bs_tkimage_create("::image::dot-gw-4",      "icons/oth/dot-gw-4x4.png")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bs_tkimage_create("::image::bs_r_logo_shapes",     "icons/32/shapes.png")
    bs_tkimage_create("::image::bs_r_logo_management", "icons/32/documentation.png")
    bs_tkimage_create("::image::bs_r_logo_analysis",   "icons/32/bs-analysis-32.png")
    bs_tkimage_create("::image::bs_r_logo_plots",      "icons/32/office-chart-pie.png")
    bs_tkimage_create("::image::bs_r_logo_settings",   "icons/32/system-settings.png")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bs_tkimage_create("::image::bs_r_logo_b"  , "icons/32/R-logo-b.png")
    bs_tkimage_create("::image::bs_r_logo_br" , "icons/32/R-logo-br.png")
    bs_tkimage_create("::image::bs_r_logo_bw" , "icons/32/R-logo-bw.png")
    bs_tkimage_create("::image::bs_r_logo_lb" , "icons/32/R-logo-lb.png")
    bs_tkimage_create("::image::bs_r_logo_c"  , "icons/32/R-logo-c.png")
    bs_tkimage_create("::image::bs_r_logo_g"  , "icons/32/R-logo-g.png")
    bs_tkimage_create("::image::bs_r_logo_lg" , "icons/32/R-logo-lg.png")
    bs_tkimage_create("::image::bs_r_logo_r"  , "icons/32/R-logo-r.png")
    bs_tkimage_create("::image::bs_r_logo_v"  , "icons/32/R-logo-v.png")
    bs_tkimage_create("::image::bs_r_logo_y"  , "icons/32/R-logo-y.png")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bs_tkimage_create("::image::bs_r"         , "icons/16/r-logo-16x16.png")
    bs_tkimage_create("::image::bs_r_blue"    , "icons/16/r-logo-b-16x16.png")
    bs_tkimage_create("::image::bs_r_brown"   , "icons/16/r-logo-br-16x16.png")
    bs_tkimage_create("::image::bs_r_bw"      , "icons/16/r-logo-bw-16x16.png")
    bs_tkimage_create("::image::bs_r_lblue"   , "icons/16/r-logo-lb-16x16.png")
    bs_tkimage_create("::image::bs_r_cyan"    , "icons/16/r-logo-c-16x16.png")
    bs_tkimage_create("::image::bs_r_green"   , "icons/16/r-logo-g-16x16.png")
    bs_tkimage_create("::image::bs_r_lgreen"  , "icons/16/r-logo-lg-16x16.png")
    bs_tkimage_create("::image::bs_r_red"     , "icons/16/r-logo-r-16x16.png")
    bs_tkimage_create("::image::bs_r_violet"  , "icons/16/r-logo-v-16x16.png")
    bs_tkimage_create("::image::bs_r_yellow"  , "icons/16/r-logo-y-16x16.png")

    bs_tkimage_create("::image::bs_dataset"   , "icons/16/list.png")
    bs_tkimage_create("::image::bs_model"     , "icons/16/model.png")

    bs_tkimage_create("::image::bs_ok"        , "icons/16/dialog-ok-apply-5.png")
    bs_tkimage_create("::image::bs_tick"      , "icons/16/dialog-ok-apply-5.png")
    bs_tkimage_create("::image::bs_delete"    , "icons/16/delete.png")
    bs_tkimage_create("::image::bs_paste"     , "icons/16/edit-paste-7.png")
    bs_tkimage_create("::image::bs_copy"      , "icons/16/edit-copy-7.png")
    bs_tkimage_create("::image::bs_rename"    , "icons/16/textfield-rename.png")
    bs_tkimage_create("::image::bs_cut"       , "icons/16/edit-cut-7.png")
    bs_tkimage_create("::image::bs_undo"      , "icons/16/edit-undo-7.png")
    bs_tkimage_create("::image::bs_redo"      , "icons/16/edit-redo-7.png")
    bs_tkimage_create("::image::bs_find"      , "icons/16/edit-find-7.png")
    bs_tkimage_create("::image::bs_refresh"   , "icons/16/refresh.png")
    bs_tkimage_create("::image::bs_select_all", "icons/16/edit-select-all.png")
    bs_tkimage_create("::image::bs_down"      , "icons/16/down-blue.png")
    bs_tkimage_create("::image::bs_chk_pkgs" ,  "icons/16/view-refresh-6.png")

    bs_tkimage_create("::image::bs_open_file",  "icons/16/document-open.png")
    bs_tkimage_create("::image::bs_open_dir" ,  "icons/16/document-open-folder.png")
    bs_tkimage_create("::image::bs_table",      "icons/16/kdb_table.png")
    bs_tkimage_create("::image::bs_rectangle",  "icons/16/draw-rectangle.png")

    bs_tkimage_create("::image::bs_go_top",     "icons/16/go-top-3.png")
    bs_tkimage_create("::image::bs_go_up",      "icons/16/go-up-4.png")
    bs_tkimage_create("::image::bs_go_down",    "icons/16/go-down-4.png")
    bs_tkimage_create("::image::bs_go_bottom",  "icons/16/go-bottom-3.png")

    bs_tkimage_create("::image::bs_go_first",   "icons/16/go-first-2.png")
    bs_tkimage_create("::image::bs_go_prev",    "icons/16/go-previous-4.png")
    bs_tkimage_create("::image::bs_go_next",    "icons/16/go-next-4.png")
    bs_tkimage_create("::image::bs_go_last",    "icons/16/go-last-2.png")

    bs_tkimage_create("::image::bs_sort_asc",   "icons/16/sort_ascending.png")
    bs_tkimage_create("::image::bs_sort_desc",  "icons/16/sort_descending.png")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bs_tkimage_create("::image::bs_import",     "icons/16/bs_import.png")
    bs_tkimage_create("::image::bs_text",       "icons/16/bs_txt.png")
    bs_tkimage_create("::image::bs_excel",      "icons/16/page-white_excel.png")
    bs_tkimage_create("::image::bs_package",    "icons/16/package-green.png")
    bs_tkimage_create("::image::bs_wpd",        "icons/16/wpd-16x16.png")
    bs_tkimage_create("::image::bs_new_doc",    "icons/16/document-new-3.png")

    bs_tkimage_create("::image::bs_word",       "icons/16/page-white_word.png")
    bs_tkimage_create("::image::bs_pptx",       "icons/16/page-white_powerpoint.png")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bs_tkimage_create("::image::bs_export",     "icons/16/bs_export.png")
    bs_tkimage_create("::image::bs_md",         "icons/16/md.png")
    bs_tkimage_create("::image::bs_print_as_df","icons/16/bs-print-as-df.png")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bs_tkimage_create("::image::bs_objects",    "icons/16/emblem-documents.png")
    bs_tkimage_create("::image::bs_join"     ,  "icons/16/application-side-list.png")
    bs_tkimage_create("::image::bs_bind_rows",  "icons/16/application-tile-vertical.png")
    bs_tkimage_create("::image::bs_bind_cols",  "icons/16/application-tile-horizontal.png")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bs_tkimage_create("::image::bs_rows"     ,  "icons/16/bs_rows.png")
    bs_tkimage_create("::image::bs_rows_sort",  "icons/16/bs_rows_sort.png")
    bs_tkimage_create("::image::bs_rows_names", "icons/16/bs_rows_names.png")
    bs_tkimage_create("::image::bs_rows_select","icons/16/bs_rows_select.png")
    bs_tkimage_create("::image::bs_rows_filter","icons/16/bs_rows_filter.png")
    bs_tkimage_create("::image::bs_rows_slice", "icons/16/bs_rows_slice.png")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bs_tkimage_create("::image::bs_columns"  ,    "icons/16/bs_cols.png")
    bs_tkimage_create("::image::bs_cols_names",   "icons/16/bs_cols_names.png")
    bs_tkimage_create("::image::bs_cols_select",  "icons/16/bs_cols_select.png")
    bs_tkimage_create("::image::bs_cols_compute", "icons/16/bs_cols_compute.png")
    bs_tkimage_create("::image::bs_cols_mutate",  "icons/16/bs_cols_mutate.png")
    bs_tkimage_create("::image::bs_cols_recode",  "icons/16/bs_cols_recode.png")
    bs_tkimage_create("::image::bs_cols_convert", "icons/16/bs_cols_convert.png")
    bs_tkimage_create("::image::bs_data_reshape", "icons/16/bs_data_reshape.png")
    bs_tkimage_create("::image::bs_data_chr",     "icons/16/bs_data_chr.png")
    bs_tkimage_create("::image::bs_data_fct",     "icons/16/bs_data_fct.png")
    bs_tkimage_create("::image::bs_data_num",     "icons/16/bs_data_num.png")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bs_tkimage_create("::image::bs_preview",    "icons/16/document-preview.png")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bs_tkimage_create("::image::bs_summary",    "icons/16/bs_enumlist.png")
    bs_tkimage_create("::image::bs_glimpse",    "icons/16/glimpse_16.png")
    bs_tkimage_create("::image::bs_desc",       "icons/16/bs_desc.png")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bs_tkimage_create("::image::bs_analyze",    "icons/16/bs_analyze.png")
    bs_tkimage_create("::image::bs_normality",  "icons/16/bs_normality.png")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bs_tkimage_create("::image::bs_plot",       "icons/16/office-chart-pie.png")
    bs_tkimage_create("::image::bs_new_window", "icons/16/window-new.png")
    bs_tkimage_create("::image::bs_plot_close", "icons/16/delete.png")
    bs_tkimage_create("::image::bs_plotly",     "icons/16/plotly.png")
    bs_tkimage_create("::image::bs_chart",      "icons/16/insert-chart-bar.png")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bs_tkimage_create("::image::bs_settings",   "icons/16/system-2.png")
    bs_tkimage_create("::image::bs_locale"    , "icons/16/locale.png")

    bs_tkimage_create("::image::bs_restart",    "icons/16/system-reboot-2.png")
    bs_tkimage_create("::image::bs_restart_r",  "icons/16/system-log-out-2.png")
    bs_tkimage_create("::image::bs_close_rcmdr","icons/16/system-log-out-4v.png")
    bs_tkimage_create("::image::bs_close_r",    "icons/16/system-shutdown-6.png")

    bs_tkimage_create("::image::bs_workspace",  "icons/16/user-desktop.png")
    bs_tkimage_create("::image::bs_open_wd",    "icons/16/bs_folder_open.png")
    bs_tkimage_create("::image::bs_path_to_wd", "icons/16/bs_folder_show.png")
    bs_tkimage_create("::image::bs_set_wd",     "icons/16/bs_folder_change.png")
    bs_tkimage_create("::image::bs_folder",     "icons/16/bs_folder.png")

    bs_tkimage_create("::image::bs_about",      "icons/16/help-about-3.png")
    bs_tkimage_create("::image::bs_bug",        "icons/16/bug.png")
    bs_tkimage_create("::image::bs_home",       "icons/16/go-home-6.png")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


