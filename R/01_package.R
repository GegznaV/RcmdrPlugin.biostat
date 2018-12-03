#' @name RcmdrPlugin.biostat
#' @docType package
#'
#' @title An R Commander Plug-in for Basic Data Analysis Tasks
#'
#' @description
#' \pkg{RcmdrPlugin.biostat} is an
#' \href{\pkg{Rcmdr}}{https://cran.r-project.org/web/packages/Rcmdr/index.html}
#' plug-in for the most common data wrangling, visualisation and analysis tasks
#' using "tidyverse" family functions as well as functions from other packages.
#'
#' @importFrom glue glue
#' @importFrom dplyr glimpse is_grouped_df
#' @importFrom magrittr "%>%"  "%<>%"  "%T>%"  "%$%"
#' @importFrom Rcmdr activeDataSet
#' @importFrom graphics plot
#' @importFrom stats p.adjust.methods C
#' @importFrom utils browseURL packageVersion data
#' @import tcltk
#' @import tcltk2
#' @import stringr
#' @import purrr
#' @import nortest
#' @import RcmdrPlugin.EZR
# @import Rcmdr
# @import biostat


NULL

# .onLoad <- function(...){
#     library(Rcmdr)
#     load_packages_command()
# }


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

    add_plugins <-
        unname(grep("RcmdrPlugin.(KMggplot2|EZR.2|biostat)",
                    installed.packages()[ , 1],
                    value = TRUE))

    # Add plugins in certain order
    plugins <- c(setdiff(Rcmdr_opts$plugins, add_plugins),
                 rev(sort(add_plugins)))

    # plugins <- unique(c(Rcmdr_opts$plugins, pkgname))

    # Open 3-window Rcmdr, if options is not defined
    if (is.null(Rcmdr_opts$console.output)) {
        console.output <- FALSE

    } else {
        console.output <- Rcmdr_opts$console.output
    }

    updated_opts <-
        modifyList(Rcmdr_opts,
                   list(plugins = plugins,
                        console.output = console.output))

    if (!identical(Rcmdr_opts, updated_opts)) {
        # Set new options and restart R Commander
        options(Rcmdr = updated_opts)

        if ("package:Rcmdr" %in% search()) {
            if (!getRcmdr("autoRestart")) {
                closeCommander(ask = FALSE, ask.save = TRUE)
                Commander()
            }

        } else {
            Commander()
        }
    }


    # Create icons
    tkimage.create("photo", "::image::bs_dataset",
                   file = system.file("etc", "list.png",
                                      package = "RcmdrPlugin.biostat"))


    # Enamble enhanced Dataset button
    res <- try(Rcmdr::getRcmdr("dataSetLabel"), silent = TRUE)

    if (!inherits(res, "try-error")) {
        tkconfigure(res,
                    # foreground = "darkred",
                    image = "::image::bs_dataset",
                    compound = "left",
                    command = window_dataset_select)
    }

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


