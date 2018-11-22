#' @name RcmdrPlugin.biostat
#' @docType package
#'
#' @title An R Commander Plug-in for Basic Data Analysis Tasks
#'
#' @description
#' \code{RcmdrPlugin.biostat} is an \code{Rcmdr} plugin for functions in package
#' \code{biostat} and other common (bio)statistical routines.
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
#' @import biostat
#' @import nortest
#' @import RcmdrPlugin.EZR
# @import Rcmdr


NULL

# .onLoad <- function(...){
#     library(Rcmdr)
#     load_packages_command()
# }


