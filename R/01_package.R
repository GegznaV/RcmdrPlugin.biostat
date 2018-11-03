#' @name RcmdrPlugin.biostat
#' @docType package
#'
#' @title An R Commander Plug-in for Basic Data Analysis Tasks
#'
#'
#' @description
#'
#' \code{RcmdrPlugin.biostat} is an \code{Rcmdr} plugin for functions in package
#' \code{biostat} and other common (bio)statistical routines.
#'
#' @importFrom glue glue
#' @importFrom dplyr glimpse
#' @importFrom magrittr "%>%"  "%<>%"  "%T>%"  "%$%"
#' @import tcltk
#' @import tcltk2
#' @import biostat
#' @import nortest
#' @import RcmdrPlugin.EZR
#' @importFrom Rcmdr activeDataSet
#' @importFrom graphics plot
#' @importFrom stats p.adjust.methods C
#' @importFrom utils browseURL packageVersion
#'

str_c    <- stringr::str_c
str_glue <- stringr::str_glue

NULL

# .onLoad <- function(...){
#     library(Rcmdr)
#     load_packages_command()
# }


