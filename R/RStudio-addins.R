#' RStudio addin: Load R Commander in BioStat mode
#'
#' Sends library loading commands to the RStudio console.
#'
#' @keywords internal
load_rcmdr_biostat <- function() {

  command <-
    paste0(
      "library(Rcmdr) \n",
      "library(RcmdrPlugin.biostat) \n",
      "load_rcmdr_biostat_mode()"
    )

  rstudioapi::sendToConsole(command)

}
