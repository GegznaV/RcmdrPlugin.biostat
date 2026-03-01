#' Open the esquisse interactive ggplot builder
#'
#' Launches esquisse with the active dataset in the RStudio viewer.
#'
#' @keywords internal
open_esquisse_app <- function() {
  suppressMessages(suppressWarnings(
    run_in_rstudio("esquisse::esquisser(data = {active_dataset()})")
  ))
}
