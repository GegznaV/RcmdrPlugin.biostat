open_esquisse_app <- function() {
  suppressMessages({suppressWarnings({
    run_in_rstudio("esquisse::esquisser(data = {active_dataset()})")
  })})
}

