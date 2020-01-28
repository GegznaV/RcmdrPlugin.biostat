open_esquisse_app <- function() {
  suppressMessages({suppressWarnings({
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    str_glue_eval("esquisse::esquisser(data = {active_dataset()})",
      envir_eval = .GlobalEnv)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  })})
}
