load_rcmdr_biostat <- function() {
  rstudioapi::sendToConsole(
    paste(sep = "\n",
      "library(Rcmdr)",
      "library(RcmdrPlugin.biostat)",
      "load_rcmdr_biostat_mode()"
    )
  )
}

