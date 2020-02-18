load_rcmdr_biostat <- function() {

  all_pkgs <- .packages(all.available = TRUE)

  pkg1 <-
    if ("RcmdrPlugin.KMggplot2" %in% all_pkgs) {
      "library(RcmdrPlugin.KMggplot2)\n"
    } else {
      ""
    }

  # pkg2 <-
  #   if ("RcmdrPlugin.EZR.as.menu" %in% all_pkgs) {
  #     "library(RcmdrPlugin.EZR.as.menu)\n"
  #   } else {
  #     ""
  #   }

  command <-
    paste0(
      "library(Rcmdr) \n",
      pkg1,
      # pkg2,
      "library(RcmdrPlugin.biostat) \n",
      "load_rcmdr_biostat_mode()"
    )

  rstudioapi::sendToConsole(command)

}

