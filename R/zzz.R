#' @importFrom grDevices windowsFonts
.onAttach <- function(libname, pkgname) {

  if (!interactive()) return()
  Rcmdr <- options()$Rcmdr
  options(
    Bio__FontSize   = "14",
    Bio__FontFamily = 0,
    Bio__ColourSet  = 0,
    Bio__SaveGraph  = 0,
    Bio__Theme      = 0
  )
  
  if (.Platform$OS.type == "windows") {
    windowsFonts(
      Japan1 = "Japan1", Japan1HeiMin = "Japan1HeiMin",
      Japan1GothicBBB = "Japan1GothicBBB", Japan1Ryumin = "Japan1Ryumin",
      Korea1 = "Korea1", Korea1deb = "Korea1deb", CNS1 = "CNS1", GB1 = "GB1"
      )
  }
    
  plugins <- Rcmdr$plugins
  if (!pkgname %in% plugins) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr = Rcmdr)
    if("package:Rcmdr" %in% search()) {
      if(!getRcmdr("autoRestart")) {
        closeCommander(ask = FALSE, ask.save = TRUE)
        Commander()
      }
    }
    else {
      Commander()
    }
  }
  
}
