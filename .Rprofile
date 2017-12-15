
# install.packages("RcmdrPlugin.KMggplot2")
#
# if (!"devtools" %in% installed.packages())  install.packages("devtools")
#
# devtools::install_github("GegznaV/RcmdrPlugin.EZR@unmodified_Rcmdr_menu")
# devtools::install_github("GegznaV/BioStat")
# devtools::install_github("GegznaV/RcmdrPlugin.BioStat")

packages <- c("devtools",
              "rlang",
              "tidyverse",
              "tidyselect",
              "checkmate",
              "RcmdrPlugin.KMggplot2",
              "RcmdrPlugin.EZR",
              "BioStat",
              "RcmdrPlugin.BioStat"
              )

missing_packages <- packages[!packages %in% utils::installed.packages()]

# Jei bent vieno paketo nėra, tada įdiegiam, ko trūksta
if (length(missing_packages) > 0) {
    utils::install.packages(missing_packages,
                            dependencies = c("Depends", "Imports", "Suggests"))
}




    # devtools::install_github("GegznaV/RcmdrPlugin.EZR@unmodified_Rcmdr_menu",
    #                          dependencies = c("Depends", "Imports", "Suggests"))
    #
    # devtools::install_github("GegznaV/BioStat",
    #                          dependencies = c("Depends", "Imports", "Suggests"))
    #
    # devtools::install_github("GegznaV/RcmdrPlugin.BioStat",
    #                          dependencies = c("Depends", "Imports", "Suggests"))



if (!"RcmdrPlugin.BioStat" %in% utils::installed.packages()) {
    utils::install.packages("BioStat")
    utils::install.packages("RcmdrPlugin.BioStat")
}


rmd_template_filenamename <- paste0(
    dir(.libPaths(), pattern = "RcmdrPlugin.BioStat", full.names = TRUE),
    "/etc/BioStat-RMarkdown-Template.Rmd"
)

###! Rcmdr Options Begin !###
options(Rcmdr = list(plugins = c("RcmdrPlugin.KMggplot2",
                                 "RcmdrPlugin.EZR.2",
                                 "RcmdrPlugin.BioStat",
                                 NULL),
                     console.output = FALSE,
                     use.rgl = FALSE,
                     rmd.template = rmd_template_filenamename)
        )

Sys.setlocale(locale = "Lithuanian")

# library(BioStat)

# library(magrittr)
# library(pander)
# library(ggplot2)
# library(spMisc)



# Uncomment the following 4 lines (remove the #s)
#  to start the R Commander automatically when R starts:

# local({
#    old <- getOption('defaultPackages')
#    options(defaultPackages = c(old, 'Rcmdr'))
# })

###! Rcmdr Options End !###


message("--- Papildinys uzkrautas: RcmdrPlugin.BioStat ---")
