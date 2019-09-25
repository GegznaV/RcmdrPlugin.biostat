# No code yet
cat("")


cat(sep = "\n", '
\n# [Copy this code and paste into Console]

old_opts <-
  options(
    repos = "https://cran.rstudio.com/",
    pkgType = "both",
    install.packages.check.source = "yes",
    install.packages.compile.from.source = "always"
  )

install.packages("rvg")
install.packages("officer")

if (!require("remotes"))  install.packages("remotes")
remotes::install_github("GegznaV/RcmdrPlugin.biostat")
remotes::install_version("RcmdrPlugin.KMggplot2", version = "0.2-5")
remotes::install_version("Rcmdr", version = "2.5-3")

options(old_opts)
')
