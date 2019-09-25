# No code yet
cat("")


cat(sep = "\n", '
\n# [Copy this code and paste into Console]

# A dalis
old_opts <-
  options(
    repos = "https://cran.rstudio.com/",
    pkgType = "both",
    install.packages.check.source = "yes",
    install.packages.compile.from.source = "always"
  )

# B dalis
install.packages("rvg")
install.packages("officer")
install.packages("Rcmdr")
install.packages("RcmdrPlugin.KMggplot2")

# C dalis
if (!require("remotes"))  install.packages("remotes")
remotes::install_github("GegznaV/RcmdrPlugin.biostat")  # reikiama versija 0.0.43
remotes::install_version("RcmdrPlugin.KMggplot2", version = "0.2-5")
remotes::install_version("Rcmdr", version = "2.5-3")

# D dalis
options(old_opts)
')
