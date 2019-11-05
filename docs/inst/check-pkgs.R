# No code yet


msg_rcmdr <- c('
# 1) Copy this code;
# 2) Restart R session: RStudio menu -> Session -> Restart R;
# 3) Paste the code to Console;
# 4) Press "Enter" to run the code (to install the packages).')


msg_rs <- c('
# 1) The code is copied into RStudio console;
# 2) Restart R session: RStudio menu -> Session -> Restart R;
# 3) Put cursor into the Console;
# 4) Press "Enter" to run the code (to install the packages)')



code <- c('
# Part A
old_opts <-
  options(
    repos = "https://cran.rstudio.com/",
    pkgType = "both",
    install.packages.check.source = "yes",
    install.packages.compile.from.source = "always"
  )

# Part B
install.packages("rvg")
install.packages("officer")
install.packages("Rcmdr")
install.packages("RcmdrPlugin.KMggplot2")

# Part C
if (!require("remotes"))  install.packages("remotes")
remotes::install_github("GegznaV/RcmdrPlugin.biostat")
remotes::install_github("rforge/rcmdr/pkg/Rcmdr-devel", upgrade = TRUE)

# Part D
options(old_opts)


# Expected versions of packages after the instalation:
#   Rcmdr                  2.6-1  or newer (NOT 2.6-0).
#   RcmdrPlugin.biostat    0.0.47 or newer.
#   RcmdrPlugin.KMggplot2  0.2-6  or newer.
')

if (identical(.Platform$GUI, "RStudio") && rstudioapi::isAvailable("1.2.1335")) {
  rstudioapi::executeCommand("activateConsole")
  rstudioapi::executeCommand("consoleClear")
  cat(sep = "\n", msg_rs)
  rstudioapi::sendToConsole(code, execute = FALSE)

} else {
  cat(sep = "\n", msg_rcmdr)
}


