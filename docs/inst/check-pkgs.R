# No code yet


msg_rcmdr <- c('
# 1) copy this code;
# 2) Restart R session: RStudio menu -> Session -> Restart R;
# 3) Paste the code to Console;
# 4) Press "Enter" to run the code (to install the packages).')


msg_rs <- c('
# 1) The code is copied into RStudio console;
# 2) Restart R session: RStudio menu -> Session -> Restart R;
# 3) Put cursor into the Console;
# 4) Press "Enter" to run the code (to install the packages)')



code <- c('
\n
# Expected versions of packages:
#   RcmdrPlugin.biostat    0.0.43 or newer.
#   RcmdrPlugin.KMggplot2  0.2-5  (NOT 0.2-6).
#   Rcmdr                  2.5-3  (NOT 2.6-0).

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
remotes::install_github("GegznaV/RcmdrPlugin.biostat")  # reikiama versija 0.0.43
remotes::install_version("RcmdrPlugin.KMggplot2", version = "0.2-5")
remotes::install_version("Rcmdr", version = "2.5-3")

# Part D
options(old_opts)
')

if (identical(.Platform$GUI, "RStudio") && rstudioapi::isAvailable("1.2.1335")) {
  rstudioapi::executeCommand("activateConsole")
  rstudioapi::executeCommand("consoleClear")
  cat(sep = "\n", msg_rs)
  rstudioapi::sendToConsole(code, execute = FALSE)

} else {
  cat(sep = "\n", msg_rcmdr)
}


