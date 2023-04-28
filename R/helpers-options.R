# opt <- "new_plots_window"
# opt_name <- str_glue("rcmdr_bs.{opt}")
# default  <- TRUE
get_bs_options <- function() {
  .prefix <- "^rcmdr_bs."
  opts <- options()
  opts <- opts[grepl(.prefix, names(opts))]
  names(opts) <- sub(.prefix, "", names(opts))
  opts
}

get_bs_option <- function(x) {
  # .prefix = "^rcmdr_bs."
  # get_bs_options(.prefix = .prefix)[[x]]
  get_bs_options()[[x]]
}

set_default_options <- function(..., .prefix = "") {
  opts <- list(...)
  names(opts) <- str_glue("{.prefix}{names(opts)}")
  modified_defaults <- purrr::imap(opts, ~ getOption(..2, default = ..1))
  options(modified_defaults)
}

set_default_bs_options <- function(...) {
  set_default_options(..., .prefix = "rcmdr_bs.")
}

set_default_option <- function(x, default = NULL, .prefix = "") {
  opt_name <- str_glue("{.prefix}{x}")
  current  <- getOption(opt_name, default = default)
  str_glue_eval("options({opt_name} = current)")
}

# set_options <- function(..., .prefix = "") {
#   opts <- list(...)
#   names(opts) <- str_glue("{.prefix}{names(opts)}")
#   purrr::imap(opts, ~options(..2 = ..1))            # Issue here: ..2 = ..1
# }
#
# set_bs_options <- function(..., .prefix = "") {
#   set_options(..., .prefix = "rcmdr_bs.")
# }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
