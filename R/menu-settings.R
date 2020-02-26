# Workspace and Settings menu functions ======================================

# Working directory ----------------------------------------------------------
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_getwd <- function() {
  Rcmdr::doItAndPrint(paste(
    '## Current working directory:',
    'getwd()',
    sep = " \n"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_list_files_wd <- function() {
  Rcmdr::doItAndPrint(paste(
    '## Files and folders in working directory:',
    'dir(all.files = TRUE)',
    sep = " \n"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_setwd <- function() {
  new_wd <- tclvalue(tkchooseDirectory(
    initialdir = getwd(),
    parent = CommanderWindow()))

  if (new_wd != "") {
    Rcmdr::doItAndPrint(str_glue('setwd("{new_wd}")'))
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_openwd <- function() {
  fs::file_show(getwd())
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_get_file_info <- function() {

  f_path <- fs::path_tidy(tkgetOpenFile(
    title = "Choose File",
    parent = CommanderWindow(),
    initialdir = getwd()
  ))

  command <- str_glue(
    '## Information about file "{basename(f_path)}" \n',
    't(fs::file_info("{f_path}"))'
  )

  Rcmdr::doItAndPrint(command)
}


# Workspace ------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TODO:
#  Convert to window
#
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_list_objects <- function() {

  # Inputs (not implemented) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  show_hidden <- TRUE

  if (show_hidden) {
    all_names <- "all.names = TRUE"
    all_txt   <- " all "
  } else {
    all_names <- ""
    all_txt   <- " "

  }

  # Code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  command <-  str_glue(
    "## List{all_txt}objects in R workspace \n",
    "objects({all_names})"
  )

  doItAndPrint(command)

}

# Session information --------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_session_info_devtools <- function() {
  Rcmdr::doItAndPrint(paste("# R session information \n",
    "devtools::session_info()"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_session_info_utils <- function() {
  Rcmdr::doItAndPrint(paste("# R session information \n",
    "sessionInfo()"))
}
