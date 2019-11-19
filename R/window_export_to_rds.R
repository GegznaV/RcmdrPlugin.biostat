
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_rds  <- function() {
  window_export_to_rds_0(ds_name = active_dataset())
}


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_rds_0 <- function(ds_name = active_dataset()) {
  file_name <- ds_name
  .ds <- safe_names(ds_name)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  file_name <- get_filename_to_save(
    title = "Choose or Create Rds File to Save Data to",
    file_name = file_name,
    filetypes = "{ {Rds file} {.rds} } { {All Files} * }",
    defaultextension = "rds"
  )

  # If canceled
  if (nchar(file_name) == 0) {
    # Message("Canceled. Object was not saved.", type = "warning")
    return()
  }

  # Change these lines: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (get_use_relative_path()) {
    # file_name <- fs::path_rel(file_name)
    file_name <- make_relative_path(file_name)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  command <- str_glue("## Save data to 'Rds' file\n",
    'saveRDS({.ds}, file = "{file_name}")')
  doItAndPrint(command)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
