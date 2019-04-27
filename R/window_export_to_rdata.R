#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_rdata  <- function() {
    window_export_to_rdata_0(ds_name = active_dataset())
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_rdata_0 <- function(ds_name = active_dataset()) {
    file_name  <- ds_name
    initialdir <- getwd()

    .ds <- safe_names(ds_name)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    file_name <- get_filename_to_save(
        title = "Choose or Create R-data File to Save Data to",
        file_name = file_name,
        filetypes = "{ {RData file} {.RData} } { {All Files} * }",
        initialdir = initialdir,
        defaultextension = "RData"
    )

    # If canceled
    if (nchar(file_name) == 0) {
        # Message("Canceled. Object was not saved.", type = "warning")
        return(NULL)
    }

    # Change these lines: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (get_use_relative_path()) {
        file_name <- make_relative_path(file_name)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    command <- str_glue(
        "## Save data to R-data file\n",
        'save({.ds}, file = "{file_name}")') %>%
        style_cmd()

    doItAndPrint(command)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
