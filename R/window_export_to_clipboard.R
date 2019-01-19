#' @rdname Menu-window-functions
#' @export
#' @keywords internal
export_to_clipboard <- function(ds_name = active_dataset_0(), sep = ",") {
    try(
        clipr::write_clip(get(ds_name, envir = .GlobalEnv), sep = sep),
        silent = TRUE
    )
}


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
export_to_clipboard_active_ds_tab <- function() {
    try(
        clipr::write_clip(get(active_dataset_0(), envir = .GlobalEnv), sep = "\t"),
        silent = TRUE)
}