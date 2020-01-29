# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_summary <- function() {

  .ds <- active_dataset_0()
  command <-
    str_glue(
      "## The summary of variables in '{.ds}'\n",
      "summary({.ds})"
    ) %>%
    style_cmd()

  doItAndPrint(command)

}
