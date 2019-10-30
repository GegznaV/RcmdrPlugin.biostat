# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_dfSummary <- function() {
  Library("summarytools")

  if (.Platform$OS.type == "windows") {
    # FIXME: remove when summarytools fixes this bug
    summarytools::st_options("use.x11" = FALSE)
  }
  .ds <- active_dataset_0()
  command <-
    str_glue(
      "## The summary of dataset '{.ds}'\n",
      "dfSummary({.ds})"
    ) %>%
    style_cmd()

  doItAndPrint(command)
}
