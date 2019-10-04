# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_dfSummary <- function() {
  Library("summarytools")

  .ds <- active_dataset_0()
  command <-
    str_glue(
      "## The summary of dataset '{.ds}'\n",
      "dfSummary({.ds})"
    ) %>%
    style_cmd()

  doItAndPrint(command)
}
