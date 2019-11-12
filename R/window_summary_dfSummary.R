# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_dfSummary <- function() {
  Library("summarytools")

  # FIXME: remove when summarytools fixes this bug
  try({
    op <- summarytools::st_options("use.x11")
    summarytools::st_options("use.x11" = FALSE)
  }, silent = TRUE)

  .ds <- active_dataset_0()
  command <-
    str_glue(
      "## The summary of dataset '{.ds}'\n",
      "dfSummary({.ds})"
    ) %>%
    style_cmd()

  doItAndPrint(command)

  # FIXME: remove when summarytools fixes this bug
  try({summarytools::st_options("use.x11" = op)}, silent = TRUE)
}
