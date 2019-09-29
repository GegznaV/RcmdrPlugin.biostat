# Create a window:
# radiobuttons: "all", "fivenum", "common", "custom"

# If "custom":
# "mean", "sd", "min", "q1", "med", "q3", "max", "mad", "iqr", "cv",
# "skewness", "se.skewness", "kurtosis", "n.valid",  "pct.valid"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_descr <- function() {
  # SummaryTools
  .ds <- active_dataset_0()

  # If any numeric variables exist
  ds_numeric <-
    purrr::map_lgl(
      str_glue_eval("{.ds}", envir_eval = .GlobalEnv),
      ~is.numeric(.)
    )

  if (any(ds_numeric)) {
    Library("tidyverse")
    Library("summarytools")
    command <- str_glue(
      .trim = FALSE,
      "## The summary of numeric variables\n",
      "{.ds} %>% ",
      # "  group_by() %>%",
      "  select_if(is.numeric) %>%",
      "  descr(round.digits = 2)"
    )
    doItAndPrint(style_cmd(command))

  } else {
    doItAndPrint("## No numeric variables found")
  }
}
