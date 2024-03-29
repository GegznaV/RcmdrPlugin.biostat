# "Summany" menu related functions ============================================

#  Graphical overview --------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_inspect <- function() {
  Library("tidyverse")
  Library("inspectdf")

  # command <- str_glue("{active_dataset_0()} %>% inspect_na() %>% show_plot()")

  # if (is_plot_in_separate_window()) {
  #     open_new_plots_window()
  # }

  # command <- str_glue("{active_dataset_0()} %>% inspect_mem() %>% show_plot()")
  # command <- str_glue("{active_dataset_0()} %>% inspect_types() %>% show_plot()")
  command <- str_glue("{active_dataset_0()} %>% inspect_num() %>% show_plot()")
  command <- str_glue("{active_dataset_0()} %>% inspect_cat() %>% show_plot()")

  doItAndPrint(command)
}

#  Overview ------------------------------------------------------------------
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_dim <- function() {
  command_dataset_dim_0(.ds = active_dataset_0())
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_dim_0 <- function(.ds) {
  doItAndPrint(str_glue(
    "## Number of rows and columns\n",
    "dim({.ds})"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_glimpse <- function() {
  Library("tidyverse")

  command <-
    str_glue(
      "## Structure of dataset \n",
      "dplyr::glimpse({active_dataset_0()})"
    ) %>%
    style_cmd()

  doItAndPrint(command)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_desc_all <- function() {
  Library("DescTools")

  opts_code <- get_desctools_opts_str()
  command <-
    str_glue(
      "{opts_code} ",
      "## Summarize all variables \n",
      'DescTools::Desc({active_dataset_0()}, plotit = FALSE, ord = "level")'
    ) %>%
    style_cmd()

  doItAndPrint(command)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_head_tail <- function() {
  .ds <- active_dataset_0()
  ds <- eval_text(.ds, envir = .GlobalEnv)

  Library("tidyverse")
  Library("data.table")

  keep_rownames_txt <-
    if (tibble::has_rownames(ds)) {
      rn <- unique_colnames("row_names")
      str_glue('keep.rownames = "{rn}"')

    } else {
      ""
    }

  str_glue(
    "## Top and bottom rows\n",
    "{.ds} %>% \n",
    "as.data.table({keep_rownames_txt}) %>% \n",
    "print(topn = 5, nrows = 10)"
  ) %>%
    style_cmd() %>%
    doItAndPrint()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_var_types <- function() {
  summary_var_types_0(.ds = active_dataset_0())
}
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_var_types_plot <- function() {
  summary_var_types_plot_0(.ds = active_dataset_0())
}

summary_var_types_0 <- function(.ds) {

  Library("tidyverse")

  doItAndPrint(
    str_glue(
      "## Variable type summary \n",
      '## (dataset "{.ds}")\n',
      "{.ds} %>% \n",
      '  purrr::map_chr(~ class(.) %>% paste(collapse = ", ")) %>% \n',
      '  tibble::enframe("variable", "class") %>% \n',
      '  dplyr::count(class, name = "n variables") %>% \n',
      '  knitr::kable(format = "pandoc")'
    )
  )
}

summary_var_types_plot_0 <- function(.ds) {
  Library("tidyverse")
  Library("inspectdf")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Each plot in a separate window
  if (is_plot_in_separate_window()) {
    open_new_plots_window()
  }
  doItAndPrint(str_glue(
    "## Variable sizes in memory \n",
    "{.ds} %>% inspect_mem() %>% show_plot()"
  ))

  if (is_plot_in_separate_window()) {
    open_new_plots_window()
  }
  doItAndPrint(str_glue(
    "## Plot of variable type frequency \n",
    "{.ds} %>% inspect_types() %>% show_plot()"
  ))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cmd_var_summary_skim <- function() {
  .ds_1 <- get_selection(var_ds_box) %>% safe_names()
  summary_skim(.ds_1)
}
