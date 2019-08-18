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
    Library("skimr")
    # Library("inspectdf")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # Each plot in a separate window
    # if (is_plot_in_separate_window()) {open_new_plots_window()}
    # doItAndPrint(str_glue(
    #     "## Variable sizes in memory \n",
    #     "{.ds} %>% inspect_mem() %>% show_plot()"
    # ))
    #
    # if (is_plot_in_separate_window()) {open_new_plots_window()}
    # doItAndPrint(str_glue(
    #     "## Plot of variable type frequency \n",
    #     "{.ds} %>% inspect_types() %>% show_plot()"
    # ))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    doItAndPrint(str_glue(
        "## Variable type summary and size \n",
        "## of dataset '{.ds}'\n",
        "summary(skimr::skim({.ds}))"
    ))

}

summary_var_types_plot_0 <- function(.ds) {
    Library("tidyverse")
    Library("inspectdf")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Each plot in a separate window
    if (is_plot_in_separate_window()) {open_new_plots_window()}
    doItAndPrint(str_glue(
        "## Variable sizes in memory \n",
        "{.ds} %>% inspect_mem() %>% show_plot()"
    ))

    if (is_plot_in_separate_window()) {open_new_plots_window()}
    doItAndPrint(str_glue(
        "## Plot of variable type frequency \n",
        "{.ds} %>% inspect_types() %>% show_plot()"
    ))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary_skim_0 <- function() {
    summary_skim(.ds = active_dataset_0())
}

summary_skim <- function(.ds) {
    Library("tidyverse")
    Library("skimr")

    if ((utils::packageVersion("skimr") < "2.0")) {

        doItAndPrint(
            str_c(
                "skimr::skim_with(\n",
                "    numeric = list(hist = NULL),\n",
                "    integer = list(hist = NULL) \n",
                ")\n\n"
            ))

        doItAndPrint(str_glue(
            "## Summary of the variables in dataset '{.ds}'\n",
            "skimr::skim({.ds})"
        ))

    } else {
        doItAndPrint(str_glue(
            "## Summary of the variables in dataset '{.ds}'\n",

            "do_skim <- skimr::skim_with(numeric = sfl(hist = NULL))\n",
            "do_skim({.ds})\n\n"
        ))
    }
}

