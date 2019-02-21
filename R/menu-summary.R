# "Summany" menu related functions ============================================

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

summary_var_types <- function() {
    summary_var_types_0(.ds = active_dataset_0())
}

summary_var_types_0 <- function(.ds) {
    Library("skimr")
    doItAndPrint(str_glue(
        "## The size & variable type summary of dataset '{.ds}'\n",
        "summary(skimr::skim({.ds}))"
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