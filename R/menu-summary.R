# "Summany" menu related functions ============================================

#  Overview ------------------------------------------------------------------
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_dim <- function() {
    doItAndPrint(str_glue("## Number of rows and columns\n",
                          "dim({ActiveDataSet()})"))
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
            "dplyr::glimpse({ActiveDataSet()})"
        ) %>%
        style_cmd()

    doItAndPrint(command)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
summary_head_tail <- function() {
    .ds <- ActiveDataSet()
     ds <- eval_text(.ds, envir = .GlobalEnv)

    Library("tidyverse")
    Library("data.table")

    doItAndPrint(command)

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

