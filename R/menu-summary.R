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

    # Note if dataset has row names
    note_txt <-
        if (tibble::has_rownames(ds)) {
            str_c(str_glue("# NOTE: Dataset '{.ds}' has row names, which are not displayed."), "\n")
        } else {
            ""
        }

    doItAndPrint(str_glue(
        "## Top and bottom rows\n",
        "{note_txt}",
        "{.ds} %>% as.data.table() %>% print(topn = 5, nrows = 10)"
    ))

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

