# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export funs ----------------------------------------------------------------

#' Export dataset as R structure via dput
#'
#' @keywords internal
to_r_structure <- function() {
  # .ds <- get_selection(var_ds_box)
  .ds <- active_dataset_0()

  doItAndPrint(str_glue(
    "## Export as R structure ('{.ds}')\n",
    "dput({.ds})"
  ))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# to_pptx() — removed (dead code after stop())
# to_word() — removed (dead code after stop())
# Backed up to functions-under-development/removed/
