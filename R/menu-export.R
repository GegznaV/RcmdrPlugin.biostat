# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export funs ----------------------------------------------------------------

to_r_structure <- function() {
  # .ds <- get_selection(var_ds_box)
  .ds <- active_dataset_0()

  doItAndPrint(str_glue(
    "## Export as R structure ('{.ds}')\n",
    "dput({.ds})"
  ))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
to_pptx <- function() {

  function_not_implemented()
  stop()

  # variables
  Library("tidyverse")
  Library("officer")

  doc <-
    read_pptx() %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type =  "title", str = "A title") %>%
    ph_with_table(type = "body", value = mtcars) %>%
    ph_with_text(type = "dt", str = format(Sys.Date()))

  print(doc, target = "ph_with_table.pptx")

  fs::file_show("ph_with_table.pptx")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# to_word
to_word <- function(variables) {
  function_not_implemented()
  stop()

  Library("tidyverse")
  Library("officer")

  f_name <- unique_file_name(str_glue("results_{Sys.Date()}.docx"))

  ds_name <- "swiss"
  ds <- swiss

  if (fs::file_exists(f_name)) {
    doc <- read_docx(f_name)
  } else {
    doc <- read_docx()
  }


  doc %>%
    # body_add_par(value = "dataset mtcars", style = "heading 1") %>%
    # body_add_break() %>%

    body_add_par(value = str_glue("Dataset '{ds_name}'"), style = "table title") %>%
    body_add_table(value = ds, style = "table_template" ) %>%
    body_end_section_portrait() %>%
    print(doc, target = f_name)

  fs::file_show(f_name)
}
