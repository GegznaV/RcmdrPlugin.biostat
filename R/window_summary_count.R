# TODO:
# 1. Add radiobuttons instead of "Summary as data frame" checkbox:
#   values: Print summary as
#     - "n-way table"
#     - "n-way table (flat)"     (ftable)
#     - "n-way table for report" (pander)
#     - "data frame"
#     - "data frame for report" (pander)
# 2. Create count variable/environment for each type of summary separately.
# 3. Add counts variable name entrance field.
#    Default to "n", "nn", "nnn", etc. Now default is "Freq"
# 4. Radiobuttons:
#    Keep summary object as:
#        1. data frame;
#        2. n-way table;
#        3. print results and remove the object
# 5. Inactivate name field if "Keep summary" is unchecked.
# 6. Create separate window to plot the results. There may be a separate
#    checkbox that automatically opens necessary plotting window:
#    - plot as bar plot;
#    - plot as mosaic plot;
# 7. Possibility to sort variables.
# 8. Add "vcd::assoc_stats()"
# 9. Correct help topic.
# 10. Add three boxes for variables as in "multi-way table.." in original Rcommander menu.
# 11. Acticate/Deactivate approptiate checkboxes if exactly 2 variables are selected.
#
# df %>% dplyr::count(..., ...);
# df %>% with(table(..., ...)) %>% as.data.frame()



#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_summary_count <- function() {
  # cur_env <- environment()

  # Functions --------------------------------------------------------------
  activate_checkboxes <- function() {
    x_var            <- getSelection(xBox)
    y_var            <- getSelection(yBox)
    z_var            <- getSelection(zBox)

    n_vars <- length(c(x_var, y_var, z_var))

    if (n_vars == 2) {
      tk_activate(chisq_testCheckBox)
      tk_activate(fisher_testCheckBox)
    } else {
      tk_disable(chisq_testCheckBox)
      tk_disable(fisher_testCheckBox)
      tclvalue(chisq_testVariable) <- "0"
      tclvalue(fisher_testVariable) <- "0"

    }
    if (n_vars >= 2) {
      tk_activate(assoc_statsCheckBox)
    } else {
      tk_disable(assoc_statsCheckBox)
      tclvalue(assoc_statsVariable) <- "0"
    }

  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Default values ---------------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  defaults <- list(
    # digits     = "NA",
    # gr_var     = NULL,
    x_var        = NULL,
    y_var        = NULL,
    z_var        = NULL,
    table_type   = "df",

    chisq_test   = FALSE,
    fisher_test  = FALSE,
    assoc_stats  = FALSE,
    keep_model   = FALSE
  )

  initial <- getDialog("window_summary_count", defaults)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Dialog elements --------------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # tabs =      c("dataTab", "optionsTab")
  # tab_names = c(" Data ",  " Options ")

  initializeDialog(title = gettext_bs("Frequency / Multi-way table"))

  # posthocFrame <- tkframe(posthocTab)
  # plotsFrame   <- tkframe(plotsTab)

  main_frame <- tkframe(top)

  # ** Data tab ------------------------------------------------------------
  # . Variable selection -----------------------------------------------------


  main_data_frame <- tkframe(main_frame)

  xBox <-
    bs_listbox(
      parent     = main_data_frame,
      values     = variables_all(),
      selectmode = "single",
      height     = 7,
      title      = gettext_bs("First/Row variable \n(select one)"),
      value      = initial$x_var,
      on_select  = activate_checkboxes)

  yBox <-
    bs_listbox(
      parent     = main_data_frame,
      values     = variables_all(),
      selectmode = "single",
      height     = 7,
      title      = gettext_bs("Second/Column variable \n(select one or none)"),
      value      = initial$y_var,
      on_select  = activate_checkboxes)

  zBox <-
    bs_listbox(
      parent     = main_data_frame,
      values     = variables_all(),
      selectmode = "multiple",
      height     = 7,
      title      = gettext_bs("Other/Control variables \n(select one, several or none)"),
      value      = initial$z_var,
      on_select  = activate_checkboxes)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ** Options tab ---------------------------------------------------------

  # Middle frame ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  main_middle_frame <- tkframe(main_frame)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Rcmdr::radioButtons(
    main_middle_frame,
    name         = "table_type",
    buttons      = c("df", "multiway"),
    labels       = gettext_bs(c("Frequency table\n(data frame)", "Multi-way table")),
    initialValue = initial$table_type,
    title        = gettext_bs("Type of table:")
  )

  # Middle right frame ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  main_middle_right_frame <- tkframe(main_middle_frame)

  bs_check_boxes(main_middle_right_frame,
    title = "Options: \n(enabled when exactly 2 variables are selected)",
    # ttk = TRUE,
    frame = "checkboxes_frame",
    boxes = c("chisq_test",
      "fisher_test",
      "assoc_stats"
    ),
    initialValues = c(
      initial$chisq_test,
      initial$fisher_test,
      initial$assoc_stats),
    labels = gettext_bs(
      c("Pearson's chi-square test",
        "Fisher's exact test",
        "Measures of association for categorical variables")
    ),
    commands = list("chisq_test"  = function() {},
      "fisher_test" = function() {},
      "assoc_stats" = function() {})
  )

  # Choose model name ------------------------------------------------------
  main_frame_b <- tkframe(main_frame)

  initial_model_name <- unique_obj_names(
    active_dataset(),
    suffix       = "_freq_table",
    all_numbered = TRUE)

  model_name_var     <- tclVar(initial_model_name)
  model_name_box     <- ttkentry(main_frame_b,
    width        = "20",
    textvariable = model_name_var)

  keep_model_frame <- tkframe(main_frame_b)
  bs_check_boxes(
    keep_model_frame,
    frame = "keep_model_inner_frame",
    boxes = c("keep_model"),
    initialValues = c(
      initial$keep_model),
    labels = gettext_bs(
      c("Keep summary in R memory")
    ),
    commands = list("keep_model"  = function() {})
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  activate_checkboxes()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onOK <- function() {
    # gr_var         <- get_selection(groupBox)
    x_var            <- get_selection(xBox)
    y_var            <- get_selection(yBox)
    z_var            <- get_selection(zBox)
    # digits         <- suppressWarnings(tclvalue_int(digitsVar))

    table_type       <- tclvalue(table_typeVariable)
    # as_df          <- tclvalue_lgl(as_dfVariable)

    model_name       <- tclvalue_chr(model_name_var)
    keep_model       <- tclvalue_lgl(keep_modelVariable)
    chisq_test       <- tclvalue_lgl(chisq_testVariable)
    fisher_test      <- tclvalue_lgl(fisher_testVariable)
    assoc_stats      <- tclvalue_lgl(assoc_statsVariable)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    closeDialog()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (!is.valid.name(model_name)) {
      errorCondition(recall = window_summary_count,
        message = sprintf(gettext_bs("\"%s\" is not a valid name."),
          model_name))
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (model_name %in% objects()) {
      if (replace_object(model_name) == FALSE) {
        window_summary_count()
        return()
      }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(c(x_var, y_var, z_var)) == 0) {
      errorCondition(
        recall  = window_summary_count,
        message = gettext_bs("You must select a variable to summarize.")
      )
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    putDialog("window_summary_count",
      list(
        # gr_var = gr_var,
        # digits = as.character(digits),
        # as_df = as_df,

        x_var        = x_var,
        y_var        = y_var,
        z_var        = z_var,
        table_type   = table_type,

        chisq_test   = chisq_test,
        fisher_test  = fisher_test,
        assoc_stats  = assoc_stats,
        keep_model   = keep_model

      )
    )

    # calculations -------------------------------------------------------
    .ds <- active_dataset_0()

    Library("tidyverse")

    vars_select <- c(x_var, y_var, z_var)
    all_vars <- stringr::str_c(safe_names(vars_select), collapse = ", ")

    n_vars <- length(vars_select)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    chisq_cmd  <-
      if (chisq_test & (n_vars == 2)) {
        "chisq.test({my_table})\n"
      } else {
        ""
      }

    fisher_cmd <-
      if (fisher_test & (n_vars == 2)) {
        "fisher.test({my_table})\n"
      } else {
        ""
      }

    assoc_cmd <-
      # More than 2 variables are possible
      if (assoc_stats & (n_vars >= 2)) {
        Library("vcd")
        "vcd::assocstats({my_table})\n"
      } else  {
        ""
      }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    freq_name <- unique_colnames("n")
    as_df_command <-
      switch(table_type,
        "df"       = '{model_name} <- as.data.frame({my_table}, responseName = "{freq_name}")\n',
        "multiway" = "{model_name} <- {my_table}\n"
      )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (keep_model) {
      keep_model_command <- ""
    } else {
      keep_model_command <- str_glue("remove({model_name})")
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    my_table <- unique_obj_names("table", all_numbered = TRUE)
    command1 <-
      str_glue("## Frequency table / Multi-way table\n",
        "{my_table} <- {.ds} %>% \n",
        'with(table({all_vars}, useNA = "ifany"))\n',
        as_df_command,
        "print({model_name})\n",
        keep_model_command) %>%
      style_cmd()

    command2 <-
      str_glue(chisq_cmd,
        fisher_cmd,
        assoc_cmd,
        "remove({my_table})") %>%
      style_cmd()

    doItAndPrint(command1)
    doItAndPrint(command2)

    # Post calculations --------------------------------------------------
    # activeModel(model_name)
    # putRcmdr("modelWithSubset", FALSE)

    tkfocus(CommanderWindow())
  }
  # ========================================================================
  tkgrid(main_frame, sticky = "w")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(main_data_frame, columnspan = 2, sticky = "sw")
  tkgrid(
    getFrame(xBox), getFrame(yBox), getFrame(zBox),
    sticky = "nw",
    pady = c(5, 5)
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(main_frame_b, columnspan = 2, sticky = "sw", pady = c(10, 10))
  tkgrid(labelRcmdr(main_frame_b,
    text = gettext_bs("Enter name for summary table: "),
    fg = Rcmdr::getRcmdr("title.color")),
  sticky = "w")

  tkgrid(keep_model_inner_frame, padx = c(10, 0))
  tkgrid(model_name_box, keep_model_frame, sticky = "ew")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(main_middle_frame, columnspan = 2, sticky = "sw")
  tkgrid(table_typeFrame, main_middle_right_frame, sticky = "new")

  tkgrid(checkboxes_frame, sticky = "nw", padx = c(20, 0))

  # ** Footer ------------------------------------------------------------------
  # OKCancelHelp()
  ok_cancel_help(
    helpSubject = "table",
    # helpPackage = "base",
    model = TRUE,
    reset = "window_summary_count",
    apply = "window_summary_count"
  )
  tkgrid(buttonsFrame, sticky = "ew", columnspan = 2, pady = c(10, 0))

  dialogSuffix(rows = 4,
    columns = 2,
    preventGrabFocus = TRUE)
  # dialogSuffix(rows = 1,
  #              columns = 2,
  #              preventGrabFocus = TRUE)
}
# ==============================================================================
