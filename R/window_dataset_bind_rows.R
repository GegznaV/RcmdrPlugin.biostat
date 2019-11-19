# TODO:
# 1. Add radiobuttons with values:
#          "Use numeric ID", "Use named ID", "Do not use ID"
#
# 2. [!!!] functions to get and put diaglog are needed.
# 3. [!!!] Check if id name does not have duplicated names in any of datasets
#          in: id_name_variable

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_bind_rows <- function() {
  # Functions --------------------------------------------------------------
  set_id_name1 <- function() {
    if (tclvalue(which_idVariable) == "id_names") {
      tclvalue(ds_1_id_var) <- get_selection(ds_1_box)
    }
  }

  set_id_name2 <- function() {
    if (tclvalue(which_idVariable) == "id_names") {
      tclvalue(ds_2_id_var) <- get_selection(ds_2_box)
    }
  }

  set_id_name3 <- function() {
    if (tclvalue(which_idVariable) == "id_names") {
      tclvalue(ds_3_id_var) <- get_selection(ds_3_box)
    }
  }

  set_ds_name <- function() {
    base_name        <- paste0(getSelection(ds_1_box), "_with_rows_added")
    unique_base_name <- unique_df_name(base_name, all_numbered = TRUE)
    tclvalue(new_ds_name_variable) <- unique_base_name
  }

  id_names_fun <- function(variables) {
    tk_activate(entry_id_name)
    tk_activate(ds_1_id_entry)
    tk_activate(ds_2_id_entry)
    tk_activate(ds_3_id_entry)

    tclvalue(id_name_variable) <- ".from_dataset"
    set_id_name1()
    set_id_name2()
    set_id_name3()
  }

  id_numeric_fun <- function(variables) {
    tk_activate(entry_id_name)
    tk_disable(ds_1_id_entry)
    tk_disable(ds_2_id_entry)
    tk_disable(ds_3_id_entry)

    tclvalue(id_name_variable) <- ".from_dataset"
    tclvalue(ds_1_id_var) <- ""
    tclvalue(ds_2_id_var) <- ""
    tclvalue(ds_3_id_var) <- ""
  }

  id_none_fun <- function(variables) {
    tk_disable(entry_id_name)
    tk_disable(ds_1_id_entry)
    tk_disable(ds_2_id_entry)
    tk_disable(ds_3_id_entry)

    tclvalue(id_name_variable) <- ""
    tclvalue(ds_1_id_var) <- ""
    tclvalue(ds_2_id_var) <- ""
    tclvalue(ds_3_id_var) <- ""

  }

  choose_id_fun <- function(variables) {
    switch(tclvalue(which_idVariable),
      id_names   = id_names_fun(),
      id_numeric = id_numeric_fun(),
      id_none    = id_none_fun()
    )
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  win_title <- gettext_bs("Bind rows of datasets")
  initializeDialog(title = win_title)
  tk_title(top, win_title, pady = c(5, 15), columnspan = 3)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Widgets ----------------------------------------------------------------
  new_ds_name_variable <- tclVar(
    unique_df_name("rows_added", all_numbered = TRUE))

  id_name_variable <- tclVar(".from_dataset")

  # new_df_name_frame  <- tkframe(top)
  enter_names_frame  <- tkframe(top)
  entry_dsname       <- ttkentry(enter_names_frame, width = "42",
    textvariable = new_ds_name_variable)

  # id_var_name  <- tkframe(top)
  entry_id_name   <- ttkentry(enter_names_frame, width = "42",
    textvariable = id_name_variable)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Boxes for dataset ID
  ds_1_id_var <- tclVar("")
  ds_2_id_var <- tclVar("")
  ds_3_id_var <- tclVar("")
  ds_1_id_entry <- ttkentry(top, width = "21", textvariable = ds_1_id_var)
  ds_2_id_entry <- ttkentry(top, width = "21", textvariable = ds_2_id_var)
  ds_3_id_entry <- ttkentry(top, width = "21", textvariable = ds_3_id_var)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  radiobuttons_frame <- tkframe(top)
  radioButtons_horizontal(
    radiobuttons_frame,
    name = "which_id",
    buttons = c("id_names", "id_numeric","id_none"),
    values  = c("id_names", "id_numeric","id_none"),
    labels  =  gettext_bs(c("Names  ",
      "Numeric (1, 2, ...) ",
      "Do not use ID")),
    command = choose_id_fun
    # initialValue = initial$which_names,
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dataSets <- listDataSets()
  .ds      <- active_dataset()

  ds_1_box <-
    bs_listbox(
      parent = top,
      values = dataSets,
      value  = .ds,
      height = 7,
      title  = gettext_bs("First dataset \n(pick one)"),
      on_select = function() {
        set_id_name1()
        set_ds_name()
      })

  ds_2_box <-
    bs_listbox(
      parent    = top,
      values    = dataSets,
      height    = 7,
      on_select = set_id_name2,
      title     = gettext_bs("Second dataset \n(pick one)"))

  ds_3_box <-
    bs_listbox(
      parent    = top,
      values    = dataSets,
      height    = 7,
      on_select = set_id_name3,
      title     = gettext_bs("Third dataset \n(pick one or none)"))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  set_id_name1()
  set_id_name2()
  set_id_name3()
  set_ds_name()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onOK <- function() {
    new_ds_name <- tclvalue_chr(new_ds_name_variable)
    id_name     <- tclvalue_chr(id_name_variable)

    name_ds_1 <- get_selection(ds_1_box)
    name_ds_2 <- get_selection(ds_2_box)
    name_ds_3 <- get_selection(ds_3_box)

    ds_1_id <- tclvalue_chr(ds_1_id_var)
    ds_2_id <- tclvalue_chr(ds_2_id_var)
    ds_3_id <- tclvalue_chr(ds_3_id_var)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    closeDialog()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (new_ds_name == "") {
      errorCondition(
        recall = window_dataset_bind_rows,
        message = gettext_bs("You must enter the name of the new dataset.")
      )
      return()
    }

    if (!is.valid.name(new_ds_name)) {
      errorCondition(
        recall = window_dataset_bind_rows,
        message = str_glue('"{new_ds_name}" ',
          gettext_bs("is not a valid name."))
      )
      return()
    }

    if (is.element(new_ds_name, listDataSets())) {
      if ("no" == tclvalue(checkReplace(new_ds_name,
        gettext_bs("Dataset")))) {
        closeDialog()
        window_dataset_bind_rows()
        return()
      }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(name_ds_1) == 0) {
      errorCondition(
        recall = window_dataset_bind_rows,
        message = gettext_bs("You must select the first dataset.")
      )
      return()
    }
    if (length(c(name_ds_1, name_ds_2, name_ds_3)) < 2) {
      errorCondition(
        recall = window_dataset_bind_rows,
        message = gettext_bs("You must select at least two datasets.")
      )
      return()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ds_names <- c(name_ds_1, name_ds_2, name_ds_3)

    switch(tclvalue(which_idVariable),
      id_names   = {
        if (ds_1_id == "") ds_1_id <- name_ds_1
        if (ds_2_id == "") ds_2_id <- name_ds_2
        if (ds_3_id == "") ds_3_id <- name_ds_3
        ds_ids <- c(ds_1_id, ds_2_id, ds_3_id)

        ds_names_cmd <-
          stringr::str_c(safe_names(ds_ids), " = ", safe_names(ds_names),
            collapse = ", \n")
        use_ids <- TRUE
      },
      id_numeric = {
        ds_names_cmd <-
          stringr::str_c(safe_names(ds_names), collapse = ", ")
        use_ids <- TRUE

      },
      id_none    = {
        ds_names_cmd <-
          stringr::str_c(safe_names(ds_names), collapse = ", ")
        use_ids <- FALSE
      }
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    annotation <- "## Bind rows of datasets \n"
    if (use_ids) {
      # Use .id variable
      command <- style_cmd(str_glue(
        annotation,
        "{new_ds_name} <- ",
        "dplyr::bind_rows(\n{ds_names_cmd}, \n",
        '.id = "{id_name}")'))

    } else {
      # No .id variable
      command <- style_cmd(str_glue(
        annotation,
        "{new_ds_name} <- ",
        "dplyr::bind_rows(\n{ds_names_cmd})"))
    }

    doItAndPrint(command)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    active_dataset(new_ds_name)
    tkfocus(CommanderWindow())
  }
  # Layout -----------------------------------------------------------------
  ok_cancel_help(helpSubject = "bind_rows", helpPackage = "dplyr")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(ds_1_box$frame, ds_2_box$frame, ds_3_box$frame, sticky = "nwe")

  # text_id <- gettext_bs("Name in ID column\n(optional):")
  tkgrid(
    labelRcmdr(top, fg = getRcmdr("title.color"),
      text = gettext_bs("First dataset's ID:")),
    labelRcmdr(top, fg = getRcmdr("title.color"),
      text = gettext_bs("Second dataset's ID:")),
    labelRcmdr(top, fg = getRcmdr("title.color"),
      text = gettext_bs("Third dataset's ID:")),
    sticky = "w")

  tkgrid(ds_1_id_entry, ds_2_id_entry, ds_3_id_entry, sticky = "w")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(radiobuttons_frame, sticky = "w", pady = c(10, 0), columnspan = 3)
  tkgrid(
    tk_label_blue(radiobuttons_frame, text = gettext_bs("Type of datasets' ID:   ")),
    which_idFrame
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(enter_names_frame, pady = c(5, 5), columnspan = 3, sticky = "sw")
  # tkgrid(id_var_name, pady = c(15, 5), columnspan = 3, sticky = "ew")
  tkgrid(
    tk_label_blue(enter_names_frame, text = gettext_bs(paste0("Name for ID column:    "))),
    entry_id_name,
    sticky = "w"
  )

  # tkgrid(entry_id_name, sticky = "w")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # tkgrid(new_df_name_frame, pady = c(0, 0), columnspan = 3, sticky = "sw")
  tkgrid(
    tk_label_blue(
      enter_names_frame,
      text = gettext_bs("Name for resulting dataset:  ")),
    entry_dsname, pady = c(5, 0), sticky = "w")

  # tkgrid(
  #     commonButton,
  #     labelRcmdr(commonFrame,
  #                text = gettext_bs("Merge only common\nrows or columns")),
  #     sticky = "nw"
  # )

  tkgrid(buttonsFrame, sticky = "we", columnspan = 3)
  dialogSuffix()
}
