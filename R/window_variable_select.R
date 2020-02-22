# TODO:
# 1. Add possibility to reorder variables
# 2. Check if `incorrect_cond_msg` argument is needed.
# 3. Create blue title for the window.
# 4. Buttons with options for dataset name: the asme name as original; new name

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Correctly initializes window `window_variable_select()`
window_variable_select0  <- function(variables) {
  window_variable_select()
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# new_dsname (character) - data frame name
# incorrect_cond_msg (character) - Message for incorrect expression.
window_variable_select <- function(new_dsname = NULL, incorrect_cond_msg = NULL) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  initializeDialog(title = gettext_bs("Select/Reorder/Remove Variables from Dataset"))
  # Functions ----------------------------------------------------------------
  clear_var_select_box <- function() {
    set_selection(var_select_box, 0, clear = TRUE)
  }

  clear_var_delete_box <- function() {
    set_selection(var_delete_box, 0, clear = TRUE)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  context_menu_select <- function() {
    if (tk_get_state(var_select_box) == "disabled") {
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = "Clear selection (for Include)",
      compound = "left",
      image    = "::image::bs_delete",
      command  = clear_var_select_box
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  context_menu_delete <- function() {

    if (tk_get_state(var_delete_box) == "disabled") {
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = "Clear selection (for Remove)",
      compound = "left",
      image    = "::image::bs_delete",
      command  = clear_var_delete_box
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }


  # Widgets ------------------------------------------------------------------
  upper_frame <- tkframe(top)

  var_select_box <-
    bs_listbox(
      parent     = upper_frame,
      values     = variables_all(),
      title      = gettext_bs("Select / Include \n(pick one or more)"),
      selectmode = "multiple",
      height     = 8,
      # on_select  = activate_var_delete_box,
      on_click_3 = context_menu_select,
      bind_row_swap = TRUE
    )

  var_delete_box <-
    bs_listbox(
      parent     = upper_frame,
      values     = variables_all(),
      title      = gettext_bs("Remove \n(pick one or more)"),
      selectmode = "multiple",
      height     = 8,
      # on_select  = activate_var_select_box,
      on_click_3 = context_menu_delete,
      bind_row_swap = TRUE
    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_but_set_1 <- tkframe(upper_frame)

  f2_but_1_1 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_go_top",
    command = function() {
      move_selected_row_in_box(var_select_box, move_to = "top")
    },
    tip = "Move selected lines \nto the top.\nCtrl + Up"
  )

  f2_but_1_2 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_go_up",
    command = function() {
      move_selected_row_in_box(var_select_box, move_to = "-1")
    },
    tip = "Move selected lines \nup by 1 position.\nAlt + Up"
  )

  f2_but_1_3 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_delete",
    command = clear_var_select_box,
    tip = "Clear selection."
  )

  f2_but_1_4 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_go_down",
    command = function() {
      move_selected_row_in_box(var_select_box, move_to = "+1")
    },
    tip = "Move selected lines \nup by 1 position.\nAlt + Down"
  )

  f2_but_1_5 <- tk2button(
    f2_but_set_1,
    image = "::image::bs_go_bottom",
    command = function() {
      move_selected_row_in_box(var_select_box, move_to = "end")
    },
    tip = "Move selected lines \nto the bottom.\nCtrl + Down"
  )

  tkgrid(f2_but_1_1)
  tkgrid(f2_but_1_2)
  tkgrid(f2_but_1_3)
  tkgrid(f2_but_1_4)
  tkgrid(f2_but_1_5)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_but_set_2 <- tkframe(upper_frame)

  f2_but_2_1 <- tk2button(
    f2_but_set_2,
    image = "::image::bs_go_top",
    command = function() {
      move_selected_row_in_box(var_delete_box, move_to = "top")
    },
    tip = "Move selected lines \nto the top.\nCtrl + Up"
  )

  f2_but_2_2 <- tk2button(
    f2_but_set_2,
    image = "::image::bs_go_up",
    command = function() {
      move_selected_row_in_box(var_delete_box, move_to = "-1")
    },
    tip = "Move selected lines \nup by 1 position.\nAlt + Up"
  )

  f2_but_2_3 <- tk2button(
    f2_but_set_2,
    image = "::image::bs_delete",
    command = clear_var_delete_box,
    tip = "Clear selection."
  )

  f2_but_2_4 <- tk2button(
    f2_but_set_2,
    image = "::image::bs_go_down",
    command = function() {
      move_selected_row_in_box(var_delete_box, move_to = "+1")
    },
    tip = "Move selected lines \nup by 1 position.\nAlt + Down"
  )

  f2_but_2_5 <- tk2button(
    f2_but_set_2,
    image = "::image::bs_go_bottom",
    command = function() {
      move_selected_row_in_box(var_delete_box, move_to = "end")
    },
    tip = "Move selected lines \nto the bottom.\nCtrl + Down"
  )

  tkgrid(f2_but_2_1)
  tkgrid(f2_but_2_2)
  tkgrid(f2_but_2_3)
  tkgrid(f2_but_2_4)
  tkgrid(f2_but_2_5)
  # ==========================================================================
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  middle_frame <- tkframe(top)

  everything_box <-
    bs_checkboxes(
      parent = middle_frame,
      boxes  = c("everything" =
          "Include all remaining columns (except explicitly removed ones)"),
      values = 0,
      layout = "horizontal",
      default_tip = paste0(
        "Include all remaining columns\n",
        "(except explicitly removed ones)\n",
        "after the selected ones.  \n",
        "Used to reorder columns.")
    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lower_frame <- tkframe(top)

  if (is.null(new_dsname))  new_dsname <- unique_df_name(active_dataset())

  new_dsname_variable <- tclVar(new_dsname)
  new_dsname_field    <- ttkentry(lower_frame,
    width = "60",
    textvariable = new_dsname_variable)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onOK <- function() {
    new_dsname     <- tclvalue_chr(new_dsname_variable)
    var_delete     <- get_selection(var_delete_box)
    var_select     <- get_selection(var_select_box)
    var_everything <- get_values(everything_box)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    closeDialog()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if ((length(var_select) + length(var_delete)) == 0) {
      errorCondition(
        recall = window_variable_select,
        message = gettext_bs("You must select one or more variables.")
      )
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check if dataset name already exists ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (new_dsname != active_dataset()) {

      if (new_dsname %in% listDataSets()) {
        if ("no" == tclvalue(checkReplace(
          new_dsname,
          gettext_bs("Data set")))) {
          window_variable_select(
            new_dsname = new_dsname,
            incorrect_cond_msg =
              str_glue('Chose other name than "{new_dsname}".'))
          return()
        }
      }

    } else {
      response <-
        tk_messageBox(
          # parent = top,
          parent = CommanderWindow(),
          caption = "Modify Dataset",
          message =
            sprintf(
              gettext_bs(str_c(
                "Variable(s):\n",
                "Explicitly selected to include: %d\n",
                "Explicitly selected to remove: %d\n",
                "Do you agree to modify your dataset?")),
              length(var_select), length(var_delete)
            ),
          icon    = "warning",
          type    = "yesno",
          default = "no"
        )

      if (response == "no") {
        onCancel()
        return()
      }

    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Library("dplyr")

    to_select <-
      if (length(var_select) > 0) {
        stringr::str_c(safe_names(var_select), collapse = ", ")
      } else {
        NULL
      }

    to_reorder <-
      # TODO: vars_reorder [???]

      if (var_everything) {
        "everything()"
      } else {
        NULL
      }

    to_delete <-
      if (length(var_delete) > 0) {
        stringr::str_c("-", safe_names(var_delete), collapse = ", ")
      } else {
        NULL
      }

    variables <- stringr::str_c(to_select, to_reorder, to_delete, sep = ", ")

    command <-
      str_glue("## Select, reorder, or remove variables \n",
        "{new_dsname} <- {active_dataset()} %>% \n",
        "dplyr::select({variables})") %>%
      style_cmd()

    result <- doItAndPrint(command)

    if (class(result)[1] !=  "try-error") {
      # Change active dataset
      active_dataset(new_dsname, flushModel = FALSE)
    }

    tkfocus(CommanderWindow())

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    command_dataset_refresh()
    tkfocus(CommanderWindow())
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ok_cancel_help(helpSubject = "select", helpPackage = "dplyr")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Grid ---------------------------------------------------------------------
  # tkgrid(
  #   tk_label(top,
  #     # fg = getRcmdr("title.color"),
  #     text = gettext_bs("Choose either variables to include, or variables to remove. ")),
  #   pady = c(0, 10))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(upper_frame, columnspan = 5, sticky = "n")
  tkgrid(
    f2_but_set_1,
    var_select_box$frame,
    var_delete_box$frame,
    f2_but_set_2,
    sticky = "s"
  )

  tkgrid.configure(f2_but_set_1, padx = c(0, 5))
  tkgrid.configure(var_delete_box$frame, padx = c(20, 0))
  tkgrid.configure(f2_but_set_2, padx = c(5, 0))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(middle_frame, sticky = "nw", columnspan = 5)
  tkgrid(everything_box$frame, sticky = "", pady = c(5, 0), padx = c(5, 0))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(lower_frame, sticky = "wn", columnspan = 5)

  tkgrid(
    tk_label(lower_frame,
      fg = getRcmdr("title.color"),
      text = gettext_bs("Name for modified dataset \n(with selected and without removed variables) ")),
    pady = c(10, 0),
    sticky = "nw")

  tkgrid(new_dsname_field, pady = c(0, 0), sticky = "")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(buttonsFrame, sticky = "ew", columnspan = 5)
  dialogSuffix(rows = 2, columns = 5)
}
