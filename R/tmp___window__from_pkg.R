# TODO:
#  1) create onOk() functio.
#  2) Prepare the most important helper functions.
#  3) Set different "selected_package", "selected_dataset" colors,
#     when appropriate.
#  4) Bind button and mouse clicks to actions.
#  5) Open help on dataset.


# TODO: May be useful:
# pkgs_all_p <- is.null(getRcmdr("bs_installed_packages", fail = FALSE))
# ifelse(pkgs_all_p, "::image::bs_package_go", "::image::bs_package_r")
# ifelse(
#     pkgs_all_p,
#     "List all installed packages. \n This option may be time consuming.",
#     "Refresh the list of all installed packages."
#   )

# ___ Main function ___ ======================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window__from_pkg <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  defaults <-
    list(
      selected_package  = "{none}",
      selected_dataset  = "{none}",
      which_packages    = "Loaded packages"
    )

  initial <- getDialog("window__from_pkg", defaults)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # tk_see_current_variable <- function() {
  #   tk_see(
  #     f1_box_pkgs,
  #     which(get_selection(f1_box_pkgs) == get_values(f1_box_pkgs)))
  # }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_loaded_packages <- function() {
    list_packages(which = "loaded") %>% setdiff(c("base", "stats"))
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_all_installed_packages <- function(force = FALSE) {
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get package list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pkgs_all <- getRcmdr("bs_installed_packages", fail = FALSE)

    if (isTRUE(force) || is.null(pkgs_all)) {
      pkgs_all <- list_packages("installed") %>% setdiff(c("base", "stats"))
      putRcmdr("bs_installed_packages", pkgs_all)
    }
    pkgs_all
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  refresh_all_installed_packages <- function() {
    get_all_installed_packages(force = TRUE)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_packages_list <- function() {
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get packages ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    switch(
      get_selection(f1_pkg_opts),

      "Loaded packages" = {
        tk_disable(f1_but_2_1)
        tk_disable(f1_box_pkgs)
        tk_normalize(f1_box_ds)

        pkgs <- get_loaded_packages()

        # FIXME: cache this operation:
        ds <- get_info_about_datasets(pkgs)
        set_values(f1_box_ds, ds$Dataset)

        tkconfigure(f1_but_2_0, image = "::image::bs_package_go")
        # tip(f1_but_2_0) <- "Refresh the list of all installed packages"
        # set_values(f1_box_pkgs, pkgs_all)

      },

      "From list: loaded only" = {
        tk_normalize(f1_but_2_1)
        tk_normalize(f1_box_pkgs)
        tk_normalize(f1_box_ds)

        pkgs <-
          list_packages(which = "loaded") %>%
          setdiff(c("base", "stats"))

        set_values(f1_box_pkgs, pkgs)
        set_values(f1_box_ds,   "")

        tkconfigure(f1_but_2_0, image = "::image::bs_package")
      },

      "From list: all installed" = {
        tk_normalize(f1_but_2_1)
        tk_normalize(f1_box_pkgs)
        tk_normalize(f1_box_ds)

        pkgs <- get_all_installed_packages(force = FALSE)

        set_values(f1_box_pkgs, pkgs)
        set_values(f1_box_ds,   "")

        tkconfigure(f1_but_2_0, image = "::image::bs_package_r")
      }
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  filter_ds_class <- function() {
    # TODO: create this function

  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  choose_package <- function() {
    pkg <- get_selection(f1_box_pkgs)

    tclvalue(f3_var_selected_ds)  <- initial$selected_dataset
    tclvalue(f3_var_info)         <- ""

    if (length(pkg) == 0) {
      set_values(f1_box_ds, " (no datasets)")
      tk_disable(f1_box_ds)
      return()
    }

    tk_normalize(f1_box_ds)
    set_values(f1_box_ds, list_datasets_in_package(pkg))

    tclvalue(f3_var_selected_pkg) <- pkg

  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  choose_dataset <- function() {
    # move_selected_row_in_tktext(recodes, move_to = "+1")
    ds  <- get_selection(f1_box_ds)

    if (length(ds) == 0) {
      return()
    }

    pkg <- get_selection(f1_box_pkgs)

    # tk_normalize(f1_box_ds)
    tclvalue(f3_var_selected_ds) <- ds
    # tclvalue(f3_var_info) <- get_ds_info(str_glue("{pkg}::{ds}"))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  reset_selection <- function() {

    set_values(f1_box_ds, NULL)

    tclvalue(f3_var_selected_pkg) <- initial$selected_package
    tclvalue(f3_var_selected_ds)  <- initial$selected_dataset
    tclvalue(f3_var_info)         <- ""
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onOK <- function() {
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    return()
    # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    recode_into       <- tclvalue(recode_intoVariable)
    variables         <- get_selection(f1_box_pkgs)
    selected_variable <- tclvalue(selected_variable)
    name              <- tclvalue_chr(newVariableName)

    # Read recode directives
    save_recodes <- trimws(tclvalue(tkget(recodes, "1.0", "end")))

    # Format code into one row
    recode_directives <- clean_directives(save_recodes)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    putDialog(
      "window__from_pkg",
      list(
        # make_factor       = make_factor,
        variables         = variables,
        name              = name,
        recode_directives = save_recodes,
        recode_into       = recode_into,
        selected_variable = selected_variable
      )
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # variables
    # If no variable is selected
    if (length(variables) == 0 || selected_variable == "{none}") {
      show_error_messages(
        parent = top,
        "No variable is selected.",
        str_c("Prease, select a variable."),
        title = "No Variable Selected"
      )
      return()
    }
    # Is empty? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (recode_directives == "") {
      show_error_messages(
        parent = top,
        "No recode directives were specified: the template was not filled.",
        str_c("No recode directives were specified: ",
          "the template was not filled.\n\n",
          "Please create a template and fill it where necessary. ",
          "Some (but not all) fields may be left empty.  \n\n",
          "To create a template, either use arrow buttons that are ",
          "between the boxes or double left/right click on a variable name."
        ),
        title = "Missing Recode Directives"
      )
      return()

    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is_empty_name(name, parent = top))              {return()}
    if (is_not_valid_name(name, parent = top))          {return()}
    if (forbid_to_replace_variable(name, parent = top)) {return()}
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    switch(
      recode_into,
      "nominal" = {
        recode_fun     <- "dplyr::recode_factor"
        ordered_factor <- ""                  # TODO: .ordered = FALSE ???
      },
      "ordinal" = {
        recode_fun     <- "dplyr::recode_factor"
        ordered_factor <- ", .ordered = TRUE"
      },
      "other" = {
        recode_fun     <- "dplyr::recode"
        ordered_factor <- ""
      })

    .ds <- active_dataset()

    command <- str_glue(
      "## ", gettext_bs("Recode variable values"), "\n\n",
      "{.ds} <- \n",
      "   {.ds} %>% \n",
      "   dplyr::mutate(\n",
      "   {name} = {recode_fun}({selected_variable}, \n",
      "   {recode_directives}{ordered_factor}))"
    )

    # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Library("tidyverse")

    result <- justDoIt(command)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (class(result)[1] != "try-error") {
      # closeDialog()
      logger(style_cmd(command))
      active_dataset(.ds, flushModel = FALSE, flushDialogMemory = FALSE)

    } else {
      logger_error(command, error_msg = result)
      show_code_evaluation_error_message()
      return()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkfocus(CommanderWindow())

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Announce about the success to run the function `onOk()`
    TRUE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  }


  # Title ------------------------------------------------------------------
  title_text <- gettext_bs("Load Data from R Packages")
  initializeDialog(title = title_text)
  tk_title(top, title_text)

  # Widgets ----------------------------------------------------------------

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f1 <- tkframe(top)

  f1_box_pkgs <-
    bs_listbox(
      parent = f1,
      height = 7,
      values = list_packages(which = "loaded"),
      on_double_click = choose_package,
      title = gettext_bs("Package \n(double-click to select)"),
      use_filter = TRUE,
      filter_label = "Filter packages"

    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f1_box_ds <-
    bs_listbox(
      parent = f1,
      height = 7,
      width = 30,
      values = "",
      on_select = choose_dataset,
      title = gettext_bs("Dataset \n(click to select)"),
      value = 1,
      use_filter = TRUE,
      filter_label = "Filter datasets"
    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  f1_but_set_2 <- tkframe(f1)

  f1_but_2_0 <- tk2button(
    parent  = f1_but_set_2,
    image   = "::image::bs_package_go",
    command = refresh_all_installed_packages,
    tip     = "Refresh the list of all installed packages."
  )

  f1_but_2_1 <- tk2button(
    parent  = f1_but_set_2,
    image   = "::image::bs_go_next",
    command =  choose_package,
    tip = "Select a package."
  )

  f1_but_2_2 <- tk2button(
    f1_but_set_2,
    image = "::image::bs_refresh",
    command = reset_selection,
    tip = "Reset selection."
  )

  tkgrid(f1_but_2_0)
  tkgrid(f1_but_2_1)
  tkgrid(f1_but_2_2)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  f1_but_set_1 <- tkframe(f1)

  # f1_but_1_1 <- tk2button(
  #   f1_but_set_1,
  #   image = "::image::bs_go_top",
  #   command = function() {
  #     move_selected_row_in_tktext(recodes, move_to = "top")
  #   },
  #   tip = "Move selected line \nto the top."
  # )
  #
  # f1_but_1_2 <- tk2button(
  #   f1_but_set_1,
  #   image = "::image::bs_go_up",
  #   command = function() {
  #     move_selected_row_in_tktext(recodes, move_to = "-1")
  #   },
  #   tip = "Move selected line \nup by 1 position."
  # )

  f1_but_1_3 <- tk2button(
    f1_but_set_1,
    image = "::image::bs_go_down",
    command = choose_dataset,
    tip = "Select a dataset"
    # tip = "Move selected line \ndown by 1 position."
  )

  # f1_but_1_4 <- tk2button(
  #   f1_but_set_1,
  #   image = "::image::bs_go_bottom",
  #   command = function() {
  #     move_selected_row_in_tktext(recodes, move_to = "end")
  #   },
  #   tip = "Move selected line \nto the bottom."
  # )

  # tkgrid(f1_but_1_1)
  # tkgrid(f1_but_1_2)
  tkgrid(f1_but_1_3)
  # tkgrid(f1_but_1_4)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  f1_pkg_opts <- bs_combobox(
    parent = f1,
    label  = "Which packages",
    label_position = "above",
    values = c(
      "Loaded packages",
      "From list: loaded only",
      "From list: all installed"),
    value = initial$which_packages,

    on_select = get_packages_list
  )

  f1_ds_opts <- bs_combobox(
    parent = f1,
    label  = "Dataset's class",
    label_position = "above",
    values = c("All", "Data frame", "Matrix", "Table", "List", "Other"),
    value = "Data frame",

    on_select = filter_ds_class
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # f2 <- tkframe(top)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f3 <- tkframe(top)

  f3_var_selected_pkg <- tclVar(initial$selected_package)
  f3_var_selected_ds  <- tclVar(initial$selected_dataset)
  f3_var_info         <- tclVar("")

  f3_lab_pkg_1 <- tk_label(f3, text = "Selected package: ")
  f3_lab_pkg_2 <- tk_label_red(f3, textvariable = f3_var_selected_pkg)

  f3_lab_data_1 <- tk_label(f3, text = "Selected dataset:   ")
  f3_lab_data_2 <- tk_label_red(f3, textvariable = f3_var_selected_ds)

  f3_lab_info   <- tk_label(f3, textvariable = f3_var_info)

  tkgrid(f3_lab_pkg_1,  f3_lab_pkg_2,  pady = c(10, 0), sticky = "w")
  tkgrid(f3_lab_data_1, f3_lab_data_2, pady = c(0, 2),  sticky = "w")
  tkgrid(f3_lab_info,                  pady = c(0, 9),  sticky = "w")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Help menus ---------------------------------------------------------------
  help_menu <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkadd(menu_main, "command",
      label    = "Function `data()`",
      command  = open_help("data", package = "utils"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }

  ok_cancel_help(
    on_help = help_menu,
    close_on_ok = TRUE,
    apply = "window__from_pkg()",
    after_apply_success_fun = function() {
      # new_name <- tclvalue_chr(newVariableName)
      #
      # set_values(f1_box_pkgs, variables_all())
      # tk_see(f1_box_pkgs, new_name)
      # set_selection(f1_box_pkgs, new_name)
      #
      # tclvalue(selected_variable) <- new_name
      #
      # tkdelete(recodes, "1.0", "end")
      #
      # tclvalue(newVariableName) <-
      #     unique_colnames(new_name, all_numbered = TRUE)
    })
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tkgrid(f1, sticky = "nw")

  tkgrid(
    f1_box_pkgs$frame,
    f1_but_set_2,
    f1_box_ds$frame,
    f1_but_set_1,
    sticky = "nw"
  )

  tkgrid(f1_pkg_opts$frame, "x", f1_ds_opts$frame, sticky = "we", pady = c(5, 0))

  tkgrid.configure(f1_but_set_1, sticky = "")
  tkgrid.configure(f1_but_set_2, sticky = "", padx = c(3, 9))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f2, sticky = "w")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f3, sticky = "w") # selected package and dataset labels.

  # ======================================================================~~~~

  tkgrid(buttonsFrame, sticky = "ew")
  # tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
  # tkgrid.configure(recodesXscroll, sticky = "ew")
  # tkgrid.configure(recodesYscroll, sticky = "ns")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dialogSuffix(bindReturn = FALSE)


  get_packages_list()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
