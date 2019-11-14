# FIXME:
#   1. cope with non data frames correctly
#   2. Filter out packages without datasets

# TODO:
#  1) Prepare the most important helper functions.
#  2) Remove red package's icon
#  3) f3_lab_info is not used

# TODO: May be useful:
# pkgs_all_p <- is.null(getRcmdr("bs_installed_packages", fail = FALSE))
# ifelse(pkgs_all_p, "::image::bs_package_go", "::image::bs_package_r")
# ifelse(
#     pkgs_all_p,
#     "List all installed packages. \n This option may be time consuming.",
#     "Refresh the list of all installed packages."
#   )




#  ===========================================================================
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_list_datasets_in_pkgs <- function() {
  doItAndPrint("## List datasets in attached R packages\ndata()")
}

# ___ Main function ___ ======================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_import_from_pkg <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  defaults <-
    list(
      selected_package  = "{none}",
      selected_dataset  = "{none}",
      which_packages    = "Any loaded package",
      which_data_type   = "Data frame"
    )

  initial <- getDialog("window_import_from_pkg", defaults)
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
  update_packages_list <- function() {
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get packages ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    switch(
      get_selection(f1_pkg_opts),

      "Any loaded package" = {

        set_values(f1_box_pkgs, "(All loaded packages)")

        tk_disable(f1_but_2_1)
        tk_disable(f1_box_pkgs)
        tk_normalize(f1_box_ds)

        pkgs <- get_loaded_packages()

        # FIXME: cache this operation:
        ds <-
          get_info_about_datasets(pkgs) %>%
          dplyr::filter(is_data_frame)

        set_values(f1_box_ds, ds$Dataset)

        tkconfigure(f1_but_2_0, image = "::image::bs_package_go")
        # tip(f1_but_2_0) <- "Refresh the list of all installed packages"
        # set_values(f1_box_pkgs, pkgs_all)
        set_values(f3_var_selected_pkg, "{Any loaded package}")
        tk_set_color(f3_lab_pkg_2, "grey")
      },

      "Selected (loaded only)" = {
        tk_normalize(f1_but_2_1)
        tk_normalize(f1_box_pkgs)
        tk_normalize(f1_box_ds)

        pkgs <-
          list_packages(which = "loaded") %>%
          setdiff(c("base", "stats"))

        set_values(f1_box_pkgs, pkgs)
        disable_datasets_list()

        tkconfigure(f1_but_2_0, image = "::image::bs_package")
        set_values(f3_var_selected_pkg, initial$selected_package)
        tk_set_color(f3_lab_pkg_2, "darkred")

      },

      "Selected (all installed)" = {
        tk_normalize(f1_but_2_1)
        tk_normalize(f1_box_pkgs)
        tk_normalize(f1_box_ds)

        pkgs <- get_all_installed_packages(force = FALSE)

        set_values(f1_box_pkgs, pkgs)
        disable_datasets_list()

        tkconfigure(f1_but_2_0, image = "::image::bs_package_r")
        set_values(f3_var_selected_pkg, initial$selected_package)
        tk_set_color(f3_lab_pkg_2, "darkred")
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

    tk_set_color(f3_lab_ds_2, "darkred")
    tclvalue(f3_var_selected_ds) <- initial$selected_dataset
    tclvalue(f3_var_info)        <- ""
    ds <- list_datasets_in_package(pkg)
    if (length(ds) == 0) {
      disable_datasets_list()
      tk_set_color(f3_lab_pkg_2, "darkred")
      tclvalue(f3_var_selected_pkg) <- initial$selected_package

      return()
    }

    tk_normalize(f1_box_ds)
    set_values(f1_box_ds, list_datasets_in_package(pkg))

    tk_set_color(f3_lab_pkg_2, "darkgreen")
    tclvalue(f3_var_selected_pkg) <- pkg
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  choose_dataset <- function() {
    # move_selected_row_in_tktext(recodes, move_to = "+1")
    ds  <- get_selection(f1_box_ds)

    if (length(ds) == 0) {
      return()
    }

    # pkg <- get_selection(f1_box_pkgs)

    # tk_normalize(f1_box_ds)
    tclvalue(f3_var_selected_ds) <- ds
    tk_set_color(f3_lab_ds_2, "darkgreen")
    # tclvalue(f3_var_info) <- get_ds_info(str_glue("{pkg}::{ds}"))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  disable_datasets_list <- function() {
    set_values(f1_box_ds, " (no datasets)")
    tk_disable(f1_box_ds)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  reset_selection <- function() {

    set_values(f1_box_ds, NULL)

    tk_set_color(f3_lab_pkg_2, "darkred")
    tk_set_color(f3_lab_ds_2,  "darkred")

    tclvalue(f3_var_selected_pkg) <- initial$selected_package
    tclvalue(f3_var_selected_ds)  <- initial$selected_dataset
    tclvalue(f3_var_info)         <- ""
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ds_context_menu_load <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

    ds <- get_selection(f1_box_ds)
    pkg <- get_selection(f1_box_pkgs)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(ds) == 0) {
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(pkg) == 0) {
      pkg <- NULL
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = str_glue("Load dataset '{ds}'"),
      compound = "left",
      image    = "::image::bs_load_pkg",
      command  = onOK
    )

    tkadd(menu_main, "command",
      label    = "Load dataset and close",
      compound = "left",
      image    = "::image::bs_load_pkg_c",
      command  = function() {
        onOK()
        close_dialog()
      })

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = str_glue("Documentation on '{ds}'"),
      compound = "left",
      image    = "::image::bs_help",
      command  = open_help(ds, package = pkg)
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ds_context_menu_help <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

    ds  <- get_selection(f1_box_ds)
    pkg <- get_selection(f1_box_pkgs)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(ds) == 0) {
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(pkg) == 0) {
      pkg <- NULL
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = str_glue("Documentation on '{ds}'"),
      compound = "left",
      image    = "::image::bs_help",
      command  = open_help(ds, package = pkg)
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onOK <- function() {
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    selected_pkg <- tclvalue(f3_var_selected_pkg)
    selected_ds  <- tclvalue(f3_var_selected_ds)


    if (str_detect(selected_ds, fixed("{"))) {
      selected_ds <- NULL
    }

    if (str_detect(selected_pkg, fixed("{"))) {
      selected_pkg <- NULL
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (variable_is_not_selected(selected_ds, "dataset", parent = top)) {
      return()
    }
    if (forbid_to_replace_object(selected_ds, parent = top)) {return()}
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    putDialog("window_import_from_pkg", list(
      which_packages  = get_selection(f1_pkg_opts),
      which_data_type = get_selection(f1_ds_opts)
    ))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    command_load <-
      if (is.null(selected_pkg)) {
        str_glue('data("{selected_ds}")')
      } else {
        str_glue('data("{selected_ds}", package = "{selected_pkg}")')
      }

    command <- str_glue(
      "## Load data from R package \n",
      "{command_load}"
    )

    # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    result <- justDoIt(command)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (class(result)[1] != "try-error") {
      logger(style_cmd(command))

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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2 <- tkframe(top)
  f3_var_selected_pkg <- tclVar(initial$selected_package)
  f3_lab_pkg_1 <- tk_label(f2, text = "Selected package: ")
  f3_lab_pkg_2 <- tk_label_red(f2, textvariable = f3_var_selected_pkg)

  f3_var_selected_ds  <- tclVar(initial$selected_dataset)
  f3_lab_ds_1 <- tk_label(f2, text = "Selected dataset:   ")
  f3_lab_ds_2 <- tk_label_red(f2, textvariable = f3_var_selected_ds)



  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f1 <- tkframe(top)

  f1_box_pkgs <-
    bs_listbox(
      parent = f1,
      height = 7,
      width = 25,
      values = list_packages(which = "loaded"),
      on_double_click = function() {
        if (tk_get_state(f1_box_pkgs) == "disabled") {return()}
        choose_package()
      },
      title = gettext_bs("Package \n(double-click to select)"),
      use_filter = TRUE,
      filter_label = "Filter packages"

    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f1_box_ds <-
    bs_listbox(
      parent = f1,
      height = 7,
      width = 28,
      values = "",
      on_select = choose_dataset,
      on_double_click = ds_context_menu_load,
      on_click_3      =  ds_context_menu_help,
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

  # f1_but_set_1 <- tkframe(f1)

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

  # f1_but_1_3 <- tk2button(
  #   f1_but_set_1,
  #   image = "::image::bs_go_down",
  #   command = choose_dataset,
  #   tip = "Select a dataset"
  #   # tip = "Move selected line \ndown by 1 position."
  # )

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
  # tkgrid(f1_but_1_3)
  # tkgrid(f1_but_1_4)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  f1_pkg_opts <- bs_combobox(
    parent = f1,
    width  = 22,
    label  = "Which package",
    label_position = "above",
    values = c(
      "Any loaded package",
      "Selected (loaded only)",
      "Selected (all installed)"),
    value = initial$which_packages,

    on_select = update_packages_list
  )

  f1_ds_opts <- bs_combobox(
    parent = f1,
    label  = "Dataset's class",
    label_position = "above",
    values = c("All", "Data frame", "Matrix", "Table", "List", "Other"),
    value = initial$which_data_type,

    on_select = filter_ds_class
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f3 <- tkframe(top)

  # f3_var_info <- tclVar("")
  # f3_lab_info <- tk_label(f3, textvariable = f3_var_info)

  tkgrid(f3_lab_pkg_1, f3_lab_pkg_2,  pady = c(0, 0), sticky = "w")
  tkgrid(f3_lab_ds_1,  f3_lab_ds_2,   pady = c(0, 2),  sticky = "w")
  # tkgrid(f3_lab_info,                 pady = c(0, 9),  sticky = "w")

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
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ok_cancel_help(
    on_help = help_menu,
    close_on_ok = TRUE,
    apply = "window_import_from_pkg()",
    apply_label = "Load"
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f2, sticky = "w")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f1, sticky = "nw")

  tkgrid(
    f1_box_pkgs$frame,
    f1_but_set_2,
    f1_box_ds$frame,
    # f1_but_set_1,
    sticky = "nw"
  )

  tkgrid(f1_pkg_opts$frame, "x",
    # f1_ds_opts$frame,
    sticky = "we", pady = c(5, 0))

  # tkgrid.configure(f1_but_set_1, sticky = "")
  tkgrid.configure(f1_but_set_2, sticky = "", padx = c(3, 9))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f3, sticky = "w") # selected package and dataset labels.

  # ======================================================================~~~~

  tkgrid(buttonsFrame, sticky = "ew")
  # tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
  # tkgrid.configure(recodesXscroll, sticky = "ew")
  # tkgrid.configure(recodesYscroll, sticky = "ns")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dialogSuffix(bindReturn = FALSE)

  update_packages_list()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
