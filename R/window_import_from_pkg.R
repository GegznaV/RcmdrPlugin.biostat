
#  ===========================================================================
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_list_datasets_in_pkgs <- function() {
  doItAndPrint(str_c(
    "## List datasets in loaded R packages\n",
    "data()"
  ))
}

command_list_datasets_in_pkg <- function(pkg) {
  function() {
    doItAndPrint(str_glue(
      '## List datasets in package "{pkg}"\n',
      'data(package = "{pkg}")'
    ))
  }

}

# ___ Main function ___ ======================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_import_from_pkg <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  defaults <-
    list(
      selected_package  = "{none}",
      selected_dataset  = "{none}",
      which_packages    = "Any loaded package"
    )

  initial <- getDialog("window_import_from_pkg", defaults)

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

    set_values(f2_var_info, "")
    tkconfigure(f2_lab_info, foreground = "darkgreen")
    tk_disable(f2_but_2_3)

    # Get packages ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    opt <- get_selection(f2_pkg_opts)


    switch(
      opt,

      "Any loaded package" = {

        set_values(f2_box_pkgs, "(All loaded packages)")
        tk_disable(f2_box_pkgs)

        tk_disable(f2_but_2_1)
        tk_normalize(f2_box_ds)

        pkgs <- get_loaded_packages()

        ds <- get_ds_list(pkgs)
        set_values(f2_box_ds, ds)

        tkconfigure(f2_icon, image = "::image::bs_package_go")
        tip(f2_icon) <- "Datasets from any \nloaded package"

        set_values(f1_var_selected_pkg, "{Any loaded package}")
        tk_set_color(f1_lab_selected_pkg, "grey")
      },

      "Selected (loaded only)" = {
        tk_normalize(f2_box_pkgs)

        tk_disable(f2_but_2_1)
        tk_normalize(f2_box_ds)

        pkgs <-
          list_packages(which = "loaded") %>%
          setdiff(c("base", "stats"))

        set_values(f2_box_pkgs, pkgs)
        disable_datasets_list()

        tkconfigure(f2_icon, image = "::image::bs_package")
        tip(f2_icon) <- "Datasets from selected \npackage that is loaded"

        set_values(f1_var_selected_pkg, defaults$selected_package)
        tk_set_color(f1_lab_selected_pkg, "darkred")

      },

      "Selected (all installed)" = {
        tk_normalize(f2_box_pkgs)

        tk_normalize(f2_but_2_1)
        tk_normalize(f2_box_ds)

        pkgs <- get_all_installed_packages(force = FALSE)

        set_values(f2_box_pkgs, pkgs)
        disable_datasets_list()

        tkconfigure(f2_icon, image = "::image::bs_package_br")
        tip(f2_icon) <- "Datasets from selected \npackage that is installed"

        set_values(f1_var_selected_pkg, defaults$selected_package)
        tk_set_color(f1_lab_selected_pkg, "darkred")
      }
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_selected_pkg <- function() {
    # Return either string or NULL
    pkg <- get_selection(f2_box_pkgs)
    if (length(pkg) == 0) {
      pkg <- NULL
    }
    pkg
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_selected_ds <- function() {
    # Return either string or NULL
    .ds <- get_selection(f2_box_ds)
    if (length(.ds) == 0) {
      .ds <- NULL
    }
    .ds
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_ds_str <- function() {

    .ds <- get_selected_ds()
    pkg <- get_selected_pkg()

    if (is.null(pkg)) {
      ds_str <- .ds
    } else {
      ds_str <- str_glue("{pkg}::{.ds}")
    }
    ds_str
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_pkg_ds_data <- function() {
    get_ds_data(get_ds_str())
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_ds_info_summary <- function(ds) {

    n_variables = ncol(ds)
    n_numeric   = sum(purrr::map_int(ds, is.numeric))
    n_factor    = sum(purrr::map_int(ds, is.factor))
    n_logical   = sum(purrr::map_int(ds, is.logical))
    n_character = sum(purrr::map_int(ds, is.character))
    n_other     = n_variables - n_character - n_logical - n_factor - n_numeric
    str_glue(
      "Class: data frame \nSize: {nrow(ds)} rows {n_variables} columns \n",
      "(num: {n_numeric}, fct: {n_factor}, chr: {n_character}, ",
      "lgl: {n_logical}, other: {n_other})"
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  select_package <- function() {

    pkg <- get_selected_pkg()

    tk_set_color(f1_lab_selected_ds, "darkred")
    set_values(f1_var_selected_ds, defaults$selected_dataset)

    ds <- list_datasets_in_package(pkg)

    n_ds <- length(ds)
    if (n_ds == 0) {

      tk_set_color(f1_lab_selected_pkg, "darkred")
      set_values(f1_var_selected_pkg, defaults$selected_package)

      disable_datasets_list()

      str_no_ds <- str_glue('There are no datasets in pacage\n"{pkg}"')
      set_values(f2_var_info, str_no_ds)
      tkconfigure(f2_lab_info, foreground = "red")

      return()

    } else {
      tk_set_color(f1_lab_selected_pkg, "darkgreen")
      set_values(f1_var_selected_pkg, pkg)

      tk_normalize(f2_box_ds)
      set_values(f2_box_ds, list_datasets_in_package(pkg))

      if (n_ds == 1) {
        ending <- ""
        are_is <- "is"
      } else {
        ending <- "s"
        are_is <- "are"
      }
      str_n_ds <- str_glue('There {are_is} {n_ds} dataset{ending} in pacage\n"{pkg}"')
      set_values(f2_var_info, str_n_ds)
      tkconfigure(f2_lab_info, foreground = "darkgreen")
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  select_dataset <- function() {

    .ds <- get_selected_ds()
    if (is.null(.ds)) {
      tk_disable(f2_but_2_3)
      return()

    } else {
      set_values(f1_var_selected_ds, .ds)
      tk_set_color(f1_lab_selected_ds, "darkgreen")

      tk_normalize(f2_but_2_3)

      ds <- get_pkg_ds_data()

      if (is.data.frame(ds)) {
        tclvalue(f2_var_info) <- get_ds_info_summary(ds)
        tkconfigure(f2_lab_info, foreground = "darkgreen")

      } else {
        tclvalue(f2_var_info) <- str_c(
          "The selected dataset is not a data frame\n",
          "and will NOT be visible in the list of \n",
          "datasets of R Commander."
        )
        tkconfigure(f2_lab_info, foreground = "red")
      }
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  disable_datasets_list <- function() {

    set_values(f2_box_ds, " (no datasets)")
    tk_disable(f2_box_ds)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  reset_selection <- function() {

    set_selection(f2_box_ds,   0)
    set_selection(f2_box_pkgs, 0)

    tk_set_color(f1_lab_selected_pkg, "darkred")
    tk_set_color(f1_lab_selected_ds,  "darkred")

    set_values(f1_var_selected_pkg, defaults$selected_package)
    set_values(f1_var_selected_ds,  defaults$selected_dataset)

    set_values(f2_var_info,         "")
    tkconfigure(f2_lab_info, foreground = "darkgreen")

    tk_disable(f2_but_2_3)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ds_context_menu_load <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

    pkg <- get_selected_pkg()
    .ds <- get_selected_ds()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.null(.ds)) {
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = str_glue("Load dataset '{.ds}'"),
      compound = "left",
      image    = "::image::bs_load_pkg",
      command  = onOK
    )

    tkadd(menu_main, "command",
      label    = "Load dataset and close window",
      compound = "left",
      image    = "::image::bs_load_pkg_c",
      command  = function() {
        onOK()
        close_dialog()
      })

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = str_glue('Documentation on "{.ds}"'),
      compound = "left",
      image    = "::image::bs_help",
      command  = open_help(.ds, package = pkg)
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  open_help_selected_ds <- function() {

    pkg <- get_selected_pkg()
    .ds <- get_selected_ds()

    open_help(.ds, package = pkg)()
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ds_context_menu_help <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

    .ds <- get_selected_ds()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.null(.ds)) {
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = str_glue('Documentation on "{.ds}"'),
      compound = "left",
      image    = "::image::bs_help",
      command  = open_help_selected_ds
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  onOK <- function() {

    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    selected_pkg <- tclvalue(f1_var_selected_pkg)
    selected_ds  <- tclvalue(f1_var_selected_ds)

    if (str_detect(selected_ds, fixed("{"))) {
      selected_ds <- NULL
    }

    if (str_detect(selected_pkg, fixed("{"))) {
      selected_pkg <- NULL
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (variable_is_not_selected(selected_ds, "dataset", parent = top)) {
      return()
    }
    if (forbid_to_replace_object(selected_ds, parent = top)) {return()}
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    putDialog("window_import_from_pkg", list(
      which_packages  = get_selection(f2_pkg_opts)
    ))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

    # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    result <- justDoIt(command)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (class(result)[1] != "try-error") {
      logger(style_cmd(command))

    } else {
      logger_error(command, error_msg = result)
      show_code_evaluation_error_message()
      return()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # If calls is "data.frame", set the ds as active dataset
    if (inherits(globalenv()[[selected_ds]], "data.frame")) {
      active_dataset(selected_ds)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkfocus(CommanderWindow())

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Announce about the success to run the function `onOk()`
    TRUE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  }

  # Title --------------------------------------------------------------------
  title_text <- gettext_bs("Load Data from R Packages")
  initializeDialog(title = title_text)
  tk_title(top, title_text)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cursor_set_busy(top)
  on.exit(cursor_set_idle(top))

  # Widgets ------------------------------------------------------------------

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f1 <- tkframe(top)
  f1_lab_selected_pkg_0 <- tk_label(f1, text = "Selected package: ")
  f1_var_selected_pkg   <- tclVar(defaults$selected_package)
  f1_lab_selected_pkg   <- tk_label_red(f1, textvariable = f1_var_selected_pkg)

  f1_lab_selected_ds_0 <- tk_label(f1, text = "Selected dataset:   ")
  f1_var_selected_ds   <- tclVar(defaults$selected_dataset)
  f1_lab_selected_ds   <- tk_label_red(f1, textvariable = f1_var_selected_ds)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2 <- tkframe(top)

  f2_box_pkgs <-
    bs_listbox(
      parent = f2,
      height = 7,
      width = 25,
      values = list_packages(which = "loaded"),
      on_select = function() {
        if (tk_get_state(f2_box_pkgs) == "disabled") {return()}
        select_package()
      },
      title = gettext_bs("Package (select one)"),
      use_filter = TRUE,
      filter_label = "Filter packages"

    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_box_ds <-
    bs_listbox(
      parent = f2,
      height = 7,
      width = 28,
      values = "",
      on_select = select_dataset,
      on_double_click = ds_context_menu_load,
      on_click_3      = ds_context_menu_help,
      title = gettext_bs("Dataset (select one)"),
      value = 1,
      use_filter = TRUE,
      filter_label = "Filter datasets"
    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_but_set_2 <- tkframe(f2)

  f2_but_2_1 <- tk2button(
    parent  = f2_but_set_2,
    image   = "::image::bs_refresh",
    command =  refresh_all_installed_packages,
    tip = "Refresh the list of\n installed packages."
  )

  f2_but_2_3 <- tk2button(
    f2_but_set_2,
    image = "::image::bs_help",
    command = open_help_selected_ds,
    tip = "Open documentation \non selected dataset."
  )

  tkgrid(f2_but_2_1)
  tkgrid(f2_but_2_3, pady = c(0, 0))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2low <- tk2frame(f2)

  f2_icon <- tk2label(
    parent   = f2low,
    compound = "center"
  )

  f2_pkg_opts <- bs_combobox(
    parent = f2low,
    width  = 20,
    label  = "Which package (category)",
    label_position = "above",
    values = c(
      "Selected (all installed)",
      "Selected (loaded only)",
      "Any loaded package"
    ),
    value = initial$which_packages,

    on_select = update_packages_list
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_var_info <- tclVar("")
  f2_lab_info <- tk_label(f2, textvariable = f2_var_info, fg = "darkgreen")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Help menus ---------------------------------------------------------------
  help_menu <- function() {

    .ds <- get_selected_ds()
    pkg <- get_selected_pkg()
    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = "Function `data()`",
      command  = open_help("data", package = "utils")
    )

    tkadd(menu_main, "separator")
    tkadd(menu_main, "command",
      label    = "List datasets in loaded packages",
      command  = command_list_datasets_in_pkgs
    )

    tkadd(menu_main, "command",
      label    = "List datasets in selected package",
      command  = command_list_datasets_in_pkg(pkg),
      state    = set_menu_state(!is.null(pkg))
    )

    tkadd(menu_main, "separator")
    tkadd(menu_main, "command",
      label    = "Documentation on selected dataset",
      command  = open_help_selected_ds,
      state    = set_menu_state(!is.null(.ds))
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }
  # Finalize -----------------------------------------------------------------
  ok_cancel_help(
    on_help = help_menu,
    close_on_ok = TRUE,
    apply = "window_import_from_pkg()",
    apply_label = "Load"
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f1, sticky = "w")

  tkgrid(f1_lab_selected_pkg_0, f1_lab_selected_pkg,  pady = c(0, 0),  sticky = "w")
  tkgrid(f1_lab_selected_ds_0,  f1_lab_selected_ds,   pady = c(0, 2),  sticky = "w")


  tkgrid(f2, sticky = "nw")

  tkgrid(f2_pkg_opts$frame, f2_icon)
  tkgrid.configure(f2_icon,         sticky = "s", padx = c(2, 2), pady = c(0, 2))
  tkgrid(f2low, f2_lab_info,        sticky = "we",                pady = c(5, 0))

  tkgrid(
    f2_box_pkgs$frame,
    f2_box_ds$frame,
    f2_but_set_2,
    sticky = "nw"
  )

  tkgrid.configure(f2low,           sticky = "w",                 pady = c(0, 15))
  tkgrid.configure(f2_box_ds$frame, sticky = "",  padx = c(5, 0))
  tkgrid.configure(f2_lab_info,     sticky = "w", padx = c(5, 0), columnspan = 2)
  tkgrid.configure(f2_but_set_2,    sticky = "",  padx = c(2, 0))
  # ======================================================================~~~~
  tkgrid(buttonsFrame, sticky = "ew")
  dialogSuffix(bindReturn = FALSE)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  update_packages_list()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cursor_set_idle(top)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
