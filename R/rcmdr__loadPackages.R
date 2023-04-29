# TODO:
#  1. [ ] enable package unloading
#         to_unload <- 1:4
#         logger(str_c(str_glue('detach("package:{to_unload}", unload = TRUE)'),
#                      collapse = "\n"))
#  2. [ ] add button "Refresh" and context menu option "Refresh".
#

window_load_packages <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  refresh_window <- function() {
    loaded_packages <- .packages()
    availablePackages <-
      sort(setdiff(.packages(all.available = TRUE), loaded_packages))

    set_values(f2_box_not_loaded, availablePackages)
    set_values(f2_box_loaded,     loaded_packages)
    set_values(
      f1_lab_var,
      str_glue("Number of loaded packages: {length(loaded_packages)}")
    )
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  onOK <- function() {
    packages <- get_selection(f2_box_not_loaded)

    if (variable_is_not_selected(packages, "package", parent = top)) {
      return()
    }

    for (package in packages) {
      Library(package)
    }
    Message(
      paste(gettextRcmdr("Packages loaded:"), paste(packages, collapse = ", ")),
      type = "note"
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Announce about the success to run the function `onOk()`
    TRUE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  }


  loaded_packages <- .packages()
  availablePackages <- sort(setdiff(.packages(all.available = TRUE), loaded_packages))

  # if (length(availablePackages) == 0) {
  #   errorCondition(message = gettextRcmdr("No packages available to load."))
  #   return()
  # }
  win_title <- gettextRcmdr("Load Packages")
  initializeDialog(title = win_title)
  tk_title(top, win_title)

  f1_lab_var <- tclVar(str_glue("Number of loaded packages: {length(loaded_packages)}"))
  f1_lab <- tk_label(top, textvariable = f1_lab_var)

  f2 <- tk2frame(top)
  f2_box_not_loaded <-
    bs_listbox(
      parent       = f2,
      values       = availablePackages,
      title        = gettextRcmdr("Available packages (to load)"),
      use_filter   = TRUE,
      height       = 10,
      width        = c(30, Inf),
      filter_label = "Filter available packages",
      selectmode   = "multiple"
    )

  f2_box_loaded <-
    bs_listbox(
      parent       = f2,
      values       = loaded_packages,
      title        = gettextRcmdr("Loaded packages (to check)"),
      use_filter   = TRUE,
      height       = 10,
      width        = c(30, Inf),
      filter_label = "Filter loaded packages",
      title_color  = "darkred", # "#404040",
      enabled      = FALSE
    )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Help menus -------------------------------------------------------------
  context_menu <- function() {

    sel_pkgs <- get_selection(f2_box_not_loaded)
    n_pkgs <- length(sel_pkgs)

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (n_pkgs > 0) {
      if (n_pkgs == 1) {
        txt <- str_glue('Load package "{sel_pkgs}"')
      } else {
        txt <- str_glue("Load {length(sel_pkgs)} selected packages")
      }

      tkadd(menu_main, "command",
        label    = txt,
        compound = "left",
        image    = "::image::bs_load_pkg",
        command  = function() {
          onOK()
          refresh_window()
        }
      )

      tkadd(menu_main, "command",
        label    = "Clear selection",
        compound = "left",
        image    = "::image::bs_delete",
        command  =  function() {
          set_selection(f2_box_not_loaded, 0)
        }
      )
    } else {
      tkadd(menu_main, "command",
        label    = "Nothing is selected",
        state    = "disabled"
      )
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  help_menu <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "command",
      label    = "Function `library()`",
      command  = open_help("library", package = "base"))

    tkadd(menu_main, "command",
      label    = "Function `.libPaths()`",
      command  = open_help(".libPaths", package = "base"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ok_cancel_help(
    on_help = help_menu,
    close_on_ok = TRUE,
    apply = "window_load_packages()",
    apply_label = "Load",
    after_apply_success_fun = refresh_window
  )

  tkgrid(f1_lab, sticky = "nw")
  tkgrid(f2, sticky = "nw", pady = c(10, 0))
  tkgrid(f2_box_loaded$frame, f2_box_not_loaded$frame, sticky = "nw")
  tkgrid.configure(f2_box_not_loaded$frame, padx = c(10, 0))

  tkgrid(buttonsFrame, sticky = "we", columnspan = 2)
  dialogSuffix()

  tkbind(f2_box_not_loaded$listbox, "<Double-ButtonPress-1>", context_menu)
  tkbind(f2_box_not_loaded$listbox, "<ButtonPress-3>",        context_menu)
}
