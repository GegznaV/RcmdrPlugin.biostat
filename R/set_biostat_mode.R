#' @rdname Helper-functions
#' @export
#' @keywords internal
is_biostat_mode <- function() {
  # This test is based on the title of commander window
  str <- tclvalue(tkwm.title(CommanderWindow()))
  isTRUE(stringr::str_detect(str, "(BioStat mode)"))
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
use_relative_path <- function() {
  biostat_env$use_relative_path <- TRUE
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
use_absolute_path <- function() {
  biostat_env$use_relative_path <- FALSE
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
get_use_relative_path <- function() {
  isTRUE(biostat_env$use_relative_path)
}

# Biostat mode ---------------------------------------------------------------
#' @rdname Helper-functions
#' @export
#' @keywords internal
set_biostat_mode <- function() {

  if (isTRUE(is_biostat_mode())) {
    return()
  }

  # Hide buttons bar -------------------------------------------------------
  buttons_bar <- tcl_get_parent(getRcmdr("dataSetLabel"))

  tkgrid.remove(buttons_bar)
  on.exit(tkgrid(buttons_bar))
  # ========================================================================
  # Get and modify default buttons -----------------------------------------
  # Twho main buttons
  button_data <- getRcmdr("dataSetLabel")
  button_data_opts <- list()
  button_data_opts$orig_image   <- tcl_get_property(button_data, "-image")
  button_data_opts$orig_command <- tcl_get_property(button_data, "-command")

  button_model <- getRcmdr("modelLabel")
  button_model_opts <- list()
  button_model_opts$orig_image   <- tcl_get_property(button_model, "-image")
  button_model_opts$orig_command <- tcl_get_property(button_model, "-command")

  # Get existing buttons' IDs
  sibl <- tcl_get_siblings_id(getRcmdr("dataSetLabel"))

  img <- purrr::map_chr(sibl, ~tcl_get_property(., "-image"))
  txt <- purrr::map_chr(sibl, ~tcl_get_property(., "-text"))

  logo            <- sibl[str_detect(img, "(^::image::RlogoIcon$|^::image::bs_r_logo_)")]
  button_edit0    <- sibl[img == "::image::editIcon"]
  button_view0    <- sibl[img == "::image::viewIcon"]
  button_id_data  <- sibl[img %in% c("::image::dataIcon",  "::image::bs_dataset")]
  button_id_model <- sibl[img %in% c("::image::modelIcon", "::image::bs_model")]
  lab_data        <- sibl[txt == gettextRcmdr("   Data set:")]
  lab_model       <- sibl[txt == gettextRcmdr("Model:")]

  # Add tooltips
  tk2tip(button_data,  "Select active data set")
  tk2tip(button_model, "Select active model")

  if (length(button_view0) > 0) {
    # tkgrid.remove(button_view0)
    tk2tip(tcl_get_obj_by_id(button_view0), "View active data set")
  }

  if (length(button_edit0) > 0) {
    # tkgrid.remove(button_edit0)
    tk2tip(tcl_get_obj_by_id(button_edit0), "Edit active data set")
  }

  # New buttons ------------------------------------------------------------
  buttons_variant <- tk2frame(buttons_bar)
  buttons_bar_low <- tk2frame(buttons_bar)

  # button_set_1 <- tk2button(buttons_variant, width = 0.5)
  # button_set_2 <- tk2button(buttons_variant, width = 0.5)
  # button_set_3 <- tk2button(buttons_variant, width = 0.5)
  #
  # tkgrid(button_set_1, button_set_2, button_set_3)

  button_set_manage   <- tk2frame(buttons_bar_low)
  button_set_analysis <- tk2frame(buttons_bar_low)
  button_set_plots    <- tk2frame(buttons_bar_low)
  button_set_settings <- tk2frame(buttons_bar_low)
  button_set_refresh  <- tk2frame(buttons_bar_low)

  button_import <- tk2button(
    button_set_manage,
    tip     = "Import dataset",
    image   = "::image::bs_import",
    command = bs_mode_menu__import
  )

  button_export <- tk2button(
    button_set_manage,
    tip     = "Export active dataset",
    image   = "::image::bs_export",
    command = bs_mode_menu__export
  )

  button_datasets <- tk2button(
    button_set_manage,
    tip     = "Datasets and objects",
    image   = "::image::bs_objects",
    command = bs_mode_menu__datasets
  )

  button_view <- tk2button(
    button_set_analysis,
    tip     = "View, summarize and print \nactive data set",
    image   = "::image::viewIcon",
    command = bs_mode_menu__print)

  button_rows <- tk2button(
    button_set_manage,
    tip     = "Manage rows (observations)\nof active data set",
    image   = "::image::bs_rows",
    command = bs_mode_menu__rows)

  button_variables <- tk2button(
    button_set_manage,
    tip     = "Manage variables (columns)\nof active data set",
    image   = "::image::bs_columns",
    command = bs_mode_menu__variables)

  button_summary <- tk2button(
    button_set_analysis,
    tip     = "Summarize variable values \nof active data set",
    image   = "::image::bs_summary",
    command = bs_mode_menu__summary)

  button_analysis <- tk2button(
    button_set_analysis,
    tip     = "Analysis",
    image   = "::image::bs_analyze",
    command = bs_mode_menu__analyze)

  button_plots <- tk2button(
    button_set_plots,
    tip     = "Plots management",
    image   = "::image::bs_plot",
    command = bs_mode_menu__plots)

  button_other <- tk2button(
    button_set_settings,
    tip     = "Session and settings",
    image   = "::image::bs_settings",
    command = bs_mode_menu__settings)

  button_refresh <- tk2button(
    button_set_refresh,
    tip     = "Refresh data and R Commander",
    image   = "::image::bs_refresh",
    command = command_dataset_refresh)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Save objects
  putRcmdr("logo",                logo) # FIXME: error if object does not exist

  putRcmdr("button_data",         button_data)
  putRcmdr("button_model",        button_model)
  putRcmdr("button_view0",        button_view0) # FIXME: error if object does not exist
  putRcmdr("button_edit0",        button_edit0) # FIXME: error if object does not exist

  putRcmdr("buttons_variant",     buttons_variant)
  putRcmdr("buttons_bar_low",     buttons_bar_low)
  putRcmdr("button_set_manage",   button_set_manage)
  putRcmdr("button_set_plots",    button_set_plots)
  putRcmdr("button_set_analysis", button_set_analysis)
  putRcmdr("button_set_settings", button_set_settings)
  putRcmdr("button_set_refresh",  button_set_refresh)

  putRcmdr("button_import",       button_import)
  putRcmdr("button_datasets",     button_datasets)
  putRcmdr("button_export",       button_export)
  putRcmdr("button_view",         button_view)
  putRcmdr("button_summary",      button_summary)
  putRcmdr("button_rows",         button_rows)
  putRcmdr("button_variables",    button_variables)
  putRcmdr("button_analysis",     button_analysis)
  putRcmdr("button_plots",        button_plots)
  putRcmdr("button_other",        button_other)
  putRcmdr("button_refresh",      button_refresh)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # New layout -------------------------------------------------------------
  # tkgrid("x", buttons_bar_low)
  tkgrid(buttons_variant, buttons_bar_low)

  lb1 <- tk_label(buttons_variant, image = "::image::dot-green", compound = "left")
  lb2 <- tk_label(buttons_variant, image = "::image::dot-red",   compound = "left")
  lb3 <- tk_label(buttons_variant, image = "::image::dot-lblue", compound = "left")
  lb4 <- tk_label(buttons_variant, image = "::image::dot-black", compound = "left")

  tkgrid(lb1, lb2, lb3, lb4, sticky = "sew")
  tkgrid.configure(buttons_variant, sticky = "se", padx = c(10, 0))
  tkgrid.configure(
    buttons_bar_low,
    columnspan = 6,
    padx = c(10, 5),
    pady = c(1,  5),
    sticky = "w"
  )

  # Button sets
  tkgrid(
    button_set_manage,
    button_set_analysis,
    button_set_plots,
    button_set_settings,
    button_set_refresh
  )

  # Set: manage
  tkgrid(
    button_import,
    button_export,
    button_datasets,
    button_rows,
    button_variables
  )

  # Set: analyze
  tkgrid(
    button_view,
    button_summary,
    button_analysis
  )

  # Set: plots
  tkgrid(
    button_plots
  )

  # Set: settings
  tkgrid(
    button_other
  )

  # Set: refresh
  tkgrid(
    button_refresh
  )

  # tkgrid(
  #     button_import,
  #     button_export,
  #     button_datasets,
  #     button_view,
  #     button_rows,
  #     button_variables,
  #     button_summary,
  #     button_analysis,
  #     button_plots,
  #     button_other,
  #     button_refresh
  # )

  if (length(logo) > 0) {
    tkgrid.configure(logo, sticky = "w", padx = c(10, 5), pady = c(0, 10), rowspan = 2)
  }
  tkgrid.configure(lab_data,        padx = c(0, 2),  pady = c(5, 0))
  tkgrid.configure(button_id_data,  padx = c(2, 5),  pady = c(5, 0))
  if (length(button_edit0) > 0) {
    tkgrid.configure(button_edit0,  pady = c(5, 0))
  }

  if (length(button_view0) > 0) {
    tkgrid.configure(button_view0,  pady = c(5, 0))
  }
  tkgrid.configure(lab_model,       padx = c(2, 2),  pady = c(5, 0))
  tkgrid.configure(button_id_model, padx = c(0, 10), pady = c(5, 0))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Functions --------------------------------------------------------------
  tip_switch_to_biostat <- function() {
    tk2tip(tcl_get_obj_by_id(logo), "Switch to the main \nBioStat buttons")
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  toggle_buttons_bar_low <- function() {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    is_visible_buttons_bar_low <- function() {
      vals <- as.character(tkgrid.info(getRcmdr("buttons_bar_low")))
      length(vals) > 0
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    is_main_bs_logo <- function() {
      # FIXME: possible issue, if logo is not set at all
      isTRUE(tcl_get_property(logo, "-image") == "::image::bs_r_logo_g")
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    set_buttons_rcmdr_fun <- function() {
      # Change buttons
      if (length(logo) > 0) {
        tkconfigure(logo, image = "::image::RlogoIcon")
        tip_switch_to_biostat()
      }

      tkconfigure(
        button_data,
        image = button_data_opts$orig_image,
        compound = "left",
        command = button_data_opts$orig_command
      )

      tkconfigure(
        button_model,
        image = button_model_opts$orig_image,
        compound = "left",
        command = button_model_opts$orig_command
      )
      if (length(button_edit0) > 0) tkgrid(button_edit0)
      if (length(button_view0) > 0) tkgrid(button_view0)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    set_buttons_bs_fun <- function(variables) {
      if (length(logo) > 0) {
        tkconfigure(logo, image = "::image::bs_r_logo_g")
        tk2tip(tcl_get_obj_by_id(logo), "Switch to standard \nRcmdr buttons")
      }
      # Change buttons
      tkconfigure(
        button_data,
        image = "::image::bs_dataset",
        compound = "left",
        command = window_dataset_select
      )

      tkconfigure(
        button_model,
        image = "::image::bs_model",
        compound = "left",
        command = window_model_select
      )
      if (length(button_edit0) > 0) tkgrid.remove(button_edit0)
      if (length(button_view0) > 0) tkgrid.remove(button_view0)

      tkgrid(
        button_set_manage,
        button_set_analysis,
        button_set_plots,
        button_set_settings,
        button_set_refresh
      )
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # if (is_visible_buttons_bar_low()) {
    if (is_main_bs_logo()) {
      # Hide BS buttons
      tkgrid.remove(buttons_bar_low)
      tkgrid.remove(buttons_variant)
      set_buttons_rcmdr_fun()

    } else {
      # Show BS buttons
      tkgrid(buttons_bar_low)
      tkgrid(buttons_variant)
      set_buttons_bs_fun()
    }
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bind interactivity -----------------------------------------------------
  tkbind(logo, "<ButtonPress-1>", toggle_buttons_bar_low)
  tkconfigure(logo, cursor = "hand2")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkconfigure(lb1, cursor = "hand2")
  tkconfigure(lb2, cursor = "hand2")
  tkconfigure(lb3, cursor = "hand2")
  tkconfigure(lb4, cursor = "hand2")

  tip(lb1) <- "Open button set: \nData Management"
  tip(lb2) <- "Open button set: \nAnalysis"
  tip(lb3) <- "Open button set: \nPlots"
  tip(lb4) <- "Open button set: \nTools and Settings"

  tkbind(lb1, "<Enter>", function() tkconfigure(lb1, image = "::image::dot-gw-4"))
  tkbind(lb1, "<Leave>", function() tkconfigure(lb1, image = "::image::dot-green"))
  tkbind(lb1, "<Button-1>", function() {
    tkconfigure(logo, image = "::image::bs_r_logo_management")
    tip_switch_to_biostat()
    tkgrid(button_set_manage)
    tkgrid.remove(button_set_analysis, button_set_plots, button_set_settings)
  })

  tkbind(lb2, "<Enter>", function() tkconfigure(lb2, image = "::image::dot-gw-4"))
  tkbind(lb2, "<Leave>", function() tkconfigure(lb2, image = "::image::dot-red"))
  tkbind(lb2, "<Button-1>", function() {
    tkconfigure(logo, image = "::image::bs_r_logo_analysis")
    tip_switch_to_biostat()
    tkgrid(button_set_analysis)
    tkgrid.remove(button_set_manage, button_set_plots, button_set_settings)
  })

  tkbind(lb3, "<Enter>", function() tkconfigure(lb3, image = "::image::dot-gw-4"))
  tkbind(lb3, "<Leave>", function() tkconfigure(lb3, image = "::image::dot-lblue"))
  tkbind(lb3, "<Button-1>", function() {
    tkconfigure(logo, image = "::image::bs_r_logo_plots")
    tip_switch_to_biostat()
    tkgrid(button_set_plots)
    tkgrid.remove(button_set_manage, button_set_analysis, button_set_settings)
  })

  tkbind(lb4, "<Enter>", function() tkconfigure(lb4, image = "::image::dot-gw-4"))
  tkbind(lb4, "<Leave>", function() tkconfigure(lb4, image = "::image::dot-black"))
  tkbind(lb4, "<Button-1>", function() {
    tkconfigure(logo, image = "::image::bs_r_logo_settings")
    tip_switch_to_biostat()
    tkgrid(button_set_settings)
    tkgrid.remove(button_set_manage, button_set_analysis, button_set_plots)
  })
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Change the title and the main icon -------------------------------------
  .rcmdr <- CommanderWindow()
  tkwm.title(.rcmdr, paste0(Rcmdr::gettextRcmdr("R Commander"), " (BioStat mode)"))
  tcl("wm", "iconphoto", .rcmdr, "-default", "::image::bs_r_logo_g")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Activate functions (if in BioStat mode) --------------------------------
  tkgrid.remove(buttons_bar_low)
  toggle_buttons_bar_low()
  command_dataset_refresh()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This command unhides buttons bar
  tkgrid.configure(buttons_bar, pady = c(4, 3))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

}

#
#     # Change buttons
#     tkconfigure(
#       button_data,
#         # foreground = "darkred",
#         image = "::image::bs_dataset",
#         compound = "left",
#         command = window_dataset_select
#       )
#
#     tkconfigure(
#       button_model,
#         # foreground = "darkred",
#         image = "::image::bs_model",
#         compound = "left",
#         command = window_model_select
#       )
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_menu_state <- function(cond) {
  if (cond) {
    "normal"
  } else {
    "disabled"
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
activate_if_active_ds <- function() {
  set_menu_state(!is.null(active_dataset_0()))
}

# Import menus -----------------------------------------------------------
bs_mode_menu__import <- function() {
  # "From clipboard..."     , 'window_import_from_clipboard()'
  # "From R package... "    , "window_import_from_pkg"
  #
  # "Import from text file (.txt, .csv, .dat, etc.)"   , "window_import_from_text"
  # "Import from Excel file..."                        , "window_import_from_excel"
  # "Import from Rds file (.Rds, .rds)..."	           , "window_import_from_rds"
  # "Import from R-data file (.RData, .Rda, .rda)..."  , "window_import_rdata"
  # "Import from SPSS data file..."                    , "importSPSS"
  # "Import from SAS xport file..."                    , "importSAS"
  # "Import from SAS b7dat file..."                    , "importSASb7dat"
  # "Import from STATA data file..."                   , "importSTATA"
  # "Import from Minitab data file..."                 , "importMinitab"

  top <- CommanderWindow()

  menu_i <- tk2menu(tk2menu(top), tearoff = FALSE)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_f <- tk2menu(menu_i, tearoff = FALSE)

  tkadd(menu_f, "command",
    label    = "from Text file (.txt, .csv, .dat, etc.)...",
    compound = "left",
    image    = "::image::bs_text",
    command  = window_import_from_text)

  tkadd(menu_f, "command",
    label   = "from Excel file (.xls, .xlsx)...",
    compound = "left",
    image    = "::image::bs_excel",
    command = window_import_from_excel)

  tkadd(menu_f, "command",
    label   = "from Rds file (.rds)...",
    compound = "left",
    image    = "::image::bs_r_lblue",
    command = window_import_from_rds)

  tkadd(menu_f, "command",
    label    = "from R-data file (.RData, .rda)...",
    compound = "left",
    image    = "::image::bs_r_brown",
    command  = window_import_rdata)

  # tkadd(menu_f, "separator")
  # tkadd(menu_f, "command",
  #   label = "from SPSS data file...",
  #   command = function() {importSPSS()}
  # )
  # tkadd(menu_f, "command",
  #   label = "from SAS xport file...",
  #   command = function() {importSAS()}
  # )
  # tkadd(menu_f, "command",
  #   label = "from SAS b7dat file...",
  #   command = function() {importSASb7dat()}
  # )
  # tkadd(menu_f, "command",
  #   label = "from STATA data file...",
  #   command = function() {importSTATA()}
  # )
  # tkadd(menu_f, "command",
  #   label = "from Minitab data file...",
  #   command = function() {importMinitab()}
  # )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tkadd(menu_i, "cascade",
    label    = "Import from file    ",
    compound = "left",
    image    = "::image::bs_open_file",
    menu     = menu_f
  )

  tkadd(menu_i, "command",
    label    = "Import from clipboard...",
    compound = "left",
    image    = "::image::bs_paste",
    command  = window_import_from_clipboard
  )

  tkadd(menu_i, "command",
    label    = "Import from R package...",
    compound = "left",
    image    = "::image::bs_package",
    command  = window_import_from_pkg
  )

  tkadd(menu_i, "command",
    label    = "Import from plot (online)...",
    compound = "left",
    image    = "::image::bs_wpd",
    command  = window_online_image_digitizer
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkadd(menu_i, "separator")

  tkadd(menu_i, "command",
    label    = "Create a new dataset...",
    compound = "left",
    image    = "::image::bs_new_doc",
    command  = window_dataset_new_rcmdr
  )

  tkadd(menu_i, "command",
    label    = "Edit active dataset...",
    compound = "left",
    image    = "::image::editIcon",
    state    = activate_if_active_ds(),
    command  = window_dataset_edit_rcmdr
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkpopup(menu_i,
    tkwinfo("pointerx", top),
    tkwinfo("pointery", top))
}

# Export menus -----------------------------------------------------------
bs_mode_menu__export <- function() {
  .ds <- active_dataset_0()
  if (is.null(.ds)) {
    command_dataset_refresh()
    active_dataset_not_persent()
    return()
  }

  top <- CommanderWindow()

  menu_e <- tk2menu(tk2menu(top), tearoff = FALSE)
  menu_c <- tk2menu(menu_e, tearoff = FALSE)
  menu_f <- tk2menu(menu_e, tearoff = FALSE)


  tkadd(menu_e, "cascade",
    label    = "Export to file",
    compound = "left",
    image    = "::image::bs_open_file",
    menu     = menu_f)


  tkadd(menu_f, "command",
    label    = "Export to text file (.txt, .csv)...",
    compound = "left",
    image    = "::image::bs_text",
    command  = window_export_to_text)

  # tkadd(menu_f, "separator")

  tkadd(menu_f, "command",
    label    = "Export to Excel file (.xlsx)...",
    compound = "left",
    image    = "::image::bs_excel",
    command = window_export_to_excel)

  # tkadd(menu_f, "separator")

  tkadd(menu_f, "command",
    label    = "Export to Rds file (.rds)...",
    compound = "left",
    image    = "::image::bs_r_lblue",
    command  = window_export_to_rds)

  tkadd(menu_f, "command",
    label    = "Export to R-data file (.RData)...",
    compound = "left",
    image    = "::image::bs_r_brown",
    command  = window_export_to_rdata)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tkadd(menu_e, "cascade",
    label    = "Export to clipboard",
    compound = "left",
    image    = "::image::bs_copy",
    menu     = menu_c)

  tkadd(menu_c, "command",
    label    = "as Tab delimited values (tsv)",
    compound = "left",
    image    = "::image::bs_copy",
    command  = function() {
      .ds <- active_dataset_0()
      export_to_clipboard(.ds, sep = "\t")
    })
  tkadd(menu_c, "command",
    label    = "as Tab delimited values (European tsv)",
    compound = "left",
    image    = "::image::bs_copy",
    command  = function() {
      .ds <- active_dataset_0()
      export_to_clipboard(.ds, sep = "\t", dec = ",")
    })

  tkadd(menu_c, "command",
    label    = "as Comma separated values (csv)",
    compound = "left",
    image    = "::image::bs_copy",
    command  = function() {
      .ds <- active_dataset_0()
      export_to_clipboard(.ds, sep = ",")
    })

  tkadd(menu_c, "command",
    label    = "as Comma separated values (European csv)",
    compound = "left",
    image    = "::image::bs_copy",
    command  = function() {
      .ds <- active_dataset_0()
      export_to_clipboard(.ds, sep = ";", dec = ",")
    })

  # tkadd(menu_e, "separator")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkadd(menu_e, "command",
    label    = "Print as R structure",
    compound = "left",
    image    = "::image::bs_r_lgreen",
    command  = to_r_structure)

  # tkadd(menu_e, "separator")
  #
  # tkadd(menu_e, "command",
  #       label    = "Export to Word table...",
  #       compound = "left",
  #       image    = "::image::bs_word",
  #       command  = to_word)
  #
  # tkadd(menu_e, "command",
  #       label    = "Export to PowerPoint table...",
  #       compound = "left",
  #       image    = "::image::bs_pptx",
  #       command  = to_pptx)

  tkpopup(menu_e,
    tkwinfo("pointerx", top),
    tkwinfo("pointery", top))
}

# Preview, summarize df, print ------------------------------------------------
bs_mode_menu__print <- function() {

  .ds <- active_dataset_0()

  if (is.null(.ds)) {
    command_dataset_refresh()
    active_dataset_not_persent()
    return()
  }

  top <- CommanderWindow()

  menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkadd(menu_p, "command",
    label    = "Class of active dataset...",
    state    = activate_if_active_ds(),
    command  = window_dataset_class)

  tkadd(menu_p, "command",
    label    = "Number of rows and columns",
    command  = command_dataset_dim)

  tkadd(menu_p, "command",
    label    = "Variable type summay",
    command  = summary_var_types)

  tkadd(menu_p, "command",
    label    = "Screen missing data...",
    state    = activate_if_active_ds(),
    command  = window_summary_missings)

  tkadd(menu_p, "command",
    label    = "Glimpse: structure of dataset",
    compound = "left",
    image    = "::image::bs_glimpse",
    command  = command_glimpse)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkadd(menu_p, "separator")

  view_style <- if (is_rstudio()) {
    tkadd(menu_p, "command",
      label    = "View dataset (in RStudio)",
      compound = "left",
      image    = "::image::viewIcon",
      command  = command_dataset_view)

    tkadd(menu_p, "command",
      label    = "View dataset (in R Commander)",
      compound = "left",
      image    = "::image::viewIcon",
      command  = window_dataset_view_rcmdr)

  } else {
    tkadd(menu_p, "command",
      label    = "View dataset",
      compound = "left",
      image    = "::image::viewIcon",
      command  = window_dataset_view_rcmdr)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkadd(menu_p, "separator")

  menu_md <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Print as Markdown table ",
    compound = "left",
    image    = "::image::bs_md",
    menu     = menu_md)

  tkadd(menu_md, "command",
    label = "Engine: kable",
    command = window_dataset_print_as_kable)

  tkadd(menu_md, "command",
    label = "Engine: pander",
    command = window_dataset_print_as_md)

  # tkadd(menu_p, "separator")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_ds <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Print as dataset",
    # compound = "left",
    # image    = "::image::bs_open_file",
    menu     = menu_ds)

  tkadd(menu_ds, "command",
    label   = "as 'data.frame'",
    command = command_dataset_print_as_df)

  tkadd(menu_ds, "command",
    label   = "as 'data.table'",
    command = command_dataset_print_as_dt)

  tkadd(menu_ds, "command",
    label   = "as 'tibble'",
    command = command_dataset_print_as_tibble)

  # tkadd(menu_ds, "separator")

  tkadd(menu_p, "command",
    label   = "Print top and bottom rows",
    command = summary_head_tail)

  # tkadd(menu_p, "separator")


  tkpopup(menu_p,
    tkwinfo("pointerx", top),
    tkwinfo("pointery", top))
}

# Summarize variables menus ---------------------------------------------------
bs_mode_menu__summary  <- function() {

  top <- CommanderWindow()
  menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

  # tkadd(menu_p, "command",
  #       label    = "Summarize variables...",
  #       # compound = "left",
  #       # image    = "::image::bs_r",
  #       command  = window_summary_variables)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tkadd(menu_p, "command",
    label    = "Summarize all variables (dfSummary)",
    command  = window_summary_dfSummary)

  tkadd(menu_p, "command",
    label    = "Summarize all variables (Desc)",
    command  = window_summary_desc_all)

  tkadd(menu_p, "separator")

  tkadd(menu_p, "command",
    # label    = "Summarize selected variables (Desc)...",
    label    = "Summarize single or pair of variables (Desc)...",
    command  = window_summary_desc)

  tkadd(menu_p, "separator")

  tkadd(menu_p, "command",
    label    = "Summarize numeric variables",
    state    = set_menu_state(numericP()),
    command  = window_summary_descr
  )

  # tkadd(menu_p, "command",
  #     label    = "Frequency table for numeric variable...",
  #     # compound = "left",
  #     # image    = "::image::bs_r",
  #     state    = set_menu_state(numericP()),
  #     command  = window_summary_Freq
  # )

  tkadd(menu_p, "separator")

  tkadd(menu_p, "command",
    label    = "Frequency & multi-way tables...",
    command  = window_summary_count)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkpopup(menu_p,
    tkwinfo("pointerx", top),
    tkwinfo("pointery", top))
}


# Row menus -----------------------------------------------------------
bs_mode_menu__rows <- function() {

  top <- CommanderWindow()

  menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkadd(menu_p, "command",
    label    = "Arrange: sort rows...",
    compound = "left",
    image    = "::image::bs_rows_sort",
    command  = window_rows_arrange)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_n  <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Row names and row numbers",
    compound = "left",
    image    = "::image::bs_rows_names",
    menu     = menu_n)

  tkadd(menu_n, "command",
    label    = "Check if table has row names",
    command  = command_rows_has_rownames)

  tkadd(menu_n, "command",
    label    = "Print row names (or row indices)",
    command  = command_rownames)

  tkadd(menu_n, "separator")

  tkadd(menu_n, "command",
    label    = "Move row names to column...",
    command  = window_rows_rownames_to_col)

  tkadd(menu_n, "command",
    label    = "Move column (with unique values) to row names...",
    state = set_menu_state(variables_with_unique_values_P()),
    command  = window_rows_col_to_rownames)

  tkadd(menu_n, "command",
    label    = "Create column with row numbers...",
    command  = window_rows_rowid_to_col)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_rm  <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Select or remove rows",
    compound = "left",
    image    = "::image::bs_rows_select",
    menu     = menu_rm)

  tkadd(menu_rm, "command",
    label    = "Filter: select rows that match conditions...",
    compound = "left",
    image    = "::image::bs_rows_filter",
    command  = window_rows_filter0)

  tkadd(menu_rm, "command",
    label    = "Slice: select/remove rows by row index...",
    compound = "left",
    image    = "::image::bs_rows_slice",
    command  = window_rows_slice)

  tkadd(menu_rm, "separator")

  tkadd(menu_rm, "command",
    label    = "Remove duplicated rows...",
    command  = window_rows_rm_duplicated)

  tkadd(menu_rm, "command",
    label    = "Remove empty rows",
    command  = command_rows_rm_empty_rows)

  tkadd(menu_rm, "command",
    label    = "Remove rows with missing values...",
    command  = window_rows_rm_with_na)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkpopup(menu_p,
    tkwinfo("pointerx", top),
    tkwinfo("pointery", top))
}
# Variable menus -----------------------------------------------------------
bs_mode_menu__variables <- function() {

  top <- CommanderWindow()

  menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_var_names  <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Variable names",
    compound = "left",
    image    = "::image::bs_cols_names",
    menu     = menu_var_names)

  tkadd(menu_var_names, "command",
    label    = "Print variable (column) names",
    command  = command_colnames)

  tkadd(menu_var_names, "command",
    label    = "Clean variable names (into snake case)",
    command  = command_clean_names)

  tkadd(menu_var_names, "command",
    label    = "Rename variables...",
    command  = window_variable_rename)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkadd(menu_p, "command",
    label    = "Select/Remove variables...",
    compound = "left",
    image    = "::image::bs_cols_select",
    command  = window_variable_select0)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_j  <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Compute, recode, convert",
    compound = "left",
    image    = "::image::bs_cols_compute",
    menu     = menu_j)

  tkadd(menu_j, "command",
    label    = "Mutate: compute a variable...",
    compound = "left",
    image    = "::image::bs_cols_mutate",
    command  = window_variable_mutate0)

  tkadd(menu_j, "command",
    label    = "Recode variable values...",
    compound = "left",
    image    = "::image::bs_cols_recode",
    command  = window_variable_recode0)

  tkadd(menu_j, "command",
    label    = "Convert variable types manually...",
    compound = "left",
    image    = "::image::bs_cols_convert",
    command  = window_variable_convert_type)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_wd <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Tidy, reshape",
    compound = "left",
    image    = "::image::bs_data_reshape",
    menu     = menu_wd)

  tkadd(menu_wd, "command",
    label    = "Gather columns into long format dataset...",
    command  = window_variable_gather)

  # tkadd(menu_wd, "command",
  #       label    = "Spread columns into wide format dataset...",
  #       command  = function_not_implemented)
  #
  # tkadd(menu_wd, "command",
  #       label    = "Separate one value into multiple columns...",
  #       command  = function_not_implemented)
  #
  # tkadd(menu_wd, "command",
  #       label    = "Unite values into one column...",
  #       command  = function_not_implemented)
  #

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tkadd(menu_p, "separator")

  menu_chr <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Character (text) variables",
    compound = "left",
    image    = "::image::bs_data_chr",
    menu     = menu_chr)

  tkadd(menu_chr, "command",
    label    = "Convert all text variables into factors",
    state = set_menu_state(characterP()),
    command  = command_all_chr_to_fctr)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_fct <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Factors (categorical variables)",
    compound = "left",
    image    = "::image::bs_data_fct",
    menu     = menu_fct)

  tkadd(menu_fct, "command",
    label    = "Drop unused levels...",
    state    = set_menu_state(factors_strict_P()),
    command  = window_factor_lvls_drop)

  tkadd(menu_fct, "command",
    label    = "Reorder levels by hand...",
    state = set_menu_state(factors_strict_P()),
    command  = window_fct_lvls_reorder_manual)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_num <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Numeric variables",
    compound = "left",
    image    = "::image::bs_data_num",
    menu     = menu_num)

  tkadd(menu_num, "command",
    label    = "Logarithmic transformation...",
    state = set_menu_state(numericP()),
    command  = window_num_transform_log)

  tkadd(menu_num, "command",
    label    = "Z transformation / Standardization...",
    state = set_menu_state(numericP()),
    command  = window_num_transform_z)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkpopup(menu_p,
    tkwinfo("pointerx", top),
    tkwinfo("pointery", top))
}

# Analysis menus -------------------------------------------------------------
bs_mode_menu__analyze <- function() {

  top <- CommanderWindow()
  menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

  # tkadd(menu_p, "command",
  #       label      = "Association between categorical variables...",
  #       # compound = "left",
  #       # image    = "::image::bs_open_file",
  #       state      = set_menu_state(factorsP(2)),
  #       command    = window_summary_count)

  # tkadd(menu_p, "command",
  #       label      = "Normality test (univariate)...",
  #       # compound = "left",
  #       # image    = "::image::bs_open_file",
  #       state      = set_menu_state(numericP()),
  #       command    = window_test_normality)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # # ~ Association / Correlation --------------------------------------------
  # menu_a <- tk2menu(menu_p, tearoff = FALSE)

  # tkadd(menu_p, "cascade",
  #       label      = "Relationship",
  #       # label    = "Association & Correlation",
  #       # compound = "left",
  #       # image    = "::image::bs_open_file",
  #       menu     = menu_a)
  #
  # tkadd(menu_p, "command",
  #     label      = "Association between categorical variables...",
  #     # compound = "left",
  #     # image    = "::image::bs_open_file",
  #     # state      = set_menu_state(factorsP(2)),
  #     command    = window_summary_count
  # )

  # tkadd(menu_a, "command",
  #       label      = "Correlation... [Rcmdr]",
  #       # compound = "left",
  #       # image    = "::image::bs_open_file",
  #       state      = set_menu_state(numericP(2)),
  #       command    = Rcmdr:::correlationTest)
  #
  # tkadd(menu_a, "command",
  #       label      = "Correlation matrix... [Rcmdr]",
  #       # compound = "left",
  #       # image    = "::image::bs_open_file",
  #       state      = set_menu_state(numericP(2)),
  #       command    = Rcmdr:::correlationMatrix)
  #
  # tkadd(menu_a, "command",
  #       label      = "Pearson's linear correlation... [EZR]",
  #       # compound = "left",
  #       # image    = "::image::bs_open_file",
  #       state      = set_menu_state(numericP(2)),
  #       command    = RcmdrPlugin.EZR::StatMedCorrelation)
  #
  # tkadd(menu_a, "command",
  #       label      = "Spearman's / Kendall's rank correlation... [EZR]",
  #       # compound = "left",
  #       # image    = "::image::bs_open_file",
  #       state      = set_menu_state(numericP(2)),
  #       command    = RcmdrPlugin.EZR::StatMedSpearman)
  #
  # tkadd(menu_a, "separator")

  # tkadd(menu_a, "command",
  #       label      = "Association between categorical variables...",
  #       # compound = "left",
  #       # image    = "::image::bs_open_file",
  #       # state      = set_menu_state(factorsP(2)),
  #       command    = window_summary_count)
  #
  # # ~ Tests ----------------------------------------------------------------
  # menu_t <- tk2menu(menu_p, tearoff = FALSE)
  #
  # tkadd(menu_p, "cascade",
  #       label    = "Tests",
  #       # compound = "left",
  #       # image    = "::image::bs_open_file",
  #       menu     = menu_t)

  tkadd(menu_p, "command",
    label      = "Normality test (univariate)...",
    state      = set_menu_state(numericP()),
    compound   = "left",
    image      = "::image::bs_normality",
    command    = window_test_normality)

  #
  #     # ~~ Central tendency ----------------------------------------------------
  #
  #     menu_t_c <- tk2menu(menu_t, tearoff = FALSE)
  #
  #     tkadd(menu_t, "cascade",
  #           label    = "Central tendency* tests",
  #           # compound = "left",
  #           # image    = "::image::bs_open_file",
  #           menu     = menu_t_c)
  #
  #
  #     # ~~ Proportion tests ----------------------------------------------------
  #
  #     menu_t_p <- tk2menu(menu_t, tearoff = FALSE)
  #
  #     tkadd(menu_t, "cascade",
  #           label    = "Proportion tests",
  #           # compound = "left",
  #           # image    = "::image::bs_open_file",
  #           menu     = menu_t_p)
  #
  #     tkadd(menu_t_p, "command",
  #           label      = ">>>",
  #           # compound = "left",
  #           # image    = "::image::bs_open_file",
  #           state      = set_menu_state(twoLevelFactorsP()),
  #           command    = function_not_implemented)
  #
  #     tkadd(menu_t_p, "command",
  #           label      = ">>>",
  #           # compound = "left",
  #           # image    = "::image::bs_open_file",
  #           state      = set_menu_state(twoLevelFactorsP()),
  #           command    = function_not_implemented)


  # # ~~ Variability tests --------------------------------------------------
  #
  # menu_t_v <- tk2menu(menu_t, tearoff = FALSE)
  #
  # tkadd(menu_t, "cascade",
  #       label    = "Variability tests",
  #       # compound = "left",
  #       # image    = "::image::bs_open_file",
  #       menu     = menu_t_v)
  #
  # tkadd(menu_t_v, "command",
  #       label      = "Two-variances F-test... [EZR]",
  #       # compound = "left",
  #       # image    = "::image::bs_open_file",
  #       state      = set_menu_state(numericP() && twoLevelFactorsP()),
  #       command    = RcmdrPlugin.EZR::StatMedFTest)
  #
  # tkadd(menu_t_v, "command",
  #       label      = "Bartlett's test... [EZR]",
  #       # compound = "left",
  #       # image    = "::image::bs_open_file",
  #       state      = set_menu_state(numericP() && factorsP()),
  #       command    = RcmdrPlugin.EZR::StatMedBartlett)
  #
  # tkadd(menu_t_v, "command",
  #       label      = "Levene's / Brown-Forsythe's test... [Rcmdr]",
  #       # compound = "left",
  #       # image    = "::image::bs_open_file",
  #       state      = set_menu_state(numericP() && factorsP()),
  #       command    = Rcmdr:::LeveneTest)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkpopup(menu_p,
    tkwinfo("pointerx", top),
    tkwinfo("pointery", top))
}

# Plots menus ----------------------------------------------------------------
bs_mode_menu__plots <- function() {

  top <- CommanderWindow()

  menu_p <- tk2menu(tk2menu(top), tearoff = FALSE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_a <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Default place to draw plots",
    menu     = menu_a
  )

  tkadd(menu_a, "command",
    label    = "Separate window for plots",
    compound = "left",
    image    =
      if (which_graphical_device() == "separate_window") {
        "::image::bs_tick"
      } else {
        ""
      },
    command    = set_plots_to_separate_window
  )

  if (is_rstudio()) {
    tkadd(menu_a, "command",
      label    = "RStudio 'Plots' tab",
      compound = "left",
      image    =
        if (which_graphical_device() == "RStudioGD") {
          "::image::bs_tick"
        } else {
          ""
        },
      command  = set_plots_to_rstudio_window)
  }

  tkadd(menu_p, "command",
    label    = "Open new window for plots",
    compound = "left",
    image    = "::image::bs_new_window",
    command  = open_new_plots_window
  )

  tkadd(menu_p, "command",
    label    = "Close all plots",
    compound = "left",
    image    = "::image::bs_plot_close",
    command  = close_all_plots
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (packageAvailable('plotly')) {

    tkadd(menu_p, "separator")

    tkadd(menu_p, "command",
      label    = "Convert ggplot into interactive plot...",
      state    = set_menu_state(gg_objects_exist() || gg_lastplot_exists()),
      compound = "left",
      image    = "::image::bs_plotly",
      command  = window_plots_ggplotly)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # tkadd(menu_p, "separator")
  #
  # tkadd(menu_p, "command",
  #     label    = "Import data from plot (online)...",
  #     compound = "left",
  #     image    = "::image::bs_chart",
  #     command  = window_online_image_digitizer)

  if (packageAvailable('officer') && packageAvailable('rvg')) {
    tkadd(menu_p, "separator")

    tkadd(menu_p, "command",
      label    = "Save editable plot to PowerPoint...",
      compound = "left",
      image    = "::image::bs_pptx",
      command  = window_export_fig_to_pptx)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkpopup(menu_p,
    tkwinfo("pointerx", top),
    tkwinfo("pointery", top))
}

# Settings, etc. menus -------------------------------------------------------
bs_mode_menu__settings <- function() {

  top <- CommanderWindow()

  menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # tkadd(menu_p, "command",
  #       label    = "Always on top",
  #       compound = "left",
  #       image    =
  #           if (isTRUE(rcmdr_get_always_on_top())) {
  #               "::image::bs_tick"
  #           } else {
  #               "::image::bs_delete"
  #           },
  #       command    = toggle_always_on_top)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_lng  <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Language",
    compound = "left",
    image    = "::image::bs_locale",
    menu     = menu_lng)

  tkadd(menu_lng, "command",
    label    = "Locale...",
    compound = "left",
    image    = "::image::bs_locale",
    command  = window_locale_set)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_opts  <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Options",
    compound = "left",
    image    = "::image::bs_r_yellow",
    menu     = menu_opts)

  to_console <- is_console_output()

  tkadd(menu_opts, "command",
    label    = "Output to R console (1 window mode)",
    compound = "left",
    image    = if (to_console) {"::image::bs_tick"} else {""},
    command  = if (to_console) {do_nothing} else {command_rcmdr_use_1_window})

  tkadd(menu_opts, "command",
    label    = "Output to R Commander (3 windows mode)",
    compound = "left",
    image    = if (!to_console) {"::image::bs_tick"} else {""},
    command  = if (!to_console) {do_nothing} else {command_rcmdr_use_3_windows})

  tkadd(menu_opts, "separator")

  sort_names <- getRcmdr("sort.names")

  tkadd(menu_opts, "command",
    label    = "Keep original order (column names in widgets)",
    compound = "left",
    image    = if (!sort_names) {"::image::bs_tick"} else {""},
    command  =
      if (!sort_names) {
        do_nothing
      } else {
        function() {
          putRcmdr("sort.names", FALSE)
          # options(Rcmdr = list(sort.names = FALSE))
          command_dataset_refresh()
        }})

  tkadd(menu_opts, "command",
    label    = "Sort alphabetically (column names in widgets)",
    compound = "left",
    image    = if (sort_names) {"::image::bs_tick"} else {""},
    command  =
      if (sort_names) {
        do_nothing
      } else {
        function() {
          putRcmdr("sort.names", TRUE)
          # options(Rcmdr = list(sort.names = TRUE))
          command_dataset_refresh()
        }})

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_session  <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Session",
    compound = "left",
    image    = "::image::bs_r",
    menu     = menu_session)

  tkadd(menu_session, "command",
    label    = "Load R packages...",
    compound = "left",
    image    = "::image::bs_package",
    command  = window_load_packages)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkadd(menu_session, "separator")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tkadd(menu_session, "command",
    label    = "Print session information: base R style",
    # compound = "left",
    # image    = "::image::bs_open_wd",
    command  = command_session_info_utils)

  tkadd(menu_session, "command",
    label    = "Print session information: devtools style",
    # compound = "left",
    # image    = "::image::bs_open_wd",
    command  = command_session_info_devtools)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkadd(menu_session, "separator")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tkadd(menu_session, "command",
    label    = "Restart R Commander",
    compound = "left",
    image    = "::image::bs_restart",
    command  = command_rcmdr_restart)

  if (is_rstudio()) {
    tkadd(menu_session, "command",
      label    = "Restart R session in RStudio",
      compound = "left",
      image    = "::image::bs_restart_r",
      command  = command_restart_rs_session)
  }

  tkadd(menu_session, "command",
    label    = "Close R Commander",
    compound = "left",
    image    = "::image::bs_close_rcmdr",
    command  = Rcmdr::closeCommander)

  tkadd(menu_session, "command",
    label    = "Close R Commander & R",
    compound = "left",
    image    = "::image::bs_close_r",
    command  = command_rcmdr_close_r)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  menu_wd <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "Working directory (WD)",
    compound = "left",
    image    = "::image::bs_folder",
    menu     = menu_wd)

  tkadd(menu_wd, "command",
    label    = "Print path to WD",
    compound = "left",
    image    = "::image::bs_path_to_wd",
    command  = command_getwd)

  tkadd(menu_wd, "command",
    label    = "Open WD",
    compound = "left",
    image    = "::image::bs_open_wd",
    command  = command_openwd)

  tkadd(menu_wd, "command",
    label    = "Change WD",
    compound = "left",
    image    = "::image::bs_set_wd",
    command  = command_setwd)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkadd(menu_p, "separator")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  menu_ab <- tk2menu(menu_p, tearoff = FALSE)

  tkadd(menu_p, "cascade",
    label    = "About",
    compound = "left",
    image    = "::image::bs_about",
    menu     = menu_ab)

  tkadd(menu_ab, "command",
    label    = "About BioStat'19...",
    compound = "left",
    image    = "::image::bs_about",
    command  = window_about_biostat_version)

  tkadd(menu_ab, "command",
    label    = "Go to Homepage",
    compound = "left",
    image    = "::image::bs_home",
    command  = window_online_homepage)

  tkadd(menu_ab, "command",
    label    = "Feedback & bug reports",
    compound = "left",
    image    = "::image::bs_bug",
    command  = window_online_bug_report)

  tkadd(menu_ab, "separator")

  tkadd(menu_ab, "command",
    label    = "Check recommended packages for BioStat",
    compound = "left",
    image    = "::image::bs_chk_pkgs",
    command  = command_chk_packages_biostat)

  tkadd(menu_ab, "separator")

  tkadd(menu_ab, "command",
    label    = "Check for updates for [BS-2019] course",
    compound = "left",
    image    = "::image::bs_chk_pkgs",
    command  = command_chk_packages_bs19)

  tkadd(menu_ab, "command",
    label    = "Check for updates for [R-2019] course",
    compound = "left",
    image    = "::image::bs_chk_pkgs",
    command  = command_chk_packages_r19)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkpopup(menu_p,
    tkwinfo("pointerx", top),
    tkwinfo("pointery", top))
}

# Datasets and objects menus --------------------------------------------------
bs_mode_menu__datasets <- function() {

  top <- CommanderWindow()

  menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkadd(menu_p, "command",
    label    = "Join two datasets by matching row ID...",
    compound = "left",
    image    = "::image::bs_join",
    command  = window_dataset_join)

  tkadd(menu_p, "command",
    label    = "Bind rows of several datasets...",
    compound = "left",
    image    = "::image::bs_bind_rows",
    command  = window_dataset_bind_rows)

  tkadd(menu_p, "command",
    label    = "Bind columns of several datasets...",
    compound = "left",
    image    = "::image::bs_bind_cols",
    command  = window_dataset_bind_cols)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkadd(menu_p, "separator")

  tkadd(menu_p, "command",
    label    = "List loaded objects (and datasets)",
    compound = "left",
    image    = "::image::bs_workspace",
    command  = command_list_objects)

  tkadd(menu_p, "command",
    label    = "Manage objects (and datasets)...",
    compound = "left",
    image    = "::image::bs_objects",
    command  = window_data_obj_manage)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tkpopup(menu_p,
    tkwinfo("pointerx", top),
    tkwinfo("pointery", top))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

