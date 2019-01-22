#' @rdname Helper-functions
#' @export
#' @keywords internal
is_biostat_mode <- function() {
    # This test is based on the title of commander window
    str <- tclvalue(tkwm.title(CommanderWindow()))
    isTRUE(stringr::str_detect(str, "(BioStat mode)"))
}

# Biostat mode ---------------------------------------------------------------
#' @rdname Helper-functions
#' @export
#' @keywords internal
set_biostat_mode <- function() {
    # Change buttons
    tkconfigure(getRcmdr("dataSetLabel"),
                # foreground = "darkred",
                image = "::image::bs_dataset",
                compound = "left",
                command = window_dataset_select)


    tkconfigure(getRcmdr("modelLabel"),
                # foreground = "darkred",
                image = "::image::bs_model",
                compound = "left",
                command = window_model_select)


    # Add tooltips
    tk2tip(getRcmdr("dataSetLabel"), "Active dataset")
    tk2tip(getRcmdr("modelLabel"),   "Active model")

    # Change icon nad button layout ------------------------------------------
    pare <- tcl_get_parent(getRcmdr("dataSetLabel"))

    # New buttons
    button_inport <- tk2button(
        pare,
        tip = "Import a dataset",
        image = "::image::bs_import",
        command = bs_mode_menu__import)

    button_export <- tk2button(
        pare,
        tip = "Export active dataset",
        image = "::image::bs_export",
        command = bs_mode_menu__export)

    button_rows <- tk2button(
        pare,
        tip = "Rows (observations)",
        image = "::image::bs_rows",
        command = bs_mode_menu__rows)

    button_variables <- tk2button(
        pare,
        tip = "Variables (columns)",
        image = "::image::bs_columns",
        command = bs_mode_menu__variables)

    button_plots <- tk2button(
        pare,
        tip = "Plots",
        image = "::image::bs_plot",
        command = bs_mode_menu__plots)

    button_analysis <- tk2button(
        pare,
        tip = "Summarize and analyze data",
        image = "::image::bs_analyze",
        command = bs_mode_menu__analyze)

    button_other <- tk2button(
        pare,
        tip     = "Session, settings and several datasets",
        image   = "::image::bs_settings",
        command = bs_mode_menu__session)

    button_refresh <- tk2button(
        pare,
        tip = "Refresh",
        image = "::image::bs_refresh",
        command = command_dataset_refresh)


    # Existing buttons
    sibl <- tcl_get_siblings(getRcmdr("dataSetLabel"))
    img  <- purrr::map_chr(sibl, ~tcltk::tclvalue(tkcget(.x, "-image")))
    txt  <- purrr::map_chr(sibl, ~tcltk::tclvalue(tkcget(.x, "-text")))

    logo         <- sibl[img == "::image::RlogoIcon"]
    button_edit  <- sibl[img == "::image::editIcon"]
    button_view  <- sibl[img == "::image::viewIcon"]
    button_data  <- sibl[img %in% c("::image::dataIcon",  "::image::bs_dataset")]
    button_model <- sibl[img %in% c("::image::modelIcon", "::image::bs_model")]
    lab_data     <- sibl[txt == gettextRcmdr("   Data set:")]
    lab_model    <- sibl[txt == gettextRcmdr("Model:")]


    # Remove old properties
    tkgrid.forget(logo, lab_data, button_data, lab_model, button_model)

    if (length(button_view) > 0) {
        tkgrid.forget(button_view)
        tkconfigure(button_view, compound = "none")
        tkconfigure(button_view, command = bs_mode_menu__print)
        # Add tooltip
        .Tcl(str_glue('tooltip::tooltip {button_view} "View and print data"'))
    }

    if (length(button_edit) > 0) {
        tkconfigure(button_edit, compound = "none")
        tkgrid.forget(button_edit)
    }

    if (length(logo) > 0) {
        tkconfigure(logo, image = "::image::bs_r_logo_g")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    putRcmdr("button_inport",    button_inport)
    putRcmdr("button_export",    button_export)
    putRcmdr("button_rows",      button_rows)
    putRcmdr("button_variables", button_variables)
    putRcmdr("button_view",      button_view)
    putRcmdr("button_analysis",  button_analysis)
    putRcmdr("button_plots",     button_plots)
    putRcmdr("button_other",     button_other)
    putRcmdr("button_refresh",   button_refresh)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # New layout
    tkgrid(logo,
           lab_data, button_data,
           button_inport,
           button_view,
           button_rows,
           button_variables,
           button_analysis,
           button_plots,
           button_other,
           button_export,
           button_refresh,
           lab_model, button_model)

    tkgrid.configure(pare, pady = c(4, 3))

    tkgrid.configure(button_data,  padx = c(2, 10))
    tkgrid.configure(lab_model,    padx = c(10, 2))
    tkgrid.configure(button_model, padx = c(0,  2))

    # Change title and main icon
    .rcmdr <- CommanderWindow()
    tkwm.title(.rcmdr, paste0(gettextRcmdr("R Commander"), " (BioStat mode)"))
    tcl("wm", "iconphoto", .rcmdr, "-default", "::image::bs_r_logo_g")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    command_dataset_refresh()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_menu_state <- function(cond) {
    if (cond) {
        "normal"
    } else {
        "disabled"
    }
}

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

    doc <- read_pptx() %>%
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



# Import menus -----------------------------------------------------------

# "From clipboard..."     , 'window_import_from_clipboard()'
# "From R package... "    , "window_import_from_pkg"
#
# "Import from text file (.txt, .csv, .dat, etc.)"   , "window_import_from_text"
# "Import from Excel file..."                        , "window_import_from_excel"
# "Import from Rds file (.Rds, .rds)..."	         , "window_import_from_rds"
# "Import from R-data file (.RData, .Rda, .rda)..."  , "window_import_rdata"
# "Import from SPSS data file..."                    , "importSPSS"
# "Import from SAS xport file..."                    , "importSAS"
# "Import from SAS b7dat file..."                    , "importSASb7dat"
# "Import from STATA data file..."                   , "importSTATA"
# "Import from Minitab data file..."                 , "importMinitab"

bs_mode_menu__import <- function() {

    top <- CommanderWindow()

    menu_i <- tk2menu(tk2menu(top), tearoff = FALSE)
    menu_f <- tk2menu(menu_i, tearoff = FALSE)

    tkadd(menu_i, "cascade",
          label    = "Import from file    ",
          compound = "left",
          image    = "::image::bs_open_file",
          menu     = menu_f)

    tkadd(menu_i, "command",
          label    = "Import from clipboard...",
          compound = "left",
          image    = "::image::bs_paste",
          command  = window_import_from_clipboard)

    tkadd(menu_i, "command",
          label    = "Import from R package...",
          compound = "left",
          image    = "::image::bs_package",
          command  = window_import_from_pkg)

    tkadd(menu_i, "command",
          label    = "Import from plot (online)...",
          compound = "left",
          image    = "::image::bs_chart",
          command  = window_online_image_digitizer)

    tkadd(menu_i, "separator")

    tkadd(menu_i, "command",
          label    = "Create a new dataset...",
          compound = "left",
          image    = "::image::bs_new_doc",
          command  = window_dataset_new_rcmdr)

    tkadd(menu_i, "command",
          label    = "Edit active dataset...",
          compound = "left",
          image    = "::image::editIcon",
          command  = window_dataset_edit_rcmdr)

    tkadd(menu_f, "command",
          label    = "from Text file (.txt, .csv, .dat, .tab)...",
          compound = "left",
          image    = "::image::bs_text",
          command  = window_import_from_text)

    # tkadd(menu_f, "separator")

    tkadd(menu_f, "command",
          label   = "from Excel file (.xls, .xlsx)...",
          compound = "left",
          image    = "::image::bs_excel",
          command = window_import_from_excel)

    # tkadd(menu_f, "separator")

    tkadd(menu_f, "command",
          label   = "from Rds file (.rds)...",
          compound = "left",
          image    = "::image::bs_r",
          command = window_import_from_rds)

    tkadd(menu_f, "command",
          label    = "from R-data file (.RData, .rda)...",
          compound = "left",
          image    = "::image::bs_r",
          command  = window_import_rdata)

    # tkadd(menu_f, "separator")
    # tkadd(menu_f, "command", label = "from SPSS data file...",    command = function() {importSPSS()})
    # tkadd(menu_f, "command", label = "from SAS xport file...",    command = function() {importSAS()})
    # tkadd(menu_f, "command", label = "from SAS b7dat file...",    command = function() {importSASb7dat()})
    # tkadd(menu_f, "command", label = "from STATA data file...",   command = function() {importSTATA()})
    # tkadd(menu_f, "command", label = "from Minitab data file...", command = function() {importMinitab()})

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


    tkadd(menu_e, "cascade",
          label    = "Export to clipboard",
          compound = "left",
          image    = "::image::bs_copy",
          menu     = menu_c)


    tkadd(menu_c, "command",
          label    = "as Tab delimited values",
          compound = "left",
          image    = "::image::bs_copy",
          command  = function() {
              .ds <- active_dataset_0()
              export_to_clipboard(.ds, sep = "\t")
          })

    tkadd(menu_c, "command",
          label    = "as Comma separated values (csv)",
          compound = "left",
          image    = "::image::bs_copy",
          command  = function() {
              .ds <- active_dataset_0()
              export_to_clipboard(.ds, sep = ",")
          })

    tkadd(menu_e, "separator")

    tkadd(menu_e, "command",
          label    = "Export to text file (.txt, .csv)...",
          compound = "left",
          image    = "::image::bs_text",
          command  = window_export_to_textfile)

    # tkadd(menu_e, "separator")

    tkadd(menu_e, "command",
          label    = "Export to Excel file (.xlsx)...",
          compound = "left",
          image    = "::image::bs_excel",
          command = window_export_to_excel)

    # tkadd(menu_e, "separator")

    tkadd(menu_e, "command",
          label    = "Export to Rds file (.rds)...",
          compound = "left",
          image    = "::image::bs_r",
          command  = window_export_to_rds)

    tkadd(menu_e, "command",
          label    = "Export to R-data file (.RData)...",
          compound = "left",
          image    = "::image::bs_r",
          command  = window_export_to_rdata)

    tkadd(menu_e, "command",
          label    = "Print as R structure",
          compound = "left",
          image    = "::image::bs_r",
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


bs_mode_menu__print <- function() {

    .ds <- active_dataset_0()
    if (is.null(.ds)) {
        command_dataset_refresh()
        active_dataset_not_persent()
        return()
    }

    top <- CommanderWindow()

    menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)
    menu_md <- tk2menu(menu_p, tearoff = FALSE)
    menu_ds <- tk2menu(menu_p, tearoff = FALSE)


    view_style <- if (.Platform$GUI == "RStudio") {
        tkadd(menu_p, "command",
              label    = "View dataset (in RStudio)",
              compound = "left",
              image    = "::image::viewIcon",
              command  = command_dataset_view)

    } else {
        tkadd(menu_p, "command",
              label    = "View dataset",
              compound = "left",
              image    = "::image::viewIcon",
              command  = window_dataset_view_rcmdr)
    }

    tkadd(menu_p, "command",
          label    = "Change class of active dataset",
          # compound = "left",
          # image    = "::image::viewIcon",
          command  = window_dataset_class)

    tkadd(menu_p, "separator")

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
    #
    tkadd(menu_p, "command",
          label   = "Print top and bottom rows",
          command = summary_head_tail)

    # tkadd(menu_p, "separator")


    tkpopup(menu_p,
            tkwinfo("pointerx", top),
            tkwinfo("pointery", top))
}


# Row menus -----------------------------------------------------------
bs_mode_menu__rows <- function() {

    top <- CommanderWindow()

    menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_n  <- tk2menu(menu_p, tearoff = FALSE)

    tkadd(menu_p, "cascade",
          label    = "Row names and row numbers",
          # compound = "left",
          # image    = "::image::bs_open_file",
          menu     = menu_n)

    tkadd(menu_n, "command",
          label    = "Print row names (or row indices)",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = command_rownames)

    tkadd(menu_n, "command",
          label    = "Check if table has row names",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = command_rows_has_rownames)

    tkadd(menu_n, "command",
          label    = "Move row names to column values...",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = window_rows_rownames_to_col)

    tkadd(menu_n, "command",
          label    = "Move column with unique values to row names...",
          # compound = "left",
          # image    = "::image::bs_locale",
          state = set_menu_state(variables_with_unique_values_P()),
          command  = window_rows_col_to_rownames)

    tkadd(menu_n, "command",
          label    = "Add column with row numbers",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = window_rows_rowid_to_col)


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_p, "command",
          label    = "Arrange: sort rows...",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = window_rows_arrange)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_rm  <- tk2menu(menu_p, tearoff = FALSE)

    tkadd(menu_p, "cascade",
          label    = "Select or remove rows",
          # compound = "left",
          # image    = "::image::bs_open_file",
          menu     = menu_rm)



    tkadd(menu_rm, "command",
          label    = "Filter: select rows that match conditions...",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = window_rows_filter0)

    tkadd(menu_rm, "command",
          label    = "Slice: select/remove rows by row index...",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = window_rows_slice)

    tkadd(menu_rm, "separator")

    tkadd(menu_rm, "command",
          label    = "Remove duplicated rows...",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = window_rows_rm_duplicated)

    tkadd(menu_rm, "command",
          label    = "Remove empty rows",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = command_rows_rm_empty_rows)

    tkadd(menu_rm, "command",
          label    = "Remove rows with missing values...",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = window_rows_rm_with_na)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_p,
            tkwinfo("pointerx", top),
            tkwinfo("pointery", top))
}

# Sumarry and analysis menus -----------------------------------------------------------
bs_mode_menu__analyze <- function() {

    top <- CommanderWindow()

    menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_n  <- tk2menu(menu_p, tearoff = FALSE)

    tkadd(menu_p, "cascade",
          label    = "Summary",
          # compound = "left",
          # image    = "::image::bs_open_file",
          menu     = menu_n)

    tkadd(menu_n, "command",
          label    = "Print number of rows and columns",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = command_dataset_dim)

    tkadd(menu_n, "command",
          label    = "Print table size and column types",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = function_not_implemented)

    tkadd(menu_n, "command",
          label    = "Glimpse: structure of dataset",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = command_glimpse)

    tkadd(menu_n, "command",
          label    = "Summarize variables...",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = function_not_implemented)

    tkadd(menu_n, "command",
          label    = "Frequency & multi-way tables...",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = window_summary_count)


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_rm  <- tk2menu(menu_p, tearoff = FALSE)

    tkadd(menu_p, "cascade",
          label    = "Analysis",
          # compound = "left",
          # image    = "::image::bs_open_file",
          menu     = menu_rm)


#
#     tkadd(menu_rm, "command",
#           label    = "Filter: select rows that match conditions...",
#           # compound = "left",
#           # image    = "::image::bs_locale",
#           command  = window_rows_filter0)
#
#     tkadd(menu_rm, "command",
#           label    = "Slice: select/remove rows by row index...",
#           # compound = "left",
#           # image    = "::image::bs_locale",
#           command  = window_rows_slice)
#
#     tkadd(menu_rm, "separator")
#
#     tkadd(menu_rm, "command",
#           label    = "Remove duplicated rows...",
#           # compound = "left",
#           # image    = "::image::bs_locale",
#           command  = window_rows_rm_duplicated)
#
#     tkadd(menu_rm, "command",
#           label    = "Remove empty rows",
#           # compound = "left",
#           # image    = "::image::bs_locale",
#           command  = command_rows_rm_empty_rows)
#
#     tkadd(menu_rm, "command",
#           label    = "Remove rows with missing values...",
#           # compound = "left",
#           # image    = "::image::bs_locale",
#           command  = window_rows_rm_with_na)

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
    menu_s  <- tk2menu(menu_p, tearoff = FALSE)

    tkadd(menu_p, "cascade",
          label    = "Variable names",
          # compound = "left",
          # image    = "::image::bs_open_file",
          menu     = menu_s)

    tkadd(menu_s, "command",
          label    = "Print variable (column) names",
          # compound = "left",
          # image    = "::image::bs_open_wd",
          command  = command_colnames)

    tkadd(menu_s, "command",
          label    = "Clean variable names (into snake case)",
          # compound = "left",
          # image    = "::image::bs_open_wd",
          command  = command_clean_names)

    tkadd(menu_s, "command",
          label    = "Rename variables...",
          # compound = "left",
          # image    = "::image::bs_open_wd",
          command  = window_variable_rename)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_p, "command",
          label    = "Select/Remove variables...",
          # compound = "left",
          # image    = "::image::bs_locale",
          command  = window_variable_select0)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_j  <- tk2menu(menu_p, tearoff = FALSE)

    tkadd(menu_p, "cascade",
          label    = "Compute, recode",
          # compound = "left",
          # image    = "::image::bs_workspace",
          menu     = menu_j)

    tkadd(menu_j, "command",
          label    = "Mutate: compute a variable...",
          # compound = "left",
          # image    = "::image::bs_folder",
          command  = window_variable_mutate0)

    tkadd(menu_j, "command",
          label    = "Recode variable values...",
          # compound = "left",
          # image    = "::image::bs_copy",
          command  = window_variable_recode0)

    tkadd(menu_j, "command",
          label    = "Convert variable types manually...",
          # compound = "left",
          # image    = "::image::bs_rename",
          command  = window_variable_convert_type)


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_wd <- tk2menu(menu_p, tearoff = FALSE)

    tkadd(menu_p, "cascade",
          label    = "Tidy, reshape",
          # compound = "left",
          # image    = "::image::bs_folder",
          menu     = menu_wd)

    tkadd(menu_wd, "command",
          label    = "Gather columns into long format dataset...",
          # compound = "left",
          # image    = "::image::bs_path_to_wd",
          command  = window_variable_gather)

    # tkadd(menu_wd, "command",
    #       label    = "Spread columns into wide format dataset...",
    #       # compound = "left",
    #       # image    = "::image::bs_open_wd",
    #       command  = function_not_implemented)
    #
    # tkadd(menu_wd, "command",
    #       label    = "Separate one value into multiple columns...",
    #       # compound = "left",
    #       # image    = "::image::bs_set_wd",
    #       command  = function_not_implemented)
    #
    # tkadd(menu_wd, "command",
    #       label    = "Unite values into one column...",
    #       # compound = "left",
    #       # image    = "::image::bs_set_wd",
    #       command  = function_not_implemented)
    #

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_chr <- tk2menu(menu_p, tearoff = FALSE)

    tkadd(menu_p, "cascade",
          label    = "Character (text) variables",
          # compound = "left",
          # image    = "::image::bs_workspace",
          menu     = menu_chr)

    tkadd(menu_chr, "command",
          label    = "Convert all text variables into factors",
          # compound = "left",
          # image    = "::image::bs_folder",
          state = set_menu_state(characterP()),
          command  = command_all_chr_to_fctr)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_fct <- tk2menu(menu_p, tearoff = FALSE)

    tkadd(menu_p, "cascade",
          label    = "Factors (categorical variables)",
          # compound = "left",
          # image    = "::image::bs_workspace",
          menu     = menu_fct)

    tkadd(menu_fct, "command",
          label    = "Drop unused levels...",
          # compound = "left",
          # image    = "::image::bs_folder",
          state = set_menu_state(factors_strict_P()),
          command  = window_factor_lvls_drop)

    tkadd(menu_fct, "command",
          label    = "Reorder levels by hand...",
          # compound = "left",
          # image    = "::image::bs_folder",
          state = set_menu_state(factors_strict_P()),
          command  = window_fct_lvls_reorder_manual)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_num <- tk2menu(menu_p, tearoff = FALSE)

    tkadd(menu_p, "cascade",
          label    = "Numeric variables",
          # compound = "left",
          # image    = "::image::bs_workspace",
          menu     = menu_num)

    tkadd(menu_num, "command",
          label    = "Log transformation...",
          # compound = "left",
          # image    = "::image::bs_folder",
          state = set_menu_state(numericP()),
          command  = window_num_transform_log)

    tkadd(menu_num, "command",
          label    = "Z transformation / Standardization...",
          # compound = "left",
          # image    = "::image::bs_folder",
          state = set_menu_state(numericP()),
          command  = window_num_transform_z)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tkpopup(menu_p,
        tkwinfo("pointerx", top),
        tkwinfo("pointery", top))
}

# Plot menus -----------------------------------------------------------
bs_mode_menu__plots <- function() {

    top <- CommanderWindow()

    menu_p <- tk2menu(tk2menu(top), tearoff = FALSE)

    tkadd(menu_p, "command",
          label    = "Open new window for plots",
          compound = "left",
          image    = "::image::bs_new_window",
          command  = open_new_plots_window)

    # tkadd(menu_p, "separator")
    #
    # tkadd(menu_p, "command",
    #       label    = "Save editable plot to PowerPoint",
    #       compound = "left",
    #       image    = "::image::bs_pptx",
    #       command  = function_not_implemented)

    # tkadd(menu_p, "separator")

    tkpopup(menu_p,
            tkwinfo("pointerx", top),
            tkwinfo("pointery", top))
}

# Session, etc. menus -----------------------------------------------------------
bs_mode_menu__session <- function() {

    top <- CommanderWindow()

    menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)

    tkadd(menu_p, "command",
          label    = "Locale...",
          compound = "left",
          image    = "::image::bs_locale",
          command  = window_locale_set)


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_s  <- tk2menu(menu_p, tearoff = FALSE)

    tkadd(menu_p, "cascade",
          label    = "Session",
          # compound = "left",
          # image    = "::image::bs_open_file",
          menu     = menu_s)

    tkadd(menu_s, "command",
          label    = "Load R packages...",
          # compound = "left",
          # image    = "::image::bs_open_wd",
          command  = window_load_packages)

    tkadd(menu_s, "separator")

    tkadd(menu_s, "command",
          label    = "Print session information: base R style",
          # compound = "left",
          # image    = "::image::bs_open_wd",
          command  = command_session_info_utils)

    tkadd(menu_s, "command",
          label    = "Print session information: devtools style",
          # compound = "left",
          # image    = "::image::bs_open_wd",
          command  = command_session_info_devtools)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_j  <- tk2menu(menu_p, tearoff = FALSE)

    tkadd(menu_p, "cascade",
          label    = "Several datasets",
          # compound = "left",
          # image    = "::image::bs_workspace",
          menu     = menu_j)

    tkadd(menu_j, "command",
          label    = "Join two datasets by matching rows...",
          # compound = "left",
          # image    = "::image::bs_folder",
          command  = window_dataset_join)

    tkadd(menu_j, "command",
          label    = "Bind rows of several datasets...",
          # compound = "left",
          # image    = "::image::bs_copy",
          command  = window_dataset_bind_rows)

    tkadd(menu_j, "command",
          label    = "Bind columns of several datasets...",
          # compound = "left",
          # image    = "::image::bs_rename",
          command  = window_dataset_bind_cols)


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
    menu_ws <- tk2menu(menu_p, tearoff = FALSE)

    tkadd(menu_p, "cascade",
          label    = "Workspace",
          compound = "left",
          image    = "::image::bs_workspace",
          menu     = menu_ws)

    tkadd(menu_ws, "command",
          label    = "List objects in R workspace",
          # compound = "left",
          # image    = "::image::bs_folder",
          command  = command_list_objects)

    tkadd(menu_ws, "command",
          label    = "Duplicate object (dataset)...",
          compound = "left",
          image    = "::image::bs_copy",
          command  = window_data_obj_copy)

    tkadd(menu_ws, "command",
          label    = "Rename object (dataset)...",
          compound = "left",
          image    = "::image::bs_rename",
          command  = window_data_obj_rename)

    tkadd(menu_ws, "command",
          label    = "Delete object (dataset)...",
          compound = "left",
          image    = "::image::bs_delete",
          command  = window_data_obj_delete)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_p,
            tkwinfo("pointerx", top),
            tkwinfo("pointery", top))
}
