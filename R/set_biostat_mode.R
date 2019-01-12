#' @rdname Helper-functions
#' @export
#' @keywords internal
is_biostat_mode <- function() {
    str <- tclvalue(tkwm.title(CommanderWindow()))
    isTRUE(stringr::str_detect(str, "(BioStat mode)"))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    b_dir <- tk2button(
        pare,
        tip = "Open working directory.\nRight-click for more options.",
        image = "::image::bs_open_dir",
        command = command_openwd)

    b_in <- tk2button(
        pare,
        tip = "Import dataset",
        image = "::image::bs_import",
        command = menu_import)

    b_out <- tk2button(
        pare,
        tip = "Export dataset",
        image = "::image::bs_export",
        command = menu_export)

    # b_print <- tk2button(
    #     pare,
    #     tip = "Print dataset to console/output window",
    #     image = "::image::bs_to_console",
    #     command = menu_print)

    b_refresh <- tk2button(
        pare,
        tip = "Refresh data",
        image = "::image::bs_refresh",
        command = command_dataset_refresh)

    b_summary <- tk2button(
        pare,
        tip = "Summary",
        image = "::image::bs_summary",
        command = function_not_implemented)

    b_plots <- tk2button(
        pare,
        tip = "Plots",
        image = "::image::bs_plot",
        command = menu_plots)

    b_analysis <- tk2button(
        pare,
        tip = "Analysis",
        image = "::image::bs_analyse",
        command = function_not_implemented)

    b_settings <- tk2button(
        pare,
        tip     = "Settings",
        image   = "::image::bs_settings",
        command = function_not_implemented)


    sibl <- tcl_get_siblings(getRcmdr("dataSetLabel"))
    img  <- map_chr(sibl, ~tcltk::tclvalue(tkcget(.x, "-image")))
    txt  <- map_chr(sibl, ~tcltk::tclvalue(tkcget(.x, "-text")))

    logo      <- sibl[img == "::image::RlogoIcon"]
    but_edit  <- sibl[img == "::image::editIcon"]
    but_view  <- sibl[img == "::image::viewIcon"]
    but_data  <- sibl[img %in% c("::image::dataIcon",  "::image::bs_dataset")]
    but_model <- sibl[img %in% c("::image::modelIcon", "::image::bs_model")]
    lab_data  <- sibl[txt == gettextRcmdr("   Data set:")]
    lab_model <- sibl[txt == gettextRcmdr("Model:")]


    # Remove old properties
    tkgrid.forget(logo, lab_data, but_data, lab_model, but_model)
    tkconfigure(but_edit, compound = "none")
    tkconfigure(but_view, compound = "none")

    if (length(logo) > 0) {
        tkconfigure(logo, image = "::image::bs_r_logo_g")
    }

    if (length(but_view) > 0) {
        tkgrid.forget(but_view)

        tkconfigure(but_view, command = menu_print)
        # Add tooltip
        .Tcl(str_glue('tooltip::tooltip {but_view} "Preview dataset"'))
    }

    if (length(but_edit) > 0) {
        tkgrid.forget(but_edit)
        # Add tooltip
        .Tcl(str_glue('tooltip::tooltip {but_edit} "Edit dataset"'))

        # New layout
        tkgrid(logo, lab_data, but_data, b_dir, b_in, b_out, but_view,
               but_edit, # < ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               b_summary, b_plots, b_analysis, b_settings, b_refresh,
               lab_model, but_model)

    } else {
        # New layout without "Edit" button
        tkgrid(logo, lab_data, but_data, b_dir, b_in, b_out, but_view,
               # but_edit, # < ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               b_summary, b_plots, b_analysis, b_settings, b_refresh,
               lab_model, but_model)
    }

    # tkcget(but_edit, "-command")

    tkgrid.configure(pare, pady = c(4, 3))

    tkgrid.configure(but_data,  padx = c(2, 10))
    tkgrid.configure(lab_model, padx = c(10, 2))
    tkgrid.configure(but_model, padx = c(0,  2))


    # Change title and main icon
    .rcmdr <- CommanderWindow()
    tkwm.title(.rcmdr, paste0(gettextRcmdr("R Commander"), " (BioStat mode)"))
    tcl("wm", "iconphoto", .rcmdr, "-default", "::image::bs_r_logo_g")

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Export funs ----------------------------------------------------------------

to_r_structure <- function() {
    # .ds <- get_selection(var_ds_box)
    .ds <- ActiveDataSet()

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
    library(tidyverse)
    library(officer)

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



    library(tidyverse)
    library(officer)
    doc <- read_docx("toc_and_captions.docx") %>%

        # body_add_par(value = "dataset mtcars", style = "heading 1") %>%
        # body_add_break() %>%

        body_add_par(value = "data mtcars", style = "table title") %>%
        body_add_table(value = swiss, style = "table_template" ) %>%
        body_end_section_portrait() %>%
        print(doc, target = "toc_and_captions.docx")

    fs::file_show("toc_and_captions.docx")
}



# Import menus -----------------------------------------------------------

# "From clipboard..."     , 'window_import_from_clipboard()'
# "From R package... "    , "window_import_from_pkg"
#
# "Import from text file (.txt, .csv, .dat, etc.)"   , "window_import_from_text"
# "Import from Excel file..."                        , "window_import_excel"
# "Import from Rds file (.Rds, .rds)..."	         , "window_import_rds"
# "Import from R-data file (.RData, .Rda, .rda)..."  , "window_import_rdata"
# "Import from SPSS data file..."                    , "importSPSS"
# "Import from SAS xport file..."                    , "importSAS"
# "Import from SAS b7dat file..."                    , "importSASb7dat"
# "Import from STATA data file..."                   , "importSTATA"
# "Import from Minitab data file..."                 , "importMinitab"

menu_import <- function() {

    top <- CommanderWindow()

    menu_i <- tk2menu(tk2menu(top), tearoff = FALSE)
    menu_f <- tk2menu(menu_i, tearoff = FALSE)


    tkadd(menu_i, "cascade",
          label    = "From file    ",
          compound = "left",
          image    = "::image::bs_open_file",
          menu     = menu_f)

    tkadd(menu_i, "command",
          label    = "From clipboard...",
          compound = "left",
          image    = "::image::bs_paste",
          command  = window_import_from_clipboard)

    tkadd(menu_i, "command",
          label    = "From package...",
          compound = "left",
          image    = "::image::bs_package",
          command  = window_import_from_pkg)

    tkadd(menu_i, "command",
          label    = "From image (online)...",
          compound = "left",
          image    = "::image::bs_chart",
          command  = window_online_image_digitizer)

    tkadd(menu_i, "separator")

    tkadd(menu_i, "command",
          label    = "Create a new dataset...",
          compound = "left",
          image    = "::image::bs_new_doc",
          command  = window_dataset_new_rcmdr)

    tkadd(menu_f, "command",
          label    = "from Text file (.txt, .csv, .dat, .tab)...",
          compound = "left",
          image    = "::image::bs_text",
          command  = window_import_from_text)

    tkadd(menu_f, "separator")

    tkadd(menu_f, "command",
          label   = "from Excel file (.xls, .xlsx)...",
          compound = "left",
          image    = "::image::bs_excel",
          command = window_import_excel)

    tkadd(menu_f, "separator")

    tkadd(menu_f, "command",
          label   = "from Rds file (.rds)...",
          compound = "left",
          image    = "::image::bs_r",
          command = window_import_rds)

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

menu_export <- function() {
    .ds <- ActiveDataSet()
    if (is.null(.ds)) {
        command_dataset_refresh()
        active_dataset_not_persent()
        return()
    }

    top <- CommanderWindow()

    menu_e <- tk2menu(tk2menu(top), tearoff = FALSE)
    menu_c <- tk2menu(menu_e, tearoff = FALSE)


    tkadd(menu_e, "cascade",
          label    = "From file    ",
          compound = "left",
          image    = "::image::bs_open_file",
          menu     = menu_c)


    tkadd(menu_c, "command",
          label    = "Tab delimited values",
          compound = "left",
          image    = "::image::bs_copy",
          command  = function() {
              .ds <- ActiveDataSet()
              export_to_clipboard(.ds, sep = "\t")
          })

    tkadd(menu_c, "command",
          label    = "Comma separated values (csv)",
          compound = "left",
          image    = "::image::bs_copy",
          command  = function() {
              .ds <- ActiveDataSet()
              export_to_clipboard(.ds, sep = ",")
          })


    tkadd(menu_e, "separator")

    tkadd(menu_e, "command", label = "To text file (.txt, .csv)...",
          compound = "left",
          image    = "::image::bs_text",
          command  = window_export_to_textfile)

    tkadd(menu_e, "separator")

    tkadd(menu_e, "command",
          compound = "left",
          image    = "::image::bs_excel",
          label    = "To Excel file (.xlsx)...",

          command = window_export_to_excel)

    tkadd(menu_e, "separator")

    tkadd(menu_e, "command",
          label    = "To Rds file (.rds)...",
          compound = "left",
          image    = "::image::bs_r",
          command  = window_export_to_rds)

    tkadd(menu_e, "command",
          label    = "To R-data file (.RData)...",
          compound = "left",
          image    = "::image::bs_r",
          command  = window_export_to_rdata)

    tkadd(menu_e, "separator")
    tkadd(menu_e, "command",
          label    = "To Word table...",
          compound = "left",
          image    = "::image::bs_word",
          command  = to_word)

    tkadd(menu_e, "command",
          label    = "To PowerPoint table...",
          compound = "left",
          image    = "::image::bs_pptx",
          command  = to_pptx)

    tkpopup(menu_e,
            tkwinfo("pointerx", top),
            tkwinfo("pointery", top))
}


# Print menus -----------------------------------------------------------
menu_print <- function() {

    .ds <- ActiveDataSet()
    if (is.null(.ds)) {
        command_dataset_refresh()
        active_dataset_not_persent()
        return()
    }

    top <- CommanderWindow()

    menu_p  <- tk2menu(tk2menu(top), tearoff = FALSE)
    menu_md <- tk2menu(menu_p, tearoff = FALSE)
    menu_ds <- tk2menu(menu_p, tearoff = FALSE)


    view_style <- if (.Platform$GUI == "RStudio") "RStudio" else "R"

    tkadd(menu_p, "command",
          compound = "left",
          image    = "::image::viewIcon",
          label    = str_glue("View dataset (in {view_style})"),
          command  = command_dataset_view)

    tkadd(menu_p, "command",
          compound = "left",
          image    = "::image::viewIcon",
          label    = "View dataset (Rcmdr style)",
          command  = window_dataset_view_rcmdr)


    tkadd(menu_p, "separator")

    tkadd(menu_p, "cascade",
          label    = "Print as Markdown table ",
          compound = "left",
          image    = "::image::bs_md",
          menu     = menu_md)

    tkadd(menu_md, "command", label = "Engine: kable",  command = window_dataset_print_as_kable)
    tkadd(menu_md, "command", label = "Engine: pander", command = window_dataset_print_as_md)

    # tkadd(menu_p, "separator")

    tkadd(menu_p, "cascade",
          label    = "Print as dataset",
          # compound = "left",
          # image    = "::image::bs_open_file",
          menu     = menu_ds)

    tkadd(menu_ds, "command", label = "as 'data.frame'",  command = command_dataset_print_as_df)
    tkadd(menu_ds, "command", label = "as 'data.table'",  command = command_dataset_print_as_dt)
    tkadd(menu_ds, "command", label = "as 'tibble'",      command = command_dataset_print_as_tibble)

    # tkadd(menu_ds, "separator")
    tkadd(menu_p, "command", label = "Print top and bottom rows", command = summary_head_tail)

    # tkadd(menu_p, "separator")

    tkadd(menu_p, "command",
          label    = "Print as R structure",
          compound = "left",
          image    = "::image::bs_r",
          command  = to_r_structure)

    tkpopup(menu_p,
            tkwinfo("pointerx", top),
            tkwinfo("pointery", top))
}


# Print menus -----------------------------------------------------------
menu_plots <- function() {

    top <- CommanderWindow()

    menu_p <- tk2menu(tk2menu(top), tearoff = FALSE)

    tkadd(menu_p, "command",
          compound = "left",
          image    = "::image::bs_new_window",
          label    = "Open new window for plots",
          command  = open_new_plots_window)

    tkadd(menu_p, "separator")

    tkadd(menu_p, "command",
          compound = "left",
          image    = "::image::bs_pptx",
          label    = "Save editable plot to PowerPoint",
          command  = function_not_implemented)

    # tkadd(menu_p, "separator")

    tkpopup(menu_p,
            tkwinfo("pointerx", top),
            tkwinfo("pointery", top))
}
