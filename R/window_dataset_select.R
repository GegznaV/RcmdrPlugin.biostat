#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_select <- function() {

    dataSets <- listDataSets()
    .ds      <- ActiveDataSet()

    # Functions --------------------------------------------------------------
    cmd_refresh_listbox  <- function(variables) {

        set_values(var_ds_box, listDataSets())

        if (get_size(var_ds_box) == 0) {
            tk_disable(var_ds_box)

        } else if (!is.null(.ds)) {
            set_selection(var_ds_box, .ds)
            tkyview(var_ds_box$listbox, which(get_values(var_ds_box) == .ds) - 1)
        }
    }

    cmd_ds_selection_callback  <- function() {

        envir = parent.frame()
        button_obj <- c(
            "i1", "i2", "i3", "i4", "i5", "i6",
            "e0", "p0"
        )

        if (get_size(var_ds_box) == 0 || get_selection_length(var_ds_box) == 0) {
            # Disable buttons
            eval_glue("tk_disable({button_obj})",    eval_envir = envir)

        } else {
            # Normalize buttons
            eval_glue("tk_normalize({button_obj})", eval_envir = envir)
        }
    }

    to_pptx <- function(variables) {

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

    to_docx <- function(variables) {

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

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        if (length(dataSets) == 0) {
            RcmdrTkmessageBox(
                "There are no datasets in R memory.\nPlease, create or import a dataset.",
                icon = "warning",
                title = "No Dataset Found in R",
                type = "ok")

            return()
        }


        if (get_selection_length(var_ds_box) == 0) {
            RcmdrTkmessageBox("Please, select a dataset.",
                              title = "Dataset Not Selected",
                              icon = "warning",
                              type = "ok")
            return()
        }

        selection <- getSelection(var_ds_box)
        closeDialog()

        active_dataset(selection)
        # active_datatet(selection)
        tkfocus(CommanderWindow())
    }

    # Menu -------------------------------------------------------------------

    from_clipboard <- function() {
        closeDialog()
        window_import_text_delim0(init_location = "clipboard")
    }

    from_package <- function() {
        closeDialog()
        window_import_from_pkg()
        # window_dataset_select()
    }

    from_rds <- function() {
        closeDialog()
        window_import_rds()
        # window_dataset_select()
    }

    from_rdata <- function() {
        closeDialog()
        window_import_rdata()
        # window_dataset_select()
        }

    from_text_file <- function() {
        closeDialog()
        window_import_text_delim()
        # window_dataset_select()
    }

    from_excel <- function() {
        closeDialog()
        window_import_excel()
        # window_dataset_select()
    }



    to_rds <- function() {
        closeDialog()
        window_export_to_rds()
        # window_dataset_select()
    }

    to_rdata <- function() {
        closeDialog()
        window_export_to_rdata()
        # window_dataset_select()
    }

    to_r_structure <- function() {
        .ds_1 <- get_selection(var_ds_box)
        # closeDialog()

        doItAndPrint(str_glue(
            "## Export as R structure ('{.ds_1}')\n",
            "dput({.ds_1})"
        ))
    }

    to_text_file <- function() {
        closeDialog()
        window_export_to_textfile()
        # window_dataset_select()
    }

    to_excel <- function() {
        closeDialog()
        window_export_to_excel()
        # window_dataset_select()
    }

    to_word <- function() {
        closeDialog()
        # window_export_
        # window_dataset_select()
    }

    to_pptx <- function() {
        closeDialog()
        # window_export_
        # window_dataset_select()
    }


    # Import menus -----------------------------------------------------------

    # "From clipboard..."     , 'window_import_text_delim0(init_location = "clipboard")'
    # "From R package... "    , "window_import_from_pkg"
    #
    # "Import from text file (.txt, .csv, .dat, etc.)"   , "window_import_text_delim"
    # "Import from Excel file..."                        , "window_import_excel"
    # "Import from Rds file (.Rds, .rds)..."	           , "window_import_rds"
    # "Import from R-data file (.RData, .Rda, .rda)..."  , "window_import_rdata"
    # "Import from SPSS data file..."                    , "importSPSS"
    # "Import from SAS xport file..."                    , "importSAS"
    # "Import from SAS b7dat file..."                    , "importSASb7dat"
    # "Import from STATA data file..."                   , "importSTATA"
    # "Import from Minitab data file..."                 , "importMinitab"

    menu_import <- function() {
        top <- top

        menu_i <- tk2menu(tk2menu(top), tearoff = FALSE)
        menu_f <- tk2menu(menu_i, tearoff = FALSE)

        tkadd(menu_i, "cascade", label = "From file    " , menu = menu_f)
        tkadd(menu_i, "command", label = "From package",   command = from_package)
        tkadd(menu_i, "command", label = "From clipboard", command = from_clipboard)


        tkadd(menu_f, "command", label = "from Rds file (.Rds, .rds)..."	        , command = from_rds)
        tkadd(menu_f, "command", label = "from R-data file (.RData, .Rda, .rda)..." , command = from_rdata)
        tkadd(menu_f, "separator")
        tkadd(menu_f, "command", label = "from text file (.txt, .csv, .dat, etc.)"  , command = from_text_file)
        tkadd(menu_f, "separator")
        tkadd(menu_f, "command", label = "from Excel file..."                       , command = from_excel)

        # tkadd(menu_f, "separator")
        # tkadd(menu_f, "command", label = "from SPSS data file..."                   , command = function() {importSPSS()})
        # tkadd(menu_f, "command", label = "from SAS xport file..."                   , command = function() {importSAS()})
        # tkadd(menu_f, "command", label = "from SAS b7dat file..."                   , command = function() {importSASb7dat()})
        # tkadd(menu_f, "command", label = "from STATA data file..."                  , command = function() {importSTATA()})
        # tkadd(menu_f, "command", label = "from Minitab data file..."                , command = function() {importMinitab()})

        tkpopup(menu_i,
                tkwinfo("pointerx", top),
                tkwinfo("pointery", top))
    }

    # Export menus -----------------------------------------------------------
    menu_export <- function() {
        top <- top

        menu_e <- tk2menu(tk2menu(top), tearoff = FALSE)

        tkadd(menu_e, "command", label = "to Rds file (.Rds, .rds)..."	        ,   command = to_rds)
        tkadd(menu_e, "command", label = "to R-data file (.RData, .Rda, .rda)..." , command = to_rdata)
        tkadd(menu_e, "separator")
        tkadd(menu_e, "command", label = "to text file (.txt, .csv, .dat, etc.)"  , command = to_text_file)
        tkadd(menu_e, "separator")
        tkadd(menu_e, "command", label = "to Excel file..."                       , command = to_excel)
        # tkadd(menu_e, "command", label = "to Word table..."                       , command = to_word)
        # tkadd(menu_e, "command", label = "to PowerPoint table..."                 , command = to_pptx)
        # tkadd(menu_e, "separator")
        # tkadd(menu_e, "command", label = "print as R structure"                   , command = to_r_structure)

        tkpopup(menu_e,
                tkwinfo("pointerx", top),
                tkwinfo("pointery", top))
    }


    # Print menus -----------------------------------------------------------
    menu_print <- function() {
        top <- top

        menu_p <- tk2menu(tk2menu(top), tearoff = FALSE)

        tkadd(menu_p, "command", label = "Print as Markdown table (engine: kable)",  command = window_dataset_print_as_kable)
        tkadd(menu_p, "command", label = "Print as Markdown table (engine: pander)", command = window_dataset_print_as_md)
        tkadd(menu_p, "separator")
        tkadd(menu_p, "command", label = "Print as 'data.frame'",  command = command_dataset_print_as_df)
        tkadd(menu_p, "command", label = "Print as 'data.table'",  command = command_dataset_print_as_dt)
        tkadd(menu_p, "command", label = "Print as 'tibble'",      command = command_dataset_print_as_tibble)
        tkadd(menu_p, "command", label = "Print top and bottom rows only", command = summary_head_tail)
        tkadd(menu_p, "separator")
        tkadd(menu_p, "command", label = "Print as R structure"  , command = to_r_structure)
        tkpopup(menu_p,
                tkwinfo("pointerx", top),
                tkwinfo("pointery", top))
    }


    # Initialize -------------- ----------------------------------------------
    initializeDialog(title = gettext_bs("Select & Explore Dataset"))
    tk_title(top, "Select & Explore a Dataset")

    # Widgets ----------------------------------------------------------------
    var_ds_box <-
        bs_listbox(
            parent          = top,
            title           = gettext_bs("Datasets in R memory (select one)"),
            title_sticky    = "",
            values          = dataSets,
            selection       = if (is.null(.ds)) NULL else which(.ds == dataSets),
            height          = 10,
            width           = c(47, Inf),
            on_release      = cmd_ds_selection_callback,
            on_double_click = onOK
        )

    tkgrid(getFrame(var_ds_box), sticky = "e",  pady = c(10, 0))


    # Dataset info buttons ---------------------------------------------------
    info_buttons_frame <- tkframe(top)

    i1 <- tk2button(
        info_buttons_frame,
        text = "Class",
        tip  = "Print class of the selected dataset.",
        width = 5,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)

            doItAndPrint(str_glue(
                "## The class of dataset '{.ds_1}'\n",
                "class({.ds_1})"))
        })

    i2 <- tk2button(
        info_buttons_frame,
        text = "Size",
        tip  = str_c("Print selected dataset's size and",
                     "column type frequencies.",
                     sep = "\n"),

        width = 4,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)
            # doItAndPrint(str_glue(
            #     "## The size of dataset '{.ds_1}' (rows, columns)\n",
            #     "dim({.ds_1})"))

            doItAndPrint(str_glue(
                "## The size of dataset '{.ds_1}'\n",
                "summary(skimr::skim({.ds_1}))"
            ))
        })


    i3 <- tk2button(
        info_buttons_frame,
        text = "Variables",
        tip  = str_c("Print variable names in",
                     "the selected dataset.",
                     sep = "\n"),
        width = 0,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)
            doItAndPrint(str_glue(
                "## Variable names in dataset '{.ds_1}'\n",
                "names({.ds_1})"
            ))
        })

    i4 <- tk2button(
        info_buttons_frame,
        text = "Structure",
        tip  = str_c("Print selected dataset's structure:",
                     "variable names, types and several ",
                     "first values.",
                     sep = "\n"),
        width = 0,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)

            doItAndPrint(str_glue(
                "## The structure of dataset '{.ds_1}'\n",
                "dplyr::glimpse({.ds_1})"))
        })

    i5 <- tk2button(
        info_buttons_frame,
        text = "Summary",
        tip  = str_c("Print summary of the variables",
                     "in the selected dataset.",
                     sep = "\n"),
        width = 0,
        command = function() {

            .ds_1 <- get_selection(var_ds_box)

            Library("tidyverse")
            Library("skimr")

            doItAndPrint(str_glue(
                "skimr::skim_with(\n",
                "    numeric = list(hist = NULL),\n",
                "    integer = list(hist = NULL) \n",
                ")\n\n"
            ))
            doItAndPrint(str_glue(
                "## Summary of the variables in dataset '{.ds_1}'\n",
                "skimr::skim({.ds_1})"
            ))

            # If any factors exist
            ds_factors <-
                purrr::map_lgl(eval_glue("{.ds_1}", envir_eval = .GlobalEnv),
                               ~inherits(., "factor"))
            get(.ds_1)
            print(.ds_1)
            print(ds_factors)

            if (any(ds_factors)) {
                doItAndPrint(style_cmd(str_glue(
                    "## More details on categorical variables\n",
                    "{.ds_1} %>% \n ",
                    "dplyr::select_if(is.factor) %>% \n",
                    "purrr::map(~data.frame(n = summary(.)))"
                )))
            }

        })

    i6 <- tk2button(
        info_buttons_frame,
        text = "Preview",
        tip  = str_c("Preview the  selected dataset",
                     "in a separate window.",
                     sep = "\n"),
        width = 0,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)
            doItAndPrint(str_glue(
                "## Preview dataset '{.ds_1}'\n",
                "View({.ds_1})"
            ))
        })

    # tkgrid(bs_label_b(top, text = "Information about selected dataset"),
    # pady = c(5, 0))
    tkgrid(i1, i2, i3, i4, i5, i6)
    tkgrid(info_buttons_frame, sticky = "e")


    # Import / Export buttons ------------------------------------------------------------
    io_buttons_frame <- tkframe(top)

    c0 <- tk2button(
        io_buttons_frame,
        text  = "Create new",
        tip   = "Create a new dataset.",
        width =  11,
        command = function() {
            window_dataset_new_rcmdr()
            closeDialog()

        })

    i0 <- tk2button(
        io_buttons_frame,
        text  = "Import",
        tip   = str_c("Import a dataset from file",
                      "clipboard or R package.",
                      sep = "\n"),
        width =  11,
        command = function() {menu_import()})

    e0 <- tk2button(
        io_buttons_frame,
        tip  = str_c("Export selected dataset to file.",
                     sep = "\n"),
        text = "Export",
        width =  11,
        command = function() {menu_export()})

    p0 <- tk2button(
        io_buttons_frame,
        tip  = str_c("Print selected dataset to console.",
                     sep = "\n"),
        text = "Print",
        width =  11,
        command = function() {menu_print()})

    tkgrid(io_buttons_frame, sticky = "e", pady = c(10, 0))
    tkgrid(c0, i0, e0, p0)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Finalize ---------------------------------------------------------------
    ok_cancel_help()
    tkgrid(buttonsFrame, pady = c(0, 0))
    dialogSuffix()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply functions --------------------------------------------------------

    cmd_ds_selection_callback()
    cmd_refresh_listbox()

    if (!isTRUE(ActiveDataSet() %in% ls(all.names = TRUE, envir = .GlobalEnv))) {
        ActiveDataSet(NULL)
    }

}
