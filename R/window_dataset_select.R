#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_dataset_select <- function() {

    dataSets <- listDataSets()
    .ds <- ActiveDataSet()

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
            "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8",

            "i1", "i2", "i3", "i4", "i5", "i6",
            "e0"
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
        cmd_refresh_listbox()

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

    # Initialize -------------------------------------------------------------
    initializeDialog(title = gettext_bs("Select Dataset"))
    tk_title(top, "Select a dataset")

    var_ds_box <-
        bs_listbox(
            parent          = top,
            title           = gettext_bs("Datasets in R memory (select one)"),
            title_sticky    = "",
            values          = dataSets,
            selection       = if (is.null(.ds)) NULL else which(.ds == dataSets) - 1,
            height          = 10,
            width           = c(47, Inf),
            on_release      = cmd_ds_selection_callback,
            on_double_click = onOK
        )

    # Info  -------------------------------------------------------------------
    info_buttons_frame <- tkframe(top)

    i1 <- tk2button(
        info_buttons_frame,
        text = "Class",
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
        width = 0,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)
            doItAndPrint(str_glue(
                "## Preview dataset '{.ds_1}'\n",
                "View({.ds_1})"
            ))
        })

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Export -----------------------------------------------------------------
    export_buttons_frame <- tkframe(top)
    export_row_1_frame <- tkframe(export_buttons_frame)
    # export_row_2_frame <- tkframe(export_buttons_frame)

    e1 <- tk2button(
        export_row_1_frame,
        text = "To R structure",
        width = 0, # 12,
        command = function() {
            .ds_1 <- get_selection(var_ds_box)
            closeDialog()

            doItAndPrint(str_glue(
                "## Export as R structure ('{.ds_1}')\n",
                "dput({.ds_1})"
            ))
            # window_dataset_new_rcmdr()
            # cmd_refresh_listbox()

        })

    e2 <- tk2button(
        export_row_1_frame,
        text = "To Rds",
        width = 0, # 12,
        command = function() {
            closeDialog()
            # window_dataset_new_rcmdr()
            # cmd_refresh_listbox()

        })

    e3 <- tk2button(
        export_row_1_frame,
        text = "To R Data",
        width = 0, #  14,
        command = function() {
            closeDialog()
            # window_import_text_delim0(init_location = "clipboard")
            # cmd_refresh_listbox()
        })





    e8 <- tk2button(
        export_row_1_frame,
        text = "More",
        width = 0, #  14,
        command = function() {
            closeDialog()
            # window_import_text_delim0(init_location = "clipboard")
            # cmd_refresh_listbox()
        })

    e4 <- tk2button(
        export_row_1_frame,
        text = "To text",
        width = 0, #  14,
        command = function() {
            closeDialog()
            # window_import_from_pkg()
            # cmd_refresh_listbox()
        })

    e5 <- tk2button(
        export_row_1_frame,
        text = "To Excel",
        width = 0, #  12,
        command = function() {
            closeDialog()
            # window_import_excel()
            # cmd_refresh_listbox()
        })

    e6 <- tk2button(
        export_row_1_frame,
        text = "To Word",
        width = 0, #  12,
        command = function() {
            closeDialog()
            # window_import_excel()
            # cmd_refresh_listbox()
        })

    e7 <- tk2button(
        export_row_1_frame,
        text = "To PowerPoint",
        width = 0, #  12,
        command = function() {
            closeDialog()
            # window_import_excel()
            # cmd_refresh_listbox()
        })

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Import -----------------------------------------------------------------
    import_buttons_frame <- tkframe(top)

    b1 <- tk2button(
        import_buttons_frame,
        text = "Create new",
        width = 0, # 12,
        command = function() {
            # closeDialog()
            window_dataset_new_rcmdr()
            cmd_refresh_listbox()

        })

    b2 <- tk2button(
        import_buttons_frame,
        text = "From clipboard",
        width = 0, #  14,
        command = function() {
            # closeDialog()
            window_import_text_delim0(init_location = "clipboard")
            cmd_refresh_listbox()
        })

    b3 <- tk2button(
        import_buttons_frame,
        text = "From package",
        width = 0, #  14,
        command = function() {
            # closeDialog()
            window_import_from_pkg()
            cmd_refresh_listbox()
        })

    b4 <- tk2button(
        import_buttons_frame,
        text = "From file",
        width = 0, #  12,
        command = function() {
            # closeDialog()
            window_import_excel()
            cmd_refresh_listbox()
        })

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # wwww ------------------------------------------------  -----------------
    manage_buttons_frame <- tkframe(top)


    b0 <- tk2button(
        manage_buttons_frame,
        text = "Create new",
        width =  15,
        command = function() {
            # closeDialog()
            window_dataset_new_rcmdr()
            cmd_refresh_listbox()

        })

    i0 <- tk2button(
        manage_buttons_frame,
        text = "Import",
        width =  15,
        command = function() {
            closeDialog()
            # window_dataset_new_rcmdr()
            # cmd_refresh_listbox()

        })

    e0 <- tk2button(
        manage_buttons_frame,
        text = "Export",
        width =  15,
        command = function() {
            closeDialog()
            # window_dataset_new_rcmdr()
            # cmd_refresh_listbox()

        })


    # Layout -----------------------------------------------------------------
    ok_cancel_help()


    # tkgrid(bs_label_b(top, text = "Import dataset"), pady = c(0, 0))
    # tkgrid(import_buttons_frame, sticky = "e")
    # tkgrid(b1, b2, b3, b4)
    #





    tkgrid(getFrame(var_ds_box), sticky = "e",  pady = c(10, 0))

    # tkgrid(bs_label_b(top, text = "More options"), pady = c(0, 0))
    tkgrid(manage_buttons_frame, sticky = "e")
    tkgrid(b0, i0, e0)



    tkgrid(bs_label_b(top, text = "Information about selected dataset"),
           pady = c(5, 0))
    tkgrid(i1, i2, i3, i4, i5, i6)
    tkgrid(info_buttons_frame, sticky = "e")



    # tkgrid(b1, b2
    # tkgrid(b3, b4)

    # tkgrid(bs_label_b(top, text = "Export selected dataset"), pady = c(5, 0))
    # tkgrid(export_buttons_frame, sticky = "e")
    # tkgrid(export_row_1_frame)
    # # tkgrid(export_row_2_frame)
    # tkgrid(e2, e3, e4, e5, e6, e8)
    # # tkgrid(e1, e2, e3, e5)
    # # tkgrid(e4, e6, e7, e8)



    tkgrid(buttonsFrame, pady = c(5, 0))

    dialogSuffix()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # menu <- tk2menu(top)           # Create a menu
    # menu_imp <- tk2menu(top)           # Create a menu
    # menu_exp <- tk2menu(top)           # Create a menu
    # tkconfigure(top, menu = menu)  # Add it to the 'win1' window
    # # tkconfigure(top, menu = menu_imp)  # Add it to the 'win1' window
    # # tkconfigure(top, menu = menu_exp)  # Add it to the 'win1' window
    #
    #
    # menuFile <- tk2menu(menu, tearoff = FALSE)
    #
    # tkadd(menu,     "cascade", label = "File", menu = menuFile)
    # tkadd(menuFile, "command", label = "Quit", command = function() tkdestroy(top))
    #
    # # tkadd(menu_imp,     "command", label = "Import", menu = menuFile)
    # tkadd(menu_exp,     "command", label = "Export", menu = menuFile)


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    cmd_ds_selection_callback()
    cmd_refresh_listbox()

    if (!isTRUE(ActiveDataSet() %in% ls(all.names = TRUE, envir = .GlobalEnv))) {
        ActiveDataSet(NULL)
    }




}
