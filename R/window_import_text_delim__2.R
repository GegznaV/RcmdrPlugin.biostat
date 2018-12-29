# fread(
#     input,
#     sep              = "auto",
#     dec              = c(".", ","),
#     quote            = c("\"", "'", "")
#
#     header           = c("auto", TRUE, FALSE)
#     skip             = "__auto__",
#     nrows            = Inf,
#
#     check.names      = FALSE,
#
#     na.strings       = "NA",  # due to change to ""; see NEWS
#     encoding         = c("unknown", "UTF-8", "Latin-1")
#
#     strip.white      = TRUE,
#     fill             = FALSE,
#     blank.lines.skip = FALSE,
#
#     stringsAsFactors = FALSE,
#     data.table       = TRUE

# ============================================================================
    # select           = NULL,
    # drop             = NULL,
    # colClasses       = NULL,
    # integer64        = getOption("datatable.integer64", "integer64"),

    # col.names,
    # key              = NULL,
    # index            = NULL,
    # showProgress     = getOption("datatable.showProgress", interactive()),
    # nThread          = getDTthreads(verbose),
    # logical01        = getOption("datatable.logical01", FALSE)  # due to change to TRUE; see NEWS
    # verbose          = getOption("datatable.verbose", FALSE),
    # file,
    # text,
    # cmd,
    # sep2           ="auto",
# )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_xxx <- function() {
    # Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ...

    # Function onOK ----------------------------------------------------------
    onOK <- function() {
        # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cursor_set_busy(top)
        on.exit(cursor_set_idle(top))

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        new_name       <- tclvalue_chr(name_variable)
        which_position <- tclvalue_chr(positionVariable)

        # Reset widget properties before checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # tkconfigure(name_entry, foreground = "black")

        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is_empty_name(new_name)) {
            return()
        }

        if (is_not_valid_name(new_name)) {
            return()
        }

        if (forbid_to_replace_variables(new_name)) {
            return()
        }

        if (variable_is_not_selected(new_name, "variable")) {
            return()
        }

        if (variable_is_not_selected(new_name, "group variable")) {
            return()
        }

        # if (forbid_to_replace_object(new_name)) {
        #     return()
        # }

        # if (is_empty_name(new_name))              {return()}
        # if (is_not_valid_name(new_name))          {return()}
        # if (forbid_to_replace_variable(new_name)) {return()}

        # if (object_is_not_selected(new_name))     {return()}
        # if (forbid_to_replace_object(new_name))   {return()}

        # if (??? == "") {
        # show_error_messages(
        #     "No ???  was selected.\nPlease select a ???.",
        #     title = "")
        # return()
        # }

        #
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_xxx", list(
            initial_position = which_position
        ))

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cmd_position <-
            switch(which_position,
                   "first" = str_glue(
                       "%>% \n dplyr::select({new_name}, everything())"),
                   "last" = "")

        cmd_ungroup <- if (is_grouped_df(ds)) "ungroup() %>% \n" else ""

        command <- str_glue(
            '## Add column with row numbers \n',
            "{ds} <- {ds} %>% \n",
            "{cmd_ungroup}",
            "dplyr::mutate({new_name} = 1:n())",
            "{cmd_position}")

        # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("tidyverse")

        # doItAndPrint(command)
        result <- justDoIt(command)

        # result <- try_command(command)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (class(result)[1] != "try-error") {
            logger(style_cmd(command))
            # doItAndPrint(style_cmd(command))

            activeDataSet(ds, flushModel = FALSE, flushDialogMemory = FALSE)

            # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            closeDialog()


        } else {
            logger_error(command, error_msg = result)
            show_code_evaluation_error_message()
            return()
        }

        # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # closeDialog()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command_dataset_refresh()
        tkfocus(CommanderWindow())
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Announce about the success to run the function `onOk()`
        TRUE
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }

    # Initial values ---------------------------------------------------------

    # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    .ds     <- activeDataSet()
    fg_col <- Rcmdr::getRcmdr("title.color")

    # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initializeDialog(title = gettext_bs("Import Data from Text"))

    tk_title(top, "Import Data from Text") # Title ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    defaults <- list(
        position = "first"
    )
    initial <- getDialog("window_xxx", defaults)





    # initialize values
    cb <- NULL

    nrows <- 50

    dec0 <- c(".", ",")
    dec1 <- c("Period ( . )", "Comma ( , )")

    sep0 <- c("auto", " ", "\t", ",", ";", "|", NA)
    sep1 <- c("Auto", "White space ( )", "Tab ( \\t )", "Comma ( , )", "Semicolon ( ; )", "Pipe ( | )", "Custom\u2026")

    nas0 <- c("NA", "na", "N/A", "n/a", NA)
    nas1 <- c("NA", "na", "N/A", "n/a", "Custom\u2026")

    quo0 <- c("\"", "'", "", NA)
    quo1 <- c("Double ( \" )", "Single ( \' )", "None", "Custom\u2026" )

    max1  <- c("All",  "Custom\u2026")
    skip1 <- c("Auto", "Custom\u2026")

    # com0 <- c("#", "!", "\\", "~", NA)
    # com1 <- c(
    #     "#",
    #     "!",
    #     "\\\\",
    #     "~",
    #     "Custom\u2026"
    # )

    enc0 <- c("unknown", "UTF-8", "Latin-1")
    enc1 <- enc0

    # assign variables linked to Tk widgets
    table_var   <- tclArray()


    # nrow_var    <- tclVar()
    # source_var  <- tclVar()
    # sep_var     <- tclVar()
    # nas_var     <- tclVar()
    # com_var     <- tclVar()
    # tt_done_var <- tclVar(0)
    # skip_var    <- tclVar(FALSE)
    # str_as_fact_var <- tclVar(0)

    # ... Widgets ============================================================
    # Widgets ----------------------------------------------------------------

    # frame 1, file locator ----------------------- --------------------------
    f1 <- ttkframe(top, relief = "flat", padding = 0, borderwidth = 0)

    f1_lab_1_1 <- ttklabel(f1, text = "Import from")
    txt <- paste(
        "or transfer data from clipboard via a copy and paste operation. ",
        "The first part of the data table will be shown below."
    )
    f1_lab_2_1 <- ttklabel(f1, text = txt)

    f1_ent_1_2 <- ttkentry(f1, textvariable = source_var)
    f1_but_1_3 <- ttkbutton(f1, width = 8, text = "Browse", command = GetDataFile)

    tkgrid(f1_lab_1_1, f1_ent_1_2, f1_but_1_3, pady = c(10, 0))
    tkgrid(f1_lab_2_1, "x", "x", "x", pady = c(5, 0), padx = c(15, 0))

    tkgrid.configure(f1_lab_1_1, sticky = "w")
    tkgrid.configure(f1_ent_1_2, sticky = "we", padx = 2)

    tkgrid.configure(f1_lab_2_1, columnspan = 3, sticky = "w")

    tkgrid.columnconfigure(f1, 1, weight = 1)

    tkgrid(f1, padx = 10, sticky = "w")

    # tkpack(f1, fill = "x", anchor = "w", padx = 10)

    # frame 2, header line information ----- ----------------------------------
    # f2 <- ttklabelframe(top, relief = "flat", borderwidth = 5, padding = 5, text = "Header lines")
    #
    # txt <- paste(
    #     "Format conversion specification strings of the variables,",
    #     "for example, '%10.6f' and '%Y-%m-%d %H:%M'."
    # )
    # f2.chk.1.1 <- ttkcheckbutton(f2, variable = conv.fmts.var, command = SetTags, text = txt)
    # txt <- "Field names of the variables, that is, names given to the columns in the data table."
    # f2.chk.2.1 <- ttkcheckbutton(f2, variable = col.names.var, command = SetTags, text = txt)
    #
    # tkgrid(f2.chk.1.1, pady = 1, sticky = "w")
    # tkgrid(f2.chk.2.1, pady = 1, sticky = "w")
    #
    # tkpack(f2, anchor = "w", fill = "x", padx = 10, pady = 10)

    # frame 3, import parameters ------------ ---------------------------------

    # txt <- paste(
    #     "Comments located above data records and header lines will be preserved;",
    #     "all other comments are ignored."
    # )
    # f3_lab_4_1 <- ttklabel(f3, text = txt, foreground = "#A40802")


    # input,
    # sep              = "auto",
    # dec              = c(".", ",")
    # quote            = c("\"", "'", "")

    # skip             = "__auto__"
    # nrows            = Inf


    # na.strings       = "NA",  # due to change to ""; see NEWS
    # encoding         = c("unknown", "UTF-8", "Latin-1")

    check.names      = FALSE
    strip.white      = TRUE
    fill             = FALSE
    blank.lines.skip = FALSE
    stringsAsFactors = FALSE

    header           = c("auto", TRUE, FALSE)
    data.table       = TRUE




    enable_entry <- function(txt, default = "") {
        obj_1 <- get(str_glue("f3_box_{txt}"), envir = parent.frame())
        obj_2 <- get(str_glue("f3_ent_{txt}"), envir = parent.frame())

        cond <- str_detect(get_selection(obj_1), "Custom")
        if (cond) {
            if (disabled(obj_2$obj_text)) {
                set_values(obj_2, default)
            }
            tk_normalize(obj_2)

        } else {
            set_values(obj_2, "")
            tk_disable(obj_2)
        }
    }

    select_custom <- function(txt, default = "") {
        obj_1 <- get(str_glue("f3_box_{txt}"), envir = parent.frame())
        obj_2 <- get(str_glue("f3_ent_{txt}"), envir = parent.frame())


        if (disabled(obj_2$obj_text)) {
            set_values(obj_2, default)
        }
        set_selection(obj_1, "Custom\u2026")
        tk_normalize(obj_2)

    }



    f3 <- tk2labelframe(top, relief = "flat", borderwidth = 5, padding = 5, text = "Options")
    tkgrid(f3, sticky = "W", padx = 10, pady = c(0, 15))

    # tkdestroy(f3)

    f3_lab_dec  <- tk2label(f3, text = "Decimal")
    f3_lab_sep  <- tk2label(f3, text = "Separator")
    f3_lab_skip <- tk2label(f3, text = "Skip lines")
    f3_lab_max  <- tk2label(f3, text = "Max lines")
    f3_lab_na   <- tk2label(f3, text = "NA string")
    f3_lab_enc  <- tk2label(f3, text = "Encoding")
    f3_lab_quo  <- tk2label(f3, text = "Quote")


    tip_box_dec  <- "Separator for decimal part \nof a number: 10.4 vs. 10,4."
    tip_box_sep  <- "Field (value) separator character."
    tip_box_skip <- "Number of rows to skip. \nInteger from 0 to infinity.\n0 equals to \"auto\"."
    tip_box_max  <- "Maximum number of rows to read. \nInteger from 0 to infinity."
    tip_box_na   <- "A character vector of strings which \nare interpreted as missing (NA) values."
    tip_box_quo  <- "Quoting characters. \nCharacters between quotes are read as one value."
    tip_box_enc  <- "Encoding."

    f3_box_dec  <- bs_combobox(f3, width = 13, values = dec1,  tip = tip_box_dec,  selection = 1)
    f3_box_sep  <- bs_combobox(f3, width = 13, values = sep1,  tip = tip_box_sep,  selection = 1, on_select = function() {enable_entry("sep", "||")})
    f3_box_skip <- bs_combobox(f3, width = 13, values = skip1, tip = tip_box_skip, selection = 1, on_select = function() {enable_entry("skip", "0")})
    f3_box_max  <- bs_combobox(f3, width = 13, values = max1,  tip = tip_box_max,  selection = 1, on_select = function() {enable_entry("max",  "0")})
    f3_box_na   <- bs_combobox(f3, width = 13, values = nas1,  tip = tip_box_na,   selection = 1, on_select = function() {enable_entry("na",   "?")})
    f3_box_quo  <- bs_combobox(f3, width = 13, values = quo1,  tip = tip_box_quo,  selection = 1, on_select = function() {enable_entry("quo",  "\"")})
    f3_box_enc  <- bs_combobox(f3, width = 13, values = enc1,  tip = tip_box_enc,  selection = 1)


    tip_ent <- "Double click to enable."
    # f3_ent_dec  <- bs_tk_textbox(f3, width = 4)
    f3_ent_sep  <- bs_tk_textbox(f3, width = 4, tip = tip_ent, on_double_click = function() {select_custom("sep", "||")})
    f3_ent_skip <- bs_tk_textbox(f3, width = 4, tip = tip_ent, on_double_click = function() {select_custom("skip", "0")})
    f3_ent_max  <- bs_tk_textbox(f3, width = 4, tip = tip_ent, on_double_click = function() {select_custom("max",  "0")})
    f3_ent_na   <- bs_tk_textbox(f3, width = 4, tip = tip_ent, on_double_click = function() {select_custom("na",   "?")})
    f3_ent_quo  <- bs_tk_textbox(f3, width = 4, tip = tip_ent, on_double_click = function() {select_custom("quo",  "\"")})
    # f3_ent_enc  <- bs_tk_textbox(f3, width = 4)

    tkgrid(f3_lab_dec,  f3_box_dec$frame,  "x",               pady = c(2, 0))
    tkgrid(f3_lab_sep,  f3_box_sep$frame,  f3_ent_sep$frame,  pady = c(2, 0))
    tkgrid(f3_lab_skip, f3_box_skip$frame, f3_ent_skip$frame, pady = c(2, 0))
    tkgrid(f3_lab_max,  f3_box_max$frame,  f3_ent_max$frame,  pady = c(2, 0))
    tkgrid(f3_lab_na,   f3_box_na$frame,   f3_ent_na$frame,   pady = c(2, 0))
    tkgrid(f3_lab_quo,  f3_box_quo$frame,  f3_ent_quo$frame,  pady = c(2, 0))
    tkgrid(f3_lab_enc,  f3_box_enc$frame,  "x",               pady = c(2, 0))

    tkgrid.configure(
        f3_lab_dec, f3_lab_sep, f3_lab_skip, f3_lab_max, f3_lab_na, f3_lab_enc,
        f3_lab_quo,
        padx = c(3, 5), sticky = "w"
    )

    tkgrid.configure(
        f3_ent_sep$frame,
        f3_ent_skip$frame,
        f3_ent_max$frame,
        f3_ent_na$frame,
        f3_ent_quo$frame,
        padx = c(2, 0)
    )

    list(f3_ent_sep, f3_ent_skip, f3_ent_max, f3_ent_na, f3_ent_quo) %>%
        walk(tk_disable)

    # Check box
    f3_opts <- bs_checkboxes(f3,
                  # boxes = c("check_names", "strip_white"),
                  boxes = c("check_names", "stringsAsFactors", "strip_white", "blank_lines_skip"), # "fill",
                  initialValues = c(0, 0, 1, 0),
                  # commands      = list("check_locale_" = cmd_checkbox),
                  labels        = gettext_bs(c(
                      "Check names",
                      "Convert strings to factors",
                      "Strip leading and tailing spaces",
                      "Skip empty lines"
                      ))
    )

    tkgrid(f3_opts$frame, padx = c(3, 0), pady = c(4, 2), columnspan = 3, sticky = "w")


    # tkpack(f3, anchor = "w", fill = "x", padx = 10, pady = c(0, 15))

    # tcl(f3_box_1_2, "current", 0)
    # tcl(f3_box_1_5, "current", 0)
    # tcl(f3_box_2_2, "current", 0)
    # tcl(f3_box_2_5, "current", 0)
    # tcl(f3_box_3_2, "current", 0)
    # tcl(f3_box_3_5, "current", 0)
    #
    # if (!is_null(Data(c("import", "sep")))) {
    #     if (Data(c("import", "sep")) %in% sep0) {
    #         tcl(f3_box_1_2, "current", match(Data(c("import", "sep")), sep0) - 1)
    #         tkconfigure(f3_ent_1_3, state = "disabled")
    #     } else {
    #         tcl(f3_box_1_2, "current", match(NA, sep0) - 1)
    #         tkconfigure(f3_ent_1_3, state = "normal")
    #         tclvalue(sep_var) <- Data(c("import", "sep"))
    #     }
    # }
    # if (!is_null(Data(c("import", "na")))) {
    #     if (Data(c("import", "na")) %in% nas0) {
    #         tcl(f3_box_2_2, "current", match(Data(c("import", "na")), nas0) - 1)
    #         tkconfigure(f3_ent_2_3, state = "disabled")
    #     } else {
    #         tcl(f3_box_2_2, "current", match(NA, nas0) - 1)
    #         tkconfigure(f3_ent_2_3, state = "normal")
    #         tclvalue(nas_var) <- Data(c("import", "na"))
    #     }
    # }
    # if (!is_null(Data(c("import", "comment")))) {
    #     if (Data(c("import", "comment")) %in% com0) {
    #         tcl(f3_box_3_2, "current", match(Data(c("import", "comment")), com0) - 1)
    #         tkconfigure(f3_ent_3_3, state = "disabled")
    #     } else {
    #         tcl(f3_box_3_2, "current", match(NA, com0) - 1)
    #         tkconfigure(f3_ent_3_3, state = "normal")
    #         tclvalue(com_var) <- Data(c("import", "comment"))
    #     }
    # }
    # if (!is_null(Data(c("import", "dec")))) {
    #     tcl(f3_box_1_5, "current", match(Data(c("import", "dec")), dec0) - 1)
    # }
    # if (!is_null(Data(c("import", "quote")))) {
    #     tcl(f3_box_2_5, "current", match(Data(c("import", "quote")), quo0) - 1)
    # }
    # if (!is_null(Data(c("import", "encoding")))) {
    #     tcl(f3_box_3_5, "current", match(Data(c("import", "encoding")), enc0) - 1)
    # }

    # frame 4, example data table ------------ --------------------------------

    f4 <- ttkframe(top, relief = "flat", padding = 0, borderwidth = 0)
    tkgrid(f4, sticky = "news")

    f4.tbl <- tk2table(
        parent             = f4,
        rows               = 1,
        cols               = 1,
        variable           = table.var,
        state              = "disabled",
        colwidth           = 13,
        rowheight          = 1,
        width              = 1,
        height             = 5,
        ipadx              = 3,
        ipady              = 1,
        wrap               = 0,
        highlightcolor     = "gray75",
        background         = "white",
        foreground         = "black",
        titlerows          = 0,
        titlecols          = 0,
        multiline          = 0,
        resizeborders      = "col",
        bordercursor       = "sb_h_double_arrow",
        cursor             = "plus",
        colstretchmode     = "none",
        rowstretchmode     = "none",
        anchor             = "nw",
        drawmode           = "single",
        rowseparator       = "\n",
        colseparator       = "\t",
        selectmode         = "extended",
        insertofftime      = 0,
        highlightthickness = 0,
        font = "TkFixedFont",
        xscrollcommand = function(...)
            tkset(f4.xsc, ...),
        yscrollcommand = function(...
        ) tkset(f4.ysc, ...)
    )



    f4.xsc <- ttkscrollbar(f4,
                           orient = "horizontal",
                           command = function(...) tkxview(f4.tbl, ...)
    )
    f4.ysc <- ttkscrollbar(f4,
                           orient = "vertical",
                           command = function(...) tkyview(f4.tbl, ...)
    )
    #
    tkgrid(f4.tbl, f4.ysc)
    tkgrid(f4.xsc, "x")

    tkgrid.configure(f4.tbl, sticky = "news", padx = c(10, 0))
    tkgrid.configure(f4.ysc, sticky = "ns", padx = c(0, 10))
    tkgrid.configure(f4.xsc, sticky = "we", padx = c(10, 0))

    tktag.configure(f4.tbl, "active", background = "#EAEEFE", relief = "")
    tktag.configure(f4.tbl, "sel",    background = "#EAEEFE", foreground = "black")

    tkgrid.columnconfigure(f4, 0, weight = 1)
    tkgrid.rowconfigure(f4, 0, weight = 1)
    #
    # tkpack(f4, fill = "both", expand = TRUE)
    #
    tkselection.set(f4.tbl, "origin")


    # ====... =================== ============================================
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # Finalize ---------------------------------------------------------------

    # Help topic
    # OKCancelHelp(helpSubject = "mutate", helpPackage = "dplyr")

    ok_cancel_help(helpSubject = "xxx", helpPackage = "xxx",
                   reset = "window_xxx()",
                   apply = "window_xxx()")

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # Interactive bindings ---------------------------------------------------

    # Add interactivity for `fname_frame` and `fname_label`
    # tkbind(file_label,     "<ButtonPress-1>", on_click)
    # tkbind(fname_frame,    "<ButtonPress-1>", on_click)
    # tkbind(fname_label,    "<ButtonPress-1>", on_click)
    #
    # tkbind(fname_frame, "<Enter>",
    #        function() tkconfigure(fname_label, foreground = "blue"))
    # tkbind(fname_frame, "<Leave>",
    #        function() tkconfigure(fname_label, foreground = "black"))
    # # tkconfigure(file_label,     cursor = "hand2")
    # tkconfigure(fname_frame,    cursor = "hand2")
    # tkconfigure(button_ch_file, cursor = "hand2")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


}
