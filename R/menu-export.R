
# Export dataset =============================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_rds  <- function() {
    window_export_to_rds_0(ds_name = active_dataset())
}


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_rds_0 <- function(ds_name = active_dataset()) {
    file_name <- ds_name
    .ds <- safe_names(ds_name)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    file_name <- get_filename_to_save(
        title = "Save data to Rds file.",
        file_name = file_name,
        filetypes = "{ {Rds file} {.rds} } { {All Files} * }",
        defaultextension = "rds"
    )

    # If canceled
    if (nchar(file_name) == 0) {
        Message("Canceled. Object was not saved.", type = "warning")
        return()
    }

    # Change these lines: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    make_relative = TRUE

    if (make_relative) {
        # file_name <- fs::path_rel(file_name)
        file_name <- make_relative_path(file_name)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    command <- str_glue("## Save data to 'Rds' file\n",
                        'saveRDS({.ds}, file = "{file_name}")')
    doItAndPrint(command)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_rdata  <- function() {
    window_export_to_rdata_0(ds_name = active_dataset())
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_rdata_0 <- function(ds_name = active_dataset()) {
    file_name <- ds_name
    .ds <- safe_names(ds_name)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    file_name <- get_filename_to_save(
        title = "Save data to R Data file",
        file_name = file_name,
        filetypes = "{ {RData file} {.RData} } { {All Files} * }",
        defaultextension = "RData"
    )

    # If canceled
    if (nchar(file_name) == 0) {
        Message("Canceled. Object was not saved.", type = "warning")
        return(NULL)
    }

    # Change these lines: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    make_relative = TRUE

    if (make_relative) {
        # file_name <- fs::path_rel(file_name)
        file_name <- make_relative_path(file_name)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    command <- str_glue(
        "## Save data to 'RData' file\n",
        'save({.ds}, file = "{file_name}")') %>%
        style_cmd()

    doItAndPrint(command)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TODO:
#  1. Allow changing sheetnames.
#  2. Allow changing more saving options.
#
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_excel_old <- function() {
    file_name <- ""

    while (TRUE) {
        break_cycle <- "yes"

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        typevariable <- tclVar("")
        file_name    <- tclvalue(tkgetSaveFile(
            typevariable = typevariable, # to capture selected type
            title = "Save data to Excel file",
            initialfile =
                if (nchar(trimws(file_name)) > 0) {
                    extract_filename(file_name)
                } else {
                    active_dataset()
                },
            filetypes = "{ {Excel file} {.xlsx} } { {All Files} * }"))

        ## Returns selected file type:
        # tclvalue(typevariable)

        # If canceled
        if (nchar(trimws(file_name)) == 0) {
            Message("Operation canceled, object was not saved.",
                    type = "warning")
            return()
        }

        # Add extension if missing
        if (!grepl("\\.xlsx$", file_name)) {
            # Add extension
            file_name <- paste0(file_name, ".xlsx")

            # Check if a file with the same name exists
            if (file.exists(file_name))
                break_cycle <- tclvalue(Rcmdr::checkReplace(
                    str_glue('"{file_name}"'), type = "File"))
        }

        # Exit the cycle, if everything is selected correctly
        if (break_cycle == "yes") {
            break
        }
    }

    # Change these lines: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    make_relative <- TRUE

    if (make_relative) {
        file_name <- make_relative_path(file_name)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # sheet_names_list <-
    #     if (file.exists(file_name)) {
    #         openxlsx::getSheetNames(file_name)
    #     } else {
    #         ""
    #     }
    #
    # sheet_name <- unique_obj_names(str_glue("{active_dataset()} {Sys.Date()}"),
    #                                list_of_choices = sheet_names_list)

    sheet_name <-
        str_c(str_trunc(active_dataset(), 19, ellipsis = ""),
              Sys.Date(), sep = " ")

    has_rownames <- tibble::has_rownames(get(active_dataset(),
                                             envir = .GlobalEnv))

    file_overwrite <- TRUE

    command <-
        str_glue("## Save data to Excel file\n",
                 "openxlsx::write.xlsx({active_dataset()}, \n",
                 'file = "{file_name}", \n',
                 'sheetName = "{sheet_name}", \n',
                 "rowNames  = {has_rownames}, \n",
                 "colNames  = TRUE, \n",
                 'colWidths = "auto",\n',
                 "overwrite = {file_overwrite})"
        ) %>%
        style_cmd()

    doItAndPrint(command)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# From Rcmdr
#
# TODO:
#  1. Add more saving options compatibele with `readr`;
#  2. Move row names to column to avoid column without name (i.e., column with rownames);
#
#
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_to_textfile <- function() {

    dsname <- active_dataset()
    has_rownames <-
        tibble::has_rownames(get(active_dataset(), envir = .GlobalEnv))


    initializeDialog(title = gettext_bs("Save Data to Text File"))

    checkBoxes(
        frame = "optionsFrame",
        boxes = c("colnames", "rownames", "quotes"),
        initialValues = c(TRUE, has_rownames, TRUE),
        labels = gettext_bs(
            c(
                "Write variable names",
                "Write row names",
                "Quotes around character values"
            )
        )
    )
    missingFrame    <- tkframe(optionsFrame)
    missingVariable <- tclVar("NA")
    missingEntry <-
        ttkentry(missingFrame, width = "8", textvariable = missingVariable)

    Rcmdr::radioButtons(
        name = "delimiter",
        buttons = c("commas", "semicolons", "spaces", "tabs"),
        labels = gettext_bs(c("Commas [,]", "Semicolons [;]", "Spaces", "Tabs")),
        initialValue = "tabs",
        title = gettext_bs("Field separator")
    )

    delimiterFrame_other <- tkframe(delimiterFrame)

    otherButton <-
        ttkradiobutton(
            delimiterFrame_other,
            variable = delimiterVariable,
            value    = "other",
            text     = gettext_bs("Other: ")
        )
    otherVariable <- tclVar("")
    otherEntry    <- ttkentry(delimiterFrame_other,
                              width = "4",
                              textvariable = otherVariable)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        closeDialog()
        col     <- tclvalue(colnamesVariable) == 1
        row     <- tclvalue(rownamesVariable) == 1
        quote   <- tclvalue(quotesVariable)   == 1
        delim   <- tclvalue(delimiterVariable)
        missing <- tclvalue(missingVariable)

        sep <-
            if (delim == "tabs") {
                "\\t"
            } else if (delim == "spaces") {
                " "
            } else if (delim == "commas") {
                ","
            } else if (delim == "semicolons") {
                ";"
            } else{
                trim.blanks(tclvalue(otherVariable))
            }

        saveFile <-
            tclvalue(tkgetSaveFile(
                title = "Save data to text file",
                filetypes = gettext_bs(
                    '{"All Files" {"*"}} {"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}}'
                ),
                defaultextension = if (delim == "commas") ".csv" else ".txt",
                initialfile = paste0(dsname, if (delim == "commas") ".csv" else ".txt"),
                parent = CommanderWindow()
            ))

        saveFile <- removeRedundantExtension(saveFile)

        if (saveFile == "") {
            tkfocus(CommanderWindow())
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command <- str_glue("## Save data to text file \n",
                            "write.table({dsname}, file = '{saveFile}', \n",
                            "sep = '{sep}', \n",
                            "col.names = {col}, \n",
                            "row.names = {row}, \n",
                            "quote = {quote}, \n",
                            "na = '{missing}')") %>%
            style_cmd()

        justDoIt(command)
        logger(command)

        Message(paste(gettext_bs("Active dataset exported to file"), saveFile),
                type = "note")
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "write.table")
    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")

    tkgrid(bs_label(
        parent = top,
        text = gettext_bs("Save data to text file"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(delimiterFrame_other, stick = "w")
    tkgrid(otherButton, otherEntry,
           sticky = "w")
    tkgrid(delimiterFrame, stick = "w")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkgrid(labelRcmdr(top,
                      text = gettext_bs("Options "),
                      fg = getRcmdr("title.color")),
           sticky = "w",
           pady = c(10, 0))

    tkgrid(labelRcmdr(missingFrame, text = gettext_bs("Missing values:  ")),
           missingEntry,
           sticky = "w")
    tkgrid(missingFrame, sticky = "w")
    tkgrid(optionsFrame, sticky = "w")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix()
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
export_to_clipboard <- function(ds_name = ActiveDataSet(), sep = ",") {
    try(
        clipr::write_clip(get(ds_name, envir = .GlobalEnv), sep = sep),
        silent = TRUE
    )
}


#' @rdname Menu-window-functions
#' @export
#' @keywords internal
export_to_clipboard_active_ds_tab <- function() {
    try(
        clipr::write_clip(get(ActiveDataSet(), envir = .GlobalEnv), sep = "\t"),
        silent = TRUE)
}

# ===========  . =================================================================
# work around for bug in Tk getSaveFile  # (Form Rcmdr)
removeRedundantExtension <- function(file) {
    find.ext <- regexpr("\\.(?:.(?!\\.))+$", file, perl = TRUE)
    if (find.ext == -1)
        return(file)
    ext  <- substring(file, find.ext, find.ext + attr(find.ext, "match.length"))
    file <- sub(paste0(ext, ext, "$"), ext, file)
    file
}
