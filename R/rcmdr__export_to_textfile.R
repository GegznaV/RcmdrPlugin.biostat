
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

    .ds <- active_dataset()
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
        buttons = c("tab", "comma", "semicolon", "space", "pipe"),
        labels = gettext_bs(c("Tab", "Comma (,)", "Semicolon (;)", "Space ( )", "Pipe (|)")),
        initialValue = "tab",
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

        sep <- switch(
            delim,
            "tab"       = "\\t",
            "comma"     = ",",
            "semicolon" = ";",
            "space"     = " ",
            "pipe"      = "|",
            trim.blanks(tclvalue(otherVariable))
        )

        # if (delim == "tabs") {
        #     "\\t"
        # } else if (delim == "spaces") {
        #     " "
        # } else if (delim == "commas") {
        #     ","
        # } else if (delim == "semicolons") {
        #     ";"
        # } else{
        #
        # }

        save_to_file <-
            tclvalue(tkgetSaveFile(
                title = "Save Data to Text File",
                filetypes = gettext_bs(
                    '{"All Files" {"*"}} {"Text Files" {".txt" ".csv"  ".dat" }}'
                ),
                defaultextension = if (delim == "commas") ".csv" else ".txt",
                initialfile = paste0(.ds, if (delim == "commas") ".csv" else ".txt"),
                parent = CommanderWindow()
            ))

        save_to_file <- removeRedundantExtension(save_to_file)

        if (save_to_file == "") {
            tkfocus(CommanderWindow())
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        command <-
            str_glue(
                "## Save data to text file \n",
                "write.table({.ds}, file = '{save_to_file}', \n",
                'sep = "{sep}", \n',
                'dec = "."\n',
                "col.names = {col}, \n",
                "row.names = {row}, \n",
                "quote = {quote}, \n",
                'na = "{missing}")'
            ) %>%
            style_cmd()

        justDoIt(command)
        logger(command)

        Message(paste(gettext_bs("Active dataset exported to file"), save_to_file),
                type = "note")
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(helpSubject = "write.table")
    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")

    bs_title(top, "Save Data to Text File")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(delimiterFrame_other,    sticky = "w")
    tkgrid(otherButton, otherEntry, sticky = "w")
    tkgrid(delimiterFrame,          sticky = "w")
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
