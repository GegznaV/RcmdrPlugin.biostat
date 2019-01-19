
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
