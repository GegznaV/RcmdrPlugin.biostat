# TODO:
# 1. Simplify code of this file;
# 2

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_rows_slice <- function(){
    dataSet <- activeDataSet()

    initializeDialog(title = gettextRcmdr("Select / Remove rows by position"))

    indexVariable <- tclVar(gettextRcmdr(""))
    indexFrame <- tkframe(top)
    indexEntry <- ttkentry(indexFrame, width = "60", textvariable = indexVariable)

    indexScroll <- ttkscrollbar(
        indexFrame,
        orient = "horizontal",
        command = function(...)
            tkxview(indexEntry, ...)
    )
    tkconfigure(
        indexEntry,
        xscrollcommand = function(...)
            tkset(indexScroll, ...)
    )
    newDataSetName <- tclVar(unique_df_name(suffix = "_subset", all_numbered = TRUE))

    dataSetNameFrame <- tkframe(top)
    dataSetNameEntry <-
        ttkentry(dataSetNameFrame, width = "36", textvariable = newDataSetName)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        new_dsname <- trim.blanks(tclvalue(newDataSetName))
        index      <- tclvalue(indexVariable)

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (!is.valid.name(new_dsname)) {
            errorCondition(
                recall = window_rows_slice,
                message = paste0( '"',new_dsname,'" ', gettextRcmdr("is not a valid name."))
            )
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is.element(new_dsname, listDataSets())) {
            if ("no" == tclvalue(checkReplace(new_dsname,
                                              type = gettextRcmdr("Data set")))) {
                closeDialog()
                window_rows_slice()
                return()
            }
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (index == "") {
            errorCondition(recall = window_rows_slice,
                           message = "No rows to select/remove")
            closeDialog()
            return()
        }

        # indexRows <- paste0("c(", gsub(" ", ",", index), ")")
        # index <- try(eval_glue(indexRows), silent = TRUE)

        if (class(index) == "try-error") {
            errorCondition(recall = window_rows_slice,
                           message = index)
            closeDialog()
            return()
        }

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("dplyr")

        # If multiple comma separated conditions are selected
        if (stringr::str_detect(index, ",")) {
            index <-  glue("c({index})")
        }

        command <- str_glue(
            "## ", gettext_Bio("Select/Remove rows by index"), "\n",
            "{new_dsname} <- {activeDataSet()} %>% \n",
            "dplyr::slice({index})") %>%
            style_cmd()

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        logger(command)
        result <- justDoIt(command)

        if (class(result)[1] !=  "try-error")
            activeDataSet(new_dsname)

        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp(helpSubject = "slice", helpPackage = "dplyr")

    # Title ------------------------------------------------------------------
    fg_col <- Rcmdr::getRcmdr("title.color")
    tkgrid(label_rcmdr(
        top,
        text = gettextRcmdr("Slice: select/remove rows by index"),
        font = tkfont.create(weight = "bold", size = 9),
        fg = fg_col),
        pady = c(5, 9))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    row_number_frame <- tkframe(top)
    tkgrid(row_number_frame)
    tkgrid(
        label_rcmdr(row_number_frame, text = "Number of rows in the dataset: "),
        label_rcmdr(row_number_frame, text = nrow(get(activeDataSet())),
                    fg = "darkred"),
        sticky = "sw"
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(
        label_rcmdr(
            indexFrame,
            text = gettextRcmdr("Row indices:"),
            foreground = getRcmdr("title.color"),
            font = "RcmdrTitleFont"
        ),
        sticky = "w"
    )

    tkgrid(indexEntry,  sticky = "w")
    tkgrid(indexScroll, sticky = "ew")
    tkgrid(indexFrame,  sticky = "w")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(labelRcmdr(dataSetNameFrame,
                      text = gettextRcmdr("Name for sliced dataset:   ")),
           dataSetNameEntry,
           sticky = "w")

    # tkgrid(dataSetNameEntry, sticky = "w")
    tkgrid(dataSetNameFrame, sticky = "w")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(
        labelRcmdr(
            top,
            text = paste0(
                gettextRcmdr("Row indices should be:\n"),
                gettextRcmdr(" - comma separated;\n"),
                gettextRcmdr(" - either positive integers to select rows;\n"),
                gettextRcmdr(" - or negative integers to remove rows.\n"),
                gettextRcmdr("Use colon to select ranges from:to. Function n() indicates the last row.\n")
            ),
            foreground = getRcmdr("title.color"),
            font = "RcmdrTitleFont"
        ),
        sticky = "sw",
        pady = c(10, 0)
    )

    tkgrid(
        label_rcmdr(
            top,
            text = paste0(
                "Example 1 (select): 1, 3, 19:52, n()\n",
                "Example 2 (remove): -1, -9, -21:-n()")
        ),

        sticky = "nw"
    )

    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
}



#
# window_rows_slice__ <- function() {
#     Library("tidyverse")
#
#     doItAndPrint(str_glue(
#
#         '\n# Select the first row: \n',
#         '# new_df <- dplyr::slice({ActiveDataSet()}, 1) \n',
#         '\n# Select the last row: \n',
#         '# new_df <- dplyr::slice({ActiveDataSet()}, n()) \n',
#         '\n# Select several adjacent rows: \n',
#         '# new_df <- dplyr::slice({ActiveDataSet()}, 5:n()) \n',
#         '\n# Use negative indices to drop rows: \n',
#         '# new_df <- dplyr::slice({ActiveDataSet()}, -5:-n()) \n'
#
#     ))
#
# }