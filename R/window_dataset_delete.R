#' command_new_dataset
#'
#' Code of this function is taken from `Rcmdr` package and slightly modified.
#'
#' @export
#' @keywords internal
window_dataset_delete <- function() {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Window to choose dataset's name

    initializeDialog(title = gettextRcmdr("Delete Active Dataset"))

    ds_to_delete <- activeDataSet()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        closeDialog()

        ActiveDataSet(NULL)
        command <- str_glue("## Delete the dataset \n",
                            "remove({ds_to_delete})")
        res <- doItAndPrint(command)

        if (!inherits(res, "try-error")) {
            Message(glue('Dataset "{ds_to_delete}" was deleted.'),
                    type = "warning")
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkfocus(CommanderWindow())
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OKCancelHelp()

    tkgrid(labelRcmdr(top,
                      fg = getRcmdr("title.color"),
                      text = gettextRcmdr("Do you want to DELETE the following dataset? ")),

           sticky = "w", pady = c(10, 0), padx = 25)

    tkgrid(labelRcmdr(top, fg = "red", text = ds_to_delete),

           sticky = "", pady = c(0, 10))

    tkgrid(buttonsFrame, columnspan = "2", sticky = "")
    dialogSuffix()
}
