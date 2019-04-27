window_load_packages <- function() {

    availablePackages <- sort(setdiff(.packages(all.available = TRUE), .packages()))

    if (length(availablePackages) == 0) {
        errorCondition(message = gettextRcmdr("No packages available to load."))
        return()
    }

    initializeDialog(title = gettextRcmdr("Load Packages"))

    packagesBox <- bs_listbox(
        top,
        availablePackages,
        title = gettextRcmdr("Available packages"),
        use_filter = TRUE,
        height = 10,
        width = c(25, Inf),
        filter_label = "Filter by name",
        selectmode = "multiple"
    )

    # selected_box <- bs_listbox(
    #     top,
    #     "",
    #     # availablePackages,
    #     title = gettextRcmdr("Selected packages"),
    #     use_filter = FALSE,
    #     height = 10,
    #     width = c(25, Inf),
    #     # filter_label = "Filter by name",
    #     selectmode = "multiple"
    # )

    tkgrid(packagesBox$frame, sticky = "nw")
    # tkgrid(packagesBox$frame, selected_box$frame, sticky = "nw")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {
        packages <- getSelection(packagesBox)
        closeDialog(top)
        if (length(packages) == 0) {
            errorCondition(recall = loadPackages,
                           message = gettextRcmdr("You must select at least one package."))
            return()
        }
        for (package in packages) {
            Library(package)
        }
        Message(paste(gettextRcmdr("Packages loaded:"),
                      paste(packages, collapse = ", ")),
                type = "note")
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(helpSubject = "library")
    tkgrid(buttonsFrame, sticky = "we", columnspan = 2)
    dialogSuffix()
}