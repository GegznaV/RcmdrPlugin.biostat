
# This function is based on function `onEdit` from package `Rcmdr` 2.5-1

window_dataset_edit_rcmdr <- function() {
    .ds <- active_dataset_0()

    if (is.null(.ds)) {
        active_dataset_not_persent()
        tkfocus(CommanderWindow())
        return()
    }


    size <- eval(parse(text = paste("prod(dim(", .ds, "))", sep = ""))) #  prod(dim(save.dataset))
    if (size < 1 || size > getRcmdr("editDataset.threshold")) {
        save.dataset <- get(.ds, envir = .GlobalEnv)
        command <- paste("fix(", .ds, ")", sep = "")
        result <- justDoIt(command)
        if (class(result)[1] != "try-error") {
            if (nrow(get(.ds)) == 0) {
                errorCondition(window = NULL,
                               message = gettextRcmdr("empty data set."))
                justDoIt(paste(.ds, "<- save.dataset"))
                return()
            }
            else {
                logger(command, rmd = FALSE)
                active_dataset(.ds)
            }
        }
        else {
            errorCondition(window = NULL, message = gettextRcmdr("data set edit error."))
            return()
        }
    }
    else {
        command <- paste("editDataset(", .ds, ")", sep = "")
        result <- justDoIt(command)

        if (class(result)[1] != "try-error") {
            logger(command, rmd = FALSE)

        } else {
            errorCondition(window = NULL, message = gettextRcmdr("data set edit error."))
            return()
        }
    }
    tkwm.deiconify(CommanderWindow())
    tkfocus(CommanderWindow())
}