
# This function is based on function `onView` from package `Rcmdr` 2.5-1

window_dataset_view_rcmdr <- function() {
    .ds <- ActiveDataSet()

    if (is.null(.ds)) {
        active_dataset_not_persent()
        tkfocus(CommanderWindow())
        return()
    }

    suppress <-
        if (getRcmdr("suppress.X11.warnings")) {
            ", suppress.X11.warnings=FALSE"
        } else {
            ""
        }

    view.height <- max(getRcmdr("output.height") + getRcmdr("log.height"), 10)
    view.width  <- getRcmdr("log.width")


    dims <- dim(get(.ds, envir = .GlobalEnv))
    nrows <- dims[1]
    ncols <- dims[2]

    threshold <- getRcmdr("showData.threshold")

    command <-
        if (nrows <= threshold[1] && ncols <= threshold[2]) {

            str_glue("showData(as.data.frame({.ds}), ",
                     "    placement = '-20+200', ",
                     "    font      = getRcmdr('logFont'), ",
                     "    maxwidth  = {view.width}, ",
                     "    maxheight = {view.height}",
                     "    {suppress})",
            )

        } else {
            tk_messageBox(
                # parent  = CommanderWindow(),
                message = str_glue(
                    "The size of the active dataset is {dims[1]} rows and {dims[2]} columns.\n",
                    "Datasets that have more than {threshold[1]} rows and {threshold[2]} columns \n",
                    "cannot be displayed in Rcmdr style window. \n",
                ),
                caption = "Dataset is Too Big to View",
                type    = "ok",
                icon    = "warning")
            tkfocus(CommanderWindow())
            return()
        }

    window <- justDoIt(command)

    if (!is.null(window)) {
        open.showData.windows <- getRcmdr("open.showData.windows")
        open.window           <- open.showData.windows[[.ds]]
        if (!is.null(open.window)) tkdestroy(open.window)
        open.showData.windows[[.ds]] <- window
        putRcmdr("open.showData.windows", open.showData.windows)
    }
}