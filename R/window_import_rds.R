# TODO:
# 1. Create new type of UI window.
# 2. Change the algorithm inside the function.
# 3. Add radiobuttons: Path: *Relative  *Absolute

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_import_rds <- function() {
    initial_dir <- getwd()

    while (TRUE) {
        break_cycle <- "yes"
        file_name   <- ""

        # Choose file name
        file_name_variable <-
            tkgetOpenFile(
                initialdir  = initial_dir,
                initialfile = file_name,
                title = "Choose Rds file to import",
                multiple = FALSE,
                filetypes = gettext_Bio(
                    "{ {Rds data file} {.RDS .Rds .rds} } { {All Files} * }"
                    )
            )

        file_name <- tclvalue(file_name_variable)

        # If cancelled
        if (file_name == "") {
            return()
        }

        # Choose variable name
        object_name <-
            unique_obj_names(make.names(extract_filename(file_name)))

        object_name <-
            enter_info_window(
                title    = "Import Rds object",
                text_1a  = "Filename: ",
                text_1b  = stringr::str_trunc(file_name,
                                              width = 60,
                                              side = "center",
                                              ellipsis = " ... "),
                text_2a  = "Name of your object: ",
                entry_2b = object_name,
                text_2c  = "",
                entryWidth = 60,
                returnValOnCancel = NULL,
                returnValOnChose  = NA
            )


        # If canceled
        if (is.null(object_name)) {
            Message("Operation to read .Rds file was canceled.",
                    type = "warning")
            return()
        }

        # If requested for a new filename
        if (is.na(object_name)) {
            # Choose a new file name
            next
        }

        # Make a valid variable name
        object_name <- make.names(object_name)

        # Check if an object with the same name exists
        if (object_name %in% ls(envir = .GlobalEnv))
            break_cycle <- tclvalue(
                checkReplace(
                    name = glue::glue('"{object_name}"'),
                    type = "Object")
                )

        # Exit the cycle, if everything is selected correctly
        if (break_cycle == "yes") {
            break
        }

        # Update initial dir
        initial_dir <- extract_path(file_name)

    } # END: Choose variable name


    # Change these lines [!!!]: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    make_relative = TRUE

    if (make_relative) {
        file_name <- make_relative_path(file_name)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    command <- glue::glue('{object_name} <- readRDS("{file_name}")')
    doItAndPrint(command)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (inherits(get(object_name, envir = .GlobalEnv), "data.frame"))
        activeDataSet(object_name)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}