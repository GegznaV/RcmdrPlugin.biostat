#' @name Menu-winow-functions
#' @title RcmdrPlugin.BioStat functions for menus and windows
#' @description Functions that open Rcmdr menus and windows windows
#' @keywords internal
NULL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Dataset" menu related functions ===========================================

# Manageme dataset -----------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
command_dataset_refresh <- function() {
    Rcmdr::activeDataSet(Rcmdr::ActiveDataSet())
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
command_dataset_class <- function() {
    doItAndPrint(glue::glue("class({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
command_dataset_as_df <- function() {
    doItAndPrint(glue::glue("{ActiveDataSet()} <- as.data.frame({ActiveDataSet()})"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Import dataset =============================================================
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_import_rds <- function() {
    while (TRUE) {
        break_cycle <- "yes"

        # Choose file name
        file_name <- tclvalue(tkgetOpenFile(
            title = "Choose an R data object file",
            multiple = FALSE,
            filetypes = "{ {R data object} {.RDS .Rds .rds} } { {All Files} * }"))

        # Choose variable name
        object_name <- make.names(extract_filename(file_name))

        object_name <-
            window_enter_info(
                title    = "Import Rds object",
                text_1a  = "Filename: ",
                text_1b  = stringr::str_trunc(file_name,
                                              width = 60,
                                              side = "center",
                                              ellipsis = " ... "),
                text_2a  = "Name of your object: ",
                entry_2b = object_name,
                text_2c  = "",
                entryWidth = 40,
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
        if (object_name %in% ls())
            break_cycle <- tclvalue(checkReplace(
                glue::glue('"{object_name}"'), type = "Object"))

        # Exit the cycle, if everything is selected correctly
        if (break_cycle == "yes") {
            break
        }
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
       ActiveDataSet(object_name)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
dataset_import_excel <- function() {
    Message("This fuction will work in future versions of the package.",
            type = "warning")

    file_filters <-
        matrix(ncol = 2, byrow = TRUE,
               c("Excel files (*.xlsx, *.xls)", "*.xlsx;*.xls",
                 "All files (*.*)",             "*.*" )
        )

    # "Select Excel file"

}

# Export dataset =============================================================
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_export_as_textfile <- function() {
    Message("This fuction will work in future versions of the package.",
            type = "warning")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-winow-functions
#' @export
#' @keywords internal
window_export_as_rds <- function() {
    file_name <- ""
    while (TRUE) {
        break_cycle <- "yes"

        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        file_name <- tclvalue(tkgetSaveFile(
            initialfile =
                if (nchar(trimws(file_name)) > 0) {
                    extract_filename(file_name)
                } else {
                    activeDataSet()
                },
            filetypes = "{ {Rds file} {.Rds .RDS .rds} } { {All Files} * }"))


        # If canceled
        if (nchar(trimws(file_name)) == 0) {
            Message("Operation canceled, object was not saved.",
                    type = "warning")
            return()
        }

        # Add extension if missing
        if (!grepl("\\.[Rr][Dd][Ss]$", file_name)) {
            # Add extension
            file_name <- paste0(file_name, ".Rds")

            # Check if a file with the same name exists
            if (file.exists(file_name))
                break_cycle <- tclvalue(Rcmdr::checkReplace(
                    glue::glue('"{file_name}"'), type = "File"))
        }

        # Exit the cycle, if everything is selected correctly
        if (break_cycle == "yes") {
            break
        }
    }



    # Change these lines: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    make_relative = TRUE

    if (make_relative) {
        file_name <- make_relative_path(file_name)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    command <- glue::glue('saveRDS({activeDataSet()}, file = "{file_name}")')
    doItAndPrint(command)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
window_export_as_excel <- function() {
    Message("This fuction will work in future versions of the package.",
            type = "warning")


}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

