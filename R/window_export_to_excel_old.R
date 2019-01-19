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