
# Import dataset =============================================================
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# window_import_rds <- function() {
#     while (TRUE) {
#         break_cycle <- "yes"
#
#         # Choose file name
#         file_name <- tclvalue(tkgetOpenFile(
#             title = "Choose Rds file to import",
#             multiple = FALSE,
#             filetypes = "{ {R data object} {.RDS .Rds .rds} } { {All Files} * }"))
#
#         # Choose variable name
#         object_name <- make.names(extract_filename(file_name))
#
#         object_name <-
#             enter_info_window(
#                 title    = "Import Rds object",
#                 text_1a  = "Filename: ",
#                 text_1b  = stringr::str_trunc(file_name,
#                                               width = 60,
#                                               side = "center",
#                                               ellipsis = " ... "),
#                 text_2a  = "Name of your object: ",
#                 entry_2b = object_name,
#                 text_2c  = "",
#                 entryWidth = 40,
#                 returnValOnCancel = NULL,
#                 returnValOnChose  = NA
#             )
#
#         # If canceled
#         if (is.null(object_name)) {
#             Message("Operation to read .Rds file was canceled.",
#                     type = "warning")
#             return()
#         }
#
#         # If requested for a new filename
#         if (is.na(object_name)) {
#             # Choose a new file name
#             next
#         }
#
#         # Make a valid variable name
#         object_name <- make.names(object_name)
#
#         # Check if an object with the same name exists
#         if (object_name %in% ls())
#             break_cycle <- tclvalue(checkReplace(
#                 str_glue('"{object_name}"'), type = "Object"))
#
#         # Exit the cycle, if everything is selected correctly
#         if (break_cycle == "yes") {
#             break
#         }
#     } # END: Choose variable name
#
#
#     # Change these lines [!!!]: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     make_relative = TRUE
#
#     if (make_relative) {
#         file_name <- make_relative_path(file_name)
#     }
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     command <- str_glue('{object_name} <- readRDS("{file_name}")')
#     doItAndPrint(command)
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     if (inherits(get(object_name, envir = .GlobalEnv), "data.frame"))
#         ActiveDataSet(object_name)
#     # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
dataset_import_excel <- function() {
    function_not_implemented("Import from excel function")
    return()


    file_filters <-
        matrix(ncol = 2, byrow = TRUE,
               c("Excel files (*.xlsx, *.xls)", "*.xlsx;*.xls",
                 "All files (*.*)",             "*.*" )
        )

    # "Select Excel file"

}
