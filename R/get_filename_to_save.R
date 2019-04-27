get_filename_to_save <- function(
    file_name = "",
    filetypes = "{ {All Files} * }",
    defaultextension = "",
    ...
) {

    initialfile <- extract_filename(file_name)
    initialdir  <- fs::path_dir(file_name)

    if (is.null(initialdir) || nchar(trimws(initialdir)) == 0 || initialdir == ".") {
        initialdir  <- getwd()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    file_name <-
        tkgetSaveFile(
            initialfile = initialfile,
            initialdir  = initialdir,
            filetypes   = filetypes,
            defaultextension = defaultextension,
            ...
        ) %>%
        tclvalue_chr()

    file_name
}


# # If canceled
# if (nchar(file_name) == 0) {
#     Message("Canceled. Object was not saved.", type = "warning")
#     return(NULL)
# }

# {
#
#     while (TRUE) {
#         break_cycle <- "yes"
#
#         initialfile <- extract_filename(file_name)
#         initialdir  <- fs::path_dir(file_name)
#
#         if (is.null(initialdir) || nchar(trimws(initialdir)) == 0 || initialdir == ".") {
#             initialdir  <- getwd()
#         }
#
#         # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         file_name <- tkgetSaveFile(
#             initialfile = initialfile,
#             initialdir  = initialdir,
#             filetypes   = filetypes,
#             defaultextension = "txt"
#             # , ...
#         ) %>%
#             tclvalue_chr()
#         # %>%
#         #     fs_path_ext_set(ext)
#
#         # If canceled
#         if (nchar(file_name) == 0) {
#             Message("Canceled. Object was not saved.", type = "warning")
#             return(NULL)
#         }
#
#         if (forbid_to_replace_file(file_name)) {
#             break_cycle <- "no"
#         }
#
#         # Exit the cycle, if everything is selected correctly
#         if (break_cycle == "yes") {
#             break
#         }
#         file_name
#     }
#
# }
