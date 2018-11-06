# TODO:
#  Ar reikalingos funkcijos `box_ask_to_replace`, `replace_object`,
#  `replace_variable `
#
#  ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?

# #
# #' Message box, which asks if object should be replaced
# #' @name box_ask_to_replace
# #' @param name The name of the object.
# #' @param type The type of the object.
# #' @keywords internal
# #' @return Logical value: TRUE -- to replace or FALSE -- not to replace.
# # @export
# #
# # @examples
# box_ask_to_replace <- function(name, type = gettextRcmdr("Object")) {
#     rez <- RcmdrTkmessageBox(
#         message = sprintf(
#             gettextRcmdr('%s "%s" already exists.\nOverwrite the %s?'),
#             type,
#             name,
#             tolower(type)
#         ),
#         icon = "warning",
#         type = "yesno",
#         default = "no"
#     )
#     tclvalue(rez) == "yes"
# }
# #  ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?
# #' @rdname box_ask_to_replace
# replace_object <- function(name, type = gettextRcmdr("Object")) {
#     box_ask_to_replace(name, type)
# }
# #  ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?
# #' @rdname box_ask_to_replace
# replace_variable <- function(name, type = gettextRcmdr("Variable")) {
#     box_ask_to_replace(name, type)
# }
# #  ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?
# replace_duplicated_variable <- function(name) {
#     # Checks if variable exists in active dataset
#     # Returns TRUE if:
#     #     - variable does not exist.
#     #     - variable exists but user agrees to overvrite it.
#
#     if (name %in% listVariables()) {
#         msg_box_confirm_to_replace(name, "Variable") == "yes"
#     } else {
#         TRUE
#     }
# }
# #  ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?
# replace_duplicated_obj <- function(name, envir = .GlobalEnv) {
#     # Checks if object exists in (Global) environment
#     # Returns TRUE if:
#     #     - object does not exist.
#     #     - object exists but user agrees to overvrite it.
#
#     if (name %in% listDataSets(envir = envir)) {
#         msg_box_confirm_to_replace(name, "Dataset") == "yes"
#
#     } else if (name %in% objects(envir = envir, all.names = TRUE)) {
#         msg_box_confirm_to_replace(name, "Object") == "yes"
#
#     } else {
#         TRUE
#     }
# }
# #  ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?

