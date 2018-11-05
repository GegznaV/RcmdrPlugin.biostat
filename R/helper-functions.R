# Imported functions =========================================================
str_c    <- stringr::str_c
str_glue <- stringr::str_glue
glue     <- str_glue

# Other functions ============================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
gettext_EZR <- function(...) {
    gettext(domain = "R-RcmdrPlugin.EZR", ...)
}

# ------------------------------------------------------------------------------
# Make a unique name for an object (e.g., data frame) by adding numbers
#
# @param name - name of dataset before suffix and preffix are added.
unique_obj_names <- function(names = ActiveDataSet(),
                             preffix = "",
                             suffix = "",
                             list_of_choices = objects(all.names = TRUE,
                                                       envir = .GlobalEnv),
                             all_numbered = FALSE) {
    initial_names <- str_glue("{preffix}{names}{suffix}")

    n_names <- length(names)

    list_to_check <-
        if (all_numbered) {
            c(list_of_choices, initial_names, initial_names)

        } else {
            c(list_of_choices, initial_names)
        }

    list_to_check %>%
        make.unique(sep = "_") %>%
        rev() %>%
        .[1:n_names] %>%    # select the last elements
        rev()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unique_df_name <- function(names = ActiveDataSet(),
                           preffix = "",
                           suffix = "",
                           list_of_choices = objects(all.names = TRUE, envir = .GlobalEnv),
                           all_numbered = FALSE) {

    unique_obj_names(names, preffix, suffix, list_of_choices, all_numbered)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unique_colnames <- function(names = "",
                            preffix = "",
                            suffix = "",
                            list_of_choices = listVariables(),
                            all_numbered = FALSE) {

    unique_obj_names(names, preffix, suffix, list_of_choices, all_numbered)
}
unique_colnames_2 <- function(names = "",
                              preffix = "",
                              suffix = "",
                              list_of_choices = listVariables(),
                              all_numbered = TRUE) {

    unique_obj_names(names, preffix, suffix, list_of_choices, all_numbered)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
variables_with_unique_values <- function() {

    ds <- get(activeDataSet(), envir = .GlobalEnv)
    not_duplicated_cols <- purrr::map_lgl(ds, ~!any(duplicated(.)))
    (not_duplicated_cols[not_duplicated_cols == TRUE]) %>%
        names() %>%
        sort()
}
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
variables_with_unique_values_P <- function(n = 1) {

    activeDataSetP() && length(variables_with_unique_values() >= n)
}


# ------------------------------------------------------------------------------
# Formatat code in a `tidyverse` style
style_cmd <- function(command, indent_by = 4, ...) {
    cmd <- styler::style_text(command, indent_by = indent_by, ...)
    paste0(as.character(cmd), collapse = "\n")
}
# ------------------------------------------------------------------------------
list_summaries_Models <- function(envir = .GlobalEnv, ...) {
    objects <- ls(envir = envir, ...)
    if (length(objects) == 0)
        NULL
    else objects[sapply(objects,
                        function(.x) "summaries_model" == (class(get(.x, envir = envir))[1]))]
}

# ------------------------------------------------------------------------------
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
function_not_implemented <- function() {

    x = NULL
    doItAndPrint("# ~~~ Not implemented yet! ~~~\n")

    if (is.null(x)) {
        x <- "This function"
    }

    text <- str_glue("# ~~~ {x} will be implemented  \n ",
                     "# ~~~ in the future versions of package `RcmdrPlugin.biostat`! ")

    msg <- str_glue("{x} will be implemented in the future versions of package",
                    " `RcmdrPlugin.biostat`! ")

    doItAndPrint(text)
    Message(msg, type = "warning")
}
# ------------------------------------------------------------------------------
spaces <- function(n) {
    paste0(rep(" ", length = n), collapse = "")
}
# ------------------------------------------------------------------------------
nonFactorsP <- function(n = 1) {
    #  n - number of non-factors.
    activeDataSetP() && length(setdiff(listVariables(), listFactors())) >= n
}
# ------------------------------------------------------------------------------
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
class_ggplot_P <- function(n = 1) {
    #  n - number of ggplot objects.
    length(objects_of_class("ggplot", envir = .GlobalEnv)) >= n
}

# ------------------------------------------------------------------------------
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
objects_in_env_P <- function(n = 1, envir = .GlobalEnv, ...) {
    #  n - number of objects.
    isTRUE(length(objects(envir = envir, ...)) >= n)
}
# ------------------------------------------------------------------------------
eval_glue <- function(..., envir = parent.frame(),
                      .sep = "", .open = "{", .close = "}",
                      envir_eval = envir,
                      envir_glue = envir) {

    x2 <- str_glue(..., .envir = envir_glue, .open = .open, .close = .close)
    eval(parse(text = x2), envir = envir_eval)
}
# ------------------------------------------------------------------------------
eval_ <- function(x, envir = parent.frame(), ...) {
    eval(parse(text = x), envir = envir, ...)
}
# ------------------------------------------------------------------------------
gettext_Bio <- function(...) {
    gettext(..., domain = "R-RcmdrPlugin.biostat")
}
# ------------------------------------------------------------------------------
#' Does active dataset contain characters?
#'
#' Return TRUE, if at least n character variables exist in the active dataset.
#'
#' @param n Minimum number of character variables
#'
#' @keywords internal
#' @export
characterP <- function(n = 1) {
    activeDataSetP() &&
        (sum(eval_glue("mapply(is.character, {activeDataSet()})")) >= n)
}
# ------------------------------------------------------------------------------
#' Does active dataset contain true factors?
#'
#' Return TRUE, if at least n factor variables exist in the active dataset.
#' Function `Factors()` list factor-like data including characters and logicals.
#'
#' @param n Minimum number of logical variables
#'
#' @keywords internal
#' @export
factors_strict_P <- function(n = 1) {
    activeDataSetP() &&
        (sum(eval_glue("mapply(is.factor, {activeDataSet()})")) >= n)
}
# ------------------------------------------------------------------------------
#' Does active dataset contain logicals?
#'
#' Return TRUE, if at least n locical variables exist in the active dataset.
#'
#' @param n Minimum number of logical variables
#'
#' @keywords internal
#' @export
logicalP <- function(n = 1) {
    activeDataSetP() &&
        (sum(eval_glue("mapply(is.logical, {activeDataSet()})")) >= n)
}
#' Character variable names in active dataset
#'
#' @keywords internal
#' @export
variables_chr <- function() {
    objects_of_class("character",
                     envir = as.environment(globalenv()[[activeDataSet()]]))
}
#' Logical variable names in active dataset
#'
#' @keywords internal
#' @export
variables_lgl <- function() {
    objects_of_class("logical",
                     envir = as.environment(globalenv()[[activeDataSet()]]))
}
#' True factor variables names in active dataset
#'
#' @keywords internal
#' @export
variables_fct <- function() {
    objects_of_class("factor",
                     envir = as.environment(globalenv()[[activeDataSet()]]))
}
#' ...
#'
#' @keywords internal
#' @export
var_pos_n <- function(variables,
                      type = c("all",
                               "numeric",
                               "factor",
                               "factor_strict",
                               "twoLevelFactor",
                               "nonfactor",
                               "character",
                               "logical"
                      ), vars = NULL)
{
    if (is.null(variables))
        return(NULL)
    type <- match.arg(type)
    if (is.null(vars))
        vars <- switch(
            type,
            all           = Variables(),
            character     = variables_chr(),
            logical       = variables_lgl(),
            factor_strict = variables_fct(),
            factor        = Factors(),
            numeric       = Numeric(),
            nonfactor     = setdiff(Variables(),
                                    Factors()), twoLevelFactor = TwoLevelFactors())
    if (any(!variables %in% vars))
        NULL
    else apply(outer(variables, vars, "=="), 1, which) - 1
}


# ------------------------------------------------------------------------------
#' Does dataset contain certain number of variables?
#'
#' Return TRUE, if at least n variables exist in the active dataset.
#'
#' @param n Minimum number of character variables
#'
#' @keywords internal
#' @export
variablesP <- function(n = 1) {
    activeDataSetP() && length(listVariables()) >= n
}
# ------------------------------------------------------------------------------

#
# ------------------------------------------------------------------------------
#' Is the first class "data.frame"
#'
#' Check if the first class of active Rcmdr dataset is "data.frame"
#' @keywords internal
#' @export
first_class_is_dataframeP <- function() {
    activeDataSetP() &&
        (eval_glue("class({activeDataSet()})[1]") == "data.frame")
}
#' [!] Is the first class the same as in brackets?
#'
#' Check if the first class of active Rcmdr dataset is the same as determined.
#' @keywords internal
#' @export
first_class_isP <- function(df_class) {
    activeDataSetP() &&
        (eval_glue("class({activeDataSet()})[1]") == df_class)
}
# ------------------------------------------------------------------------------
#' Chech the class of the active model in Rcmdr
#'
#' @param class_ (string) a character vector of length 1. The name of class.
#'
#' @keywords internal
#' @export
modelClassP <- function(class_) {
    activeModelP() && (inherits(
        x = get(ActiveModel(), envir = .GlobalEnv),
        what = class_))
}
# ------------------------------------------------------------------------------
#' Make path to relative
#'
#' Make absolute path into relative one in respect to current working directory.
#'
#' @param str (character) Sting (or vector of strings) with absolute path.
#'
#' @keywords internal
#' @export
make_relative_path <- function(str) {
    sub(paste0(getwd(), "/?"), "", str)
}

#' Extract file parts.
#' @name extract-fileparts
#' @param str (character) Path to file (with filename and extension).
#'
#' @keywords internal
#' @export
extract_filename <- function(str) {
    sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", str)
}
#' @rdname extract-fileparts
#' @keywords internal
#' @export
extract_path <- function(str) {
    sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\1", str)
}
#' @rdname extract-fileparts
#' @keywords internal
#' @export
extract_extension <- function(str) {
    sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\3", str)
}
#' @rdname extract-fileparts
#' @keywords internal
#' @export
clean_str <- function(str, ...) {
    snakecase::to_any_case(make.names(str), ...)
}


# Path =======================================================================
# path <- file.path("c:", "p1p1p1p1p1", "p2p2p2p2p2p2", "p3p3p3p3p3p3p3", "file2.xlsx")
#                      # "c:/p1p1p1p1p1/p2p2p2p2p2p2/p3p3p3p3p3p3p3/file2.xlsx"
# path_truncate(path)  # --->  "c:/p1p1p1p1p1/ ... /file2.xlsx"
path_truncate <- function(path, max_length = 30) {
    path <- normalizePath(path, winslash = "/")

    if (str_length(path) <= max_length) {
        show_trunc <- path
    } else {
        path_parts <- str_split(path, "/")[[1]]
        last_ind <- length(path_parts)
        legths <- path_parts %>% map_int(str_length)
        lengths2 <-
            cumsum(c(legths[last_ind], legths[-last_ind])) + 5 # 5 is length of " ... "
        add_parts <- max(which(lengths2 <= max_length)) - 1 # -1 is minus the last one
        add_parts <- max(1, add_parts)
        show_trunc <-
            file.path(str_c(path_parts[1:add_parts], collapse = "/"),
                      " ... ",
                      path_parts[last_ind])
    }

show_trunc
}
