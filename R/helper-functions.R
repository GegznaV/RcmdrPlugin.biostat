#' @name Helper-functions
#' @title Helper functions for RcmdrPlugin.biostat.
#' @description Helper functions for package \pkg{RcmdrPlugin.biostat}.
#' @keywords internal
NULL

# ___ List variables  ___ ====================================================
#' @rdname Helper-functions
#' @export
#' @keywords internal
# Get contents of active dataset
get_active_ds <- function() {
    globalenv()[[activeDataSet()]]
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
list_objects_of_class <- function(
    class = NULL, all.names = FALSE,  envir = parent.frame()) {

    checkmate::assert_character(class, null.ok = TRUE)

    all_variable_names <- objects(envir, all.names = all.names)

    if (length(all_variable_names) == 0 || is.null(class)) {
        return(all_variable_names)
    } else {
        # Object names of class to return
        mget(all_variable_names, envir = envir) %>%
            purrr::keep(~inherits(.x, class)) %>%
            names()
    }
}



#' All variable names in active dataset
#'
#' @keywords internal
#' @export
variables_all <- function() {
    Variables()
}
#' Character variable names in active dataset
#'
#' @keywords internal
#' @export
variables_chr <- function() {
    list_objects_of_class("character", envir = as.environment(get_active_ds()))
}
#' Logical variable names in active dataset
#'
#' @keywords internal
#' @export
variables_lgl <- function() {
    list_objects_of_class("logical", envir = as.environment(get_active_ds()))
}
#' True factor variable names in active dataset
#'
#' @keywords internal
#' @export
variables_fct <- function() {
    list_objects_of_class("factor", envir = as.environment(get_active_ds()))

}
#' Factor-like variable names in active dataset
#'
#' @keywords internal
#' @export
variables_fct_like <- function() {
    Factors()
}
#' Non-factor-like variable names in active dataset
#'
#' @keywords internal
#' @export
variables_non_fct_like <- function() {
    setdiff(Variables(), Factors())
}
#' Two-level factor names in active dataset
#'
#' @keywords internal
#' @export
variables_fct_2_lvls <- function() {
    TwoLevelFactors()
}
#' Numeric variable names in active dataset
#'
#' @keywords internal
#' @export
variables_num <- function() {
    Numeric()
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
                               "logical"),
                      vars = NULL)
{
    if (is.null(variables))
        return(NULL)
    type <- match.arg(type)
    if (is.null(vars))
        vars <- switch(
            type,
            all            = Variables(),
            character      = variables_chr(),
            logical        = variables_lgl(),
            factor_strict  = variables_fct(),
            factor         = Factors(),
            numeric        = Numeric(),
            nonfactor      = setdiff(Variables(), Factors()),
            twoLevelFactor = TwoLevelFactors())
    if (any(!variables %in% vars))
        NULL
    else apply(outer(variables, vars, "=="), 1, which) - 1
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
list_summaries_Models <- function(envir = .GlobalEnv, ...) {
    objects <- ls(envir = envir, ...)
    if (length(objects) == 0)
        NULL
    else objects[sapply(objects, function(.x)
        "summaries_model" == (class(get(.x, envir = envir))[1]))]
}



# ___ Code ___ ===============================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
eval_glue <- function(..., envir = parent.frame(),
                      .sep = "", .open = "{", .close = "}",
                      envir_eval = envir,
                      envir_glue = envir) {

    x2 <- str_glue(..., .envir = envir_glue, .open = .open, .close = .close)
    eval(parse(text = x2), envir = envir_eval)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
# This function will deprecate as it is the same as `eval_text`. [!!!]
eval_ <- function(x, envir = parent.frame(), ...) {
    eval(parse(text = x), envir = envir, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
# The same as `eval_`
eval_text <- function(x, envir = parent.frame(), ...) {
    eval(parse(text = x), envir = envir, ...)
}


# Formatat code in a `tidyverse` style ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
style_cmd <- function(command, indent_by = 4, ...) {
    cmd <- styler::style_text(command, indent_by = indent_by, ...)
    paste0(as.character(cmd), collapse = "\n")
}

# ___ Translate ___ ==========================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
gettext_EZR <- function(...) {
    gettext(domain = "R-RcmdrPlugin.EZR", ...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
gettext_Bio <- function(...) {
    gettext(..., domain = "R-RcmdrPlugin.biostat")
}


# ___ Text ___ ===============================================================
#' @rdname Helper-functions
#' @export
#' @keywords internal
s2u <- function(str) {
    gsub(" ", "_", str)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
u2s <- function(str) {
    gsub("_", " ", str)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
spaces <- function(n, symbol = " ") {
    paste0(rep(symbol, length = n), collapse = "")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
# Print code if code evaluation error occured
logger_error <- function(command) {

    logger(str_c("#### START (code with error) ", spaces(30, "-")), rmd = FALSE)
    rez <- logger(str_c("   #   ", str_split(command,"\n")[[1]],
                        collapse = "\n"), rmd = FALSE)
    logger(str_c("#### END (code with error)   ", spaces(30, "-")), rmd = FALSE)
    invisible(rez)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
tk_label_blue <- function(...) {
    label_rcmdr(..., foreground = getRcmdr("title.color"))
}


# ___ Vectors  ___ ===========================================================
#' @rdname Helper-functions
#' @export
#' @keywords internal
# @examples swap(1:5, 2, 4)
swap <- function(x, i, j) {
    x[c(i, j)] <- x[c(j, i)]
    x
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
swap_rows <- function(x, i, j) {
    tmp <- str_split(x, "\n")[[1]]
    tmp <- swap(tmp, i, j)
    str_c(tmp, collapse = "\n")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
correct_row_index <- function(i, n_max) {
    # Make a valid row index: between 1 and maximum (n)
    if (i < 1) 1 else if (i > n_max) n_max else i
}

# ___ Names ___ ==============================================================

#' @rdname helper-functions
#' @keywords internal
#' @export
clean_str <- function(str, ...) {
    snakecase::to_any_case(make.names(str), ...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
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
#' @rdname Helper-functions
#' @export
#' @keywords internal
unique_df_name <- function(names = ActiveDataSet(),
                           preffix = "",
                           suffix = "",
                           list_of_choices = objects(all.names = TRUE, envir = .GlobalEnv),
                           all_numbered = FALSE) {

    unique_obj_names(names, preffix, suffix, list_of_choices, all_numbered)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
unique_colnames <- function(names = "",
                            preffix = "",
                            suffix = "",
                            list_of_choices = listVariables(),
                            all_numbered = FALSE) {

    unique_obj_names(names, preffix, suffix, list_of_choices, all_numbered)
}
#' @rdname Helper-functions
#' @export
#' @keywords internal
unique_colnames_2 <- function(names = "",
                              preffix = "",
                              suffix = "",
                              list_of_choices = listVariables(),
                              all_numbered = TRUE) {

    unique_obj_names(names, preffix, suffix, list_of_choices, all_numbered)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
variables_with_unique_values <- function() {

    ds <- get(activeDataSet(), envir = .GlobalEnv)
    not_duplicated_cols <- purrr::map_lgl(ds, ~!any(duplicated(.)))
    (not_duplicated_cols[not_duplicated_cols == TRUE]) %>%
        names() %>%
        sort()
}
#' @rdname Helper-functions
#' @export
#' @keywords internal
variables_with_unique_values_P <- function(n = 1) {

    activeDataSetP() && length(variables_with_unique_values() >= n)
}



# ___ Path / File ___ ========================================================

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



#' @rdname Helper-functions
#' @export
#' @keywords internal
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
        add_parts <- max(which(lengths2 <= max_length)) - 1    # -1 is minus the last one
        add_parts <- max(1, add_parts)
        show_trunc <-
            file.path(str_c(path_parts[1:add_parts], collapse = "/"),
                      " ... ",
                      path_parts[last_ind])
    }

    show_trunc
}

#' Extract file parts.
#' @rdname extract-fileparts
#' @keywords internal
#' @export
extract_path <- function(str) {
    sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\1", str)
    # sub("(.*)[\\/]([^.]+)(\\.[[:alnum:]]+$)", "\\1", str)
}

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
extract_extension <- function(str) {
    sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\3", str)
}




# ___ Check ___ ==============================================================
#' @rdname Helper-functions
#' @export
#' @keywords internal
show_error_messages <- function(message, message2 = message, title = "") {
    Message(message = message,  type = "error")
    RcmdrTkmessageBox(message2, icon = "error", title = title, type = "ok")
}

# + Valid name ---------------------------------------------------------------
#' Check if name is valid
#'
#' Function checks if string meets requirements for a single, valid, non-empty
#' R variable name. In case of incalid name, a box wit error message is thrown,
#' error message is written to R Commander window and result FALSE is returned.
#' Oterwise, TRUE is returned.\cr
#' \code{is_valit_name} - checks character validity.\cr
#' \code{is_empty_name} - does not check character validity.
#'
#' @param name (character) a single string.
#'
#' @return Logical value TRUE if string meets requirements, FALSE - otherwise.
#' @export
#'
#' @keywords internal
#'
#' @examples
#' is_valid_name("a")
#' is_valid_name("")
#' is_valid_name("|||")
#'
#' is_empty_name("a")
#' is_empty_name("")
#' is_empty_name("|||")
is_valid_name <- function(name) {

    if (is_empty_name(name)) {
        return(FALSE) # is not valid name

    } else if (!(name == make.names(name))) {
        # message  <- str_glue('"{name}" {gettext_bs("is not a valid name.")}')
        message  <- str_glue('Name "{name}" is not valid.')
        message2 <- str_glue(
            "{message} \n\n",
            "Valid names must start with a letter and contain only \n ",
            "letters, numbers, underscores (_) and periods (.). ")

        show_error_messages(message, message2, title = "Invalid Name")

        # is not valid name
        return(FALSE)

    } else {
        # is_valid_name
        return(TRUE)
    }


}

#' @rdname Helper-functions
#' @export
#' @keywords internal
is_not_valid_name <- function(name) {
    !is_valid_name(name)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
is_empty_name <- function(name) {
    !is_not_empty_name(name)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
is_not_empty_name <- function(name) {

    if (length(name) < 1) {
        message  <- "The object does not contain any strings.\n Please, enter the name."
        show_error_messages(message, message, title = "Missing Name")

        return(FALSE)

    } else if (length(name) > 1) {
        message  <- "The object cotains more than one string."
        show_error_messages(message, message, title = "Too Many Names")

        return(FALSE)

    } else if (!(is.character(name))) {
        message  <- ('The class of the object with \nthe name must be "character".')
        show_error_messages(message, message, title = "Invalid Class")

        return(FALSE)

    } else if (name == "") {
        message  <- str_glue('The name must not be empty.\nPlease, enter the name.')
        show_error_messages(message, message, title = "Empty Name")

        return(FALSE)

    } else {
        # is_valid_name
        return(TRUE)
    }

}


# + Duplicated name -----------------------------------------------------------

#' Message box to confirm replacement
#'
#' @param name string - name of the object to repace.
#' @param type string - type of the object to repace.
#'
#' @return Sring eihter "yes" or "no"
#' @export
#'
#' @examples
#' msg_box_confirm_to_replace()
msg_box_confirm_to_replace <- function(name, type = "Variable") {
    Type <- stringr::str_to_title(type)

    tclvalue(RcmdrTkmessageBox(
        title = str_glue("Overwrite {Type}"),
        message = sprintf('%s "%s" already exists.\n\nDo you agree to OVERWRITE the %s?',
                          Type, name, tolower(type)),
        icon = "warning",
        type = "yesno",
        default = "no"))
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
forbid_to_replace_variable <- function(name) {
    # Checks if variable exists in active dataset.
    #
    # Returns FALSE if:
    #     - variable does not exist.
    #     - variable exists but user agrees to overvrite it.
    #
    # Otherwise TRUE

    if (name %in% listVariables()) {
        msg_box_confirm_to_replace(name, "Variable") == "no"

    } else {
        FALSE
    }
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
forbid_to_replace_object <- function(name, envir = .GlobalEnv) {
    # Checks if object exists in (Global) environment
    #
    # Returns FALSE if:
    #     - object does not exist.
    #     - object exists but user agrees to overvrite (replace) it.
    #
    # Othervise TRUE

    if (name %in% listDataSets(envir = envir)) {
        msg_box_confirm_to_replace(name, "Dataset") == "no"

    } else if (name %in% objects(envir = envir, all.names = TRUE)) {
        msg_box_confirm_to_replace(name, "Object") == "no"

    } else {
        FALSE
    }
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
show_code_evaluation_error_message <- function() {
    show_error_messages(
        str_c("Something went wrong while evaluating the code.\n",
              "Please, check if all options are selected correctly."),

        str_c("Something went wrong while evaluating the code.\n\n",
              "Please, check if all options are selected correctly.\n\n",
              "If no error was found, you may consider reporting\n",
              "the bug in the package `RcmdrPlugin.biostat`\n",
              '(see link in "About").\n'),

        title = "Code Evaluation Error")
}


# + Class --------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
nonFactorsP <- function(n = 1) {
    #  n - number of non-factors.
    activeDataSetP() && length(setdiff(listVariables(), listFactors())) >= n
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
class_ggplot_P <- function(n = 1) {
    #  n - number of ggplot objects.
    length(list_objects_of_class("ggplot", envir = .GlobalEnv)) >= n
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
# List ggolot2 objects in global  environment.
list_objects_ggplot <- function(envir = .GlobalEnv) {
    list_objects_of_class("ggplot", envir = envir)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
objects_in_env_P <- function(n = 1, envir = .GlobalEnv, ...) {
    #  n - number of objects.
    isTRUE(length(objects(envir = envir, ...)) >= n)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
