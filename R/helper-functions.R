# TODO:
# -- get_system_info() code for other OS'es is needed

# FIXME: check if these functions are not (partial) duplicates:
#        get_obj_names(), list_objects_of_class()


#' @name Helper-functions
#' @title Helper functions for RcmdrPlugin.biostat.
#' @description Helper functions for package \pkg{RcmdrPlugin.biostat}.
#' @keywords internal
NULL

# ___ List variables  ___ ====================================================
#' Get contents of active dataset.
#' @param fail (logical) When no active dataset is selected:
#'        if `TRUE`, the function fails,
#'        if `FALSE`, then `NULL` is returned.
#'
#' @return
#' The contents of active dataset or `NULL`.
#'
#' @export
#' @md
#'
get_active_ds <- function(fail = TRUE) {
  active_ds <- active_dataset_0()

  if (is.null(active_ds)) {
    if (isTRUE(fail)) {
      stop("Active dataset is not present. ")

    } else {
      return(NULL)
    }
  }

  globalenv()[[active_ds]]
}

#' List objects of certain class.
#'
#' Create a vector of object names of certain class.
#'
#' @param class (character|`NULL`) A vector of object classes to return.
#'        Value `NULL` disables class filter (all classes will be returned).
#' @param envir An environment to look for the objects in.
#'        Other kind of objects will be coerced to an environment.
#'        The default value is parent frame.
#' @param hidden_names (logical)
#'        If `TRUE`, all object names are returned.
#'        If `FALSE`, names which begin with a `.` are omitted.
#'
#' @return A character vector of object names of certain class.
#' @export
#' @keywords internal
list_objects_of_class <-
  function(class = NULL, envir = parent.frame(), hidden_names = TRUE) {

    checkmate::assert_character(class, null.ok = TRUE)
    checkmate::assert_flag(hidden_names, null.ok = TRUE)

    all_variable_names <- names(envir)

    if (is.null(all_variable_names) || length(all_variable_names) == 0) {
      return(character(0))
    }

    if (!isTRUE(hidden_names)) {
      # Remove hidden names
      all_variable_names <- str_subset(all_variable_names, "^[^.]")
    }

    if (is.null(class)) {
      # Return obj. names of all classes
      return(all_variable_names)

    } else {
      # Return obj. names of indicated classes only

      if (is.data.frame(envir)) {
        # For data frames
        variable_names <-
          envir %>%
          dplyr::select({all_variable_names}) %>%
          dplyr::select_if(~inherits(., class)) %>%
          names()

        return(variable_names)

      } else {
        # For envirenments, lists, etc.
        envir <- as.environment(envir)
        variable_names <-
          all_variable_names %>%
          mget(envir = envir) %>%
          purrr::keep(~inherits(.x, class)) %>%
          names()

        return(variable_names)
      }
    }
  }

make_sorted <- function(vars) {
  if (getRcmdr("sort.names")) {sort(vars)} else {vars}
}


#' All variable names in active dataset
#'
#' @keywords internal
#' @export
variables_all <- function() {
  Variables()
  # list_objects_of_class(envir = get_active_ds(fail = FALSE))
}
#' Character variable names in active dataset
#'
#' @keywords internal
#' @export
variables_chr <- function() {
  vars <- list_objects_of_class("character", envir = get_active_ds(fail = FALSE))
  make_sorted(vars)
}
#' Logical variable names in active dataset
#'
#' @keywords internal
#' @export
variables_lgl <- function() {
  vars <- list_objects_of_class("logical", envir = get_active_ds(fail = FALSE))
  make_sorted(vars)
}
#' True factor variable names in active dataset
#'
#' @keywords internal
#' @export
variables_fct <- function() {
  vars <- list_objects_of_class("factor", envir = get_active_ds(fail = FALSE))
  make_sorted(vars)
}
#' Factor-like variable names in active dataset
#'
#' @keywords internal
#' @export
variables_fct_like <- function() {
  Factors()
  # list_objects_of_class(
  #   c("factor", "character", "logical"),
  #   envir = get_active_ds(fail = FALSE)
  # )
}
#' Non-factor-like variable names in active dataset
#'
#' @keywords internal
#' @export
variables_non_fct_like <- function() {
  setdiff(variables_all(), Factors())
  # setdiff(variables_all(), variables_fct_like())
}

#' Non-factor variable names in active dataset
#'
#' @keywords internal
#' @export
variables_non_fct <- function() {
  setdiff(variables_all(), variables_fct())
}


#' Two-level factor names in active dataset
#'
#' @keywords internal
#' @export
variables_fct_like_2_lvls <- function() {
  TwoLevelFactors()
}
#' Numeric variable names in active dataset
#'
#' @keywords internal
#' @export
variables_num <- function() {
  Numeric()
  # list_objects_of_class(
  #   c("integer", "numeric"),
  #   envir =  get_active_ds(fail = FALSE)
  # )
}

#' Numeric variable names in active dataset
#'
#' @keywords internal
#' @export
variables_int <- function() {
  vars <- list_objects_of_class("integer", envir = get_active_ds(fail = FALSE))
  make_sorted(vars)
}

#' Numeric variable names in active dataset
#'
#' @keywords internal
#' @export
variables_dbl <- function() {
  base::setdiff(Numeric(), variables_int())
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
variables_with_unique_values <- function() {

  ds <- get_active_ds()
  not_duplicated_cols <- purrr::map_lgl(ds, ~!any(duplicated(.)))
  vars <- names(not_duplicated_cols[not_duplicated_cols == TRUE])
  make_sorted(vars)
}



#' Numeric variable names in active dataset
#'
#' @keywords internal
#' @export
variables_oth <- function() {
  setdiff(
    variables_all(),
    c(variables_num(), variables_chr(), variables_lgl(), variables_fct())
  )
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
# ?stringr::str_glue
# ?parse
# ?eval
#
# envir_eval  - environment to evaluate in.
# envir_glue  - environment to glue in.
str_glue_eval <- function(..., envir = parent.frame(),
  # .collapse = "\n",
  .sep = "", .open = "{", .close = "}",
  envir_eval = envir,
  envir_glue = envir) {

  commands_as_text <- stringr::str_glue(...,
    .envir = envir_glue,
    .open  = .open,
    .close = .close)
  # commands_as_text <- stringr::str_c(commands_as_text, collapse = .collapse)
  eval(parse(text = commands_as_text), envir = envir_eval)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal

# x - commands written as text;
# envir - environment to evaluate in.
eval_text <- function(x, envir = parent.frame(), ...) {
  eval(parse(text = x), envir = envir, ...)
}


# Formatat code in a `tidyverse` style ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
style_cmd <- function(command, indent_by = 2, ...) {
  cmd <- styler::style_text(command, indent_by = indent_by, ...)
  structure(
    paste0(as.character(cmd), collapse = "\n"),
    class = c("glue", "character")
  )
}


# Try to parse the code written as text ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
try_command <- function(x) {
  safe_parse <-
    purrr::safely(parse, otherwise = structure("", class = "try-error"))
  rez <- safe_parse(text = x)

  if (!is.null(rez$error)) {
    structure(rez$error, class = "try-error")

  } else {
    rez$result
  }

  # On error returns class "try-error" as `justDoIt()` does
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
is_try_error <- function(obj) {
  inherits(obj, "try-error")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
function_not_implemented <- function() {

  top <- CommanderWindow()

  tk_messageBox(
    parent = top,
    "The function is not implemented yet!",
    icon = "warning",
    title = "No Function Yet",
    type = "ok")

  return()

  x = NULL
  doItAndPrint("## ~~~ Not implemented yet! ~~~\n")

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
# Function that creates a function to be used in 'Help' context menus.) {
open_help <- function(topic = NULL, package = NULL, vignette = NULL, ...) {
  function() {

    if (!is.null(topic)) {
      print(utils::help((topic), package = (package), ...))

    } else if (!is.null(vignette)) {
      print(utils::vignette(topic = (vignette), package = (package)))

    } else {
      print(utils::help(package = (package), ...))
    }

  }
}


# Function that creates a function to be used in 'Help' context menus.
open_online_fun <- function(url) {
  function() {
    open_online_tool(url)
  }
}

# ___ Translate ___ ==========================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
gettext_ezr <- function(...) {
  gettext(domain = "R-RcmdrPlugin.EZR", ...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
gettext_bs <- function(...) {
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
# Print code if code evaluation error occurred
logger_error <- function(command = NULL, error_msg = NULL) {


  if (!is.null(command)) {
    logger(str_c("#### START (code with error) ", spaces(50, "-")), rmd = FALSE)
    rez <- logger(str_c(
      "   #   ", str_split(command,"\n")[[1]], collapse = "\n"),  rmd = FALSE)

    txt <- "-----"

  } else {
    txt <- "START"
  }

  if (!is.null(error_msg)) {
    logger(str_c("#### ", txt ," (error message) ", spaces(52, "-")), rmd = FALSE)
    rez <- logger(str_c(
      "   #   ", str_split(error_msg,"\n")[[1]], collapse = "\n"),  rmd = FALSE)
  }

  logger(str_c("#### END ", spaces(70, "-")), rmd = FALSE)

  invisible(NULL)
}


# ___ Clipboard  ___ ==========================================================
#' Read text from clipboard.
#'
#' @param which (string) either `"tcltk"` or `"clipr"`.
#'
#' @return
#' - `read_clipboard()` and `read_clipboard_tcltk()` return a string from
#'  clipboard. If the clipboard contains non-sting object, empty sting (`""`)
#'  is returned.
#'
#' @details
#' - `read_clipboard_clipr()` uses **`clipr`** implementation.
#'    Encoding issues may occur on Windows.
#' - `read_clipboard_tcltk()` uses Tcl/Tk implementation of clipboard.
#'    Tcl/Tk clipboard gets empty, if is window gets closed.
#'
#'  The functions in all cases must return a single string.
#'  Possible different conditions on clipboard:
#' - one line of text;
#' - several lines of text;
#' - non-text object.
#'
#' @tests
#' expect_true(
#'    all.equal(read_clipboard(), read_clipboard_tcltk())
#' )
#' expect_true(
#'    all.equal(read_clipboard_clipr(), read_clipboard_tcltk())
#' )
#'
#' @examples
#' read_clipboard()
#'
#' @export
#' @md

read_clipboard <- function(which = "tcltk") {

  switch(
    which,
    tcltk = read_clipboard_tcltk(),
    clipr = read_clipboard_clipr()
  )

}

#' @rdname read_clipboard
#' @export
read_clipboard_clipr <- function() {

  tryCatch(
    paste0(clipr::read_clip(), collapse = "\n"),
    warning = function(w) "",
    error =   function(e) ""
  )

}

#' @rdname read_clipboard
#' @export
read_clipboard_tcltk <- function() {

  str <- try(
    silent = TRUE,
    tcltk::tclvalue(tcltk::.Tcl("selection get -selection CLIPBOARD"))
  )

  if (!inherits(str, "try-error")) str else ""
}

#' Expotr text/dataframe to clipboard.
#'
#' @param ds_name (string) Dataset's name.
#' @param sep (string) Data field separator, if data frame is exported.
#' @param ... Arguments passed to `clipr::write_clip`.
#'
#' @export
#' @md
export_to_clipboard <- function(ds_name = active_dataset_0(), sep = ",", ...) {
  try(
    clipr::write_clip(get(ds_name, envir = .GlobalEnv), sep = sep, ...),
    silent = TRUE
  )
}


#' @rdname export_to_clipboard
#' @export
export_to_clipboard_active_ds_tab <- function() {
  try(
    clipr::write_clip(
      get(active_dataset_0(), envir = .GlobalEnv), sep = "\t"),
    silent = TRUE)
}

#' @rdname export_to_clipboard
#' @export
export_to_clipboard_active_ds_tab_euro <- function() {
  try(
    clipr::write_clip(
      get(active_dataset_0(), envir = .GlobalEnv), sep = "\t", dec = ","),
    silent = TRUE)
}

# ___ Vectors  ___ ===========================================================
#' @rdname Helper-functions
#' @export
#' @keywords internal
# @examples swap(1:5, 2, 4)

# Swap 2 elements in a vector
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

#' @rdname Helper-functions
#' @keywords internal
#' @export
#' @param str sring
#' @param ... other arguments
clean_str <- function(str, transliterations = "Latin-ASCII", ...) {
  snakecase::to_any_case(
    make.names(str), transliterations = transliterations, ...)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
# Get object names by class
#
# @param name - name of dataset before suffix and prefix are added.
get_obj_names <-  function(
  include_class  = NULL,
  exclude_class  = NULL,
  include2_class = NULL,
  all.names = TRUE,
  envir = globalenv()) {


  all_variable_names <- objects(envir, all.names = all.names)

  if (length(all_variable_names) > 0) {
    objs <- mget(all_variable_names, envir = envir)

    if (!is.null(include_class)) {
      objs <- purrr::keep(objs, ~inherits(.x, include_class))
    }

    if (!is.null(exclude_class)) {
      objs <- purrr::discard(objs, ~inherits(.x, exclude_class))
    }

    if (!is.null(include2_class)) {
      objs <- purrr::keep(objs, ~inherits(.x, include2_class))
    }

    names(objs)
  }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
# Make a unique name for an object (e.g., data frame) by adding numbers
#
# @param name - name of dataset before suffix and prefix are added.
unique_obj_names <- function(names,
  prefix = "",
  suffix  = "",
  list_of_choices = objects(all.names = TRUE,
    envir = .GlobalEnv),
  all_numbered = FALSE) {
  if (length(names) == 0)
    return(NULL)

  initial_names <- str_glue("{prefix}{names}{suffix}")

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
unique_df_name <- function(names = active_dataset_0(),
  prefix = "",
  suffix = "",
  list_of_choices = objects(all.names = TRUE,
    envir = .GlobalEnv),
  all_numbered = FALSE) {

  unique_obj_names(names, prefix, suffix, list_of_choices, all_numbered)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
unique_file_name <- function(name = "file", # all names are converted to lower case.
  dir = getwd(),
  list_of_choices = dir(dir, all.files = TRUE),
  all_numbered = FALSE) {

  if (length(name) == 0) {
    name <- "file"

  } else if (length(name) > 1) {
    stop("There should be only one file name.")
  }

  ext <- fs::path_ext(name)
  initial_names <- fs::path_ext_remove(name)

  n_names <- length(name)

  list_to_check <-
    if (all_numbered) {
      c(list_of_choices, initial_names, initial_names)
    } else {
      c(list_of_choices, initial_names)
    }

  list_to_check %>%
    str_to_lower() %>%
    make.unique(sep = "_") %>%
    tail(n = n_names) %>%
    set_multi_ext(ext)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TODO: [???] "Not implemented"
unique_file_name_2 <- function(name = "file", # all names are converted to lower case.
  dir = NULL,
  list_of_choices = NULL,
  all_numbered = FALSE) {

  stop("Not implemented") # TODO: [???]

  if (length(name) == 0) {
    name <- "file"
  } else if (length(name) > 1) {
    stop("There should be only one file name.")
  }

  ext <- fs::path_ext(name)
  initial_names <- fs::path_ext_remove(name)
  n_names <- length(name)

  if (is.null(dir) || dir == "" || !fs::dir_exists(dir)) {
    dir <- getwd()
  }


  if (is.null(list_of_choices)) {
    list_of_choices <-
      dir(path = dir,
        pattern = str_replace_all(initial_names, "\\.", ".*?"),
        ignore.case = TRUE,
        all.files = TRUE)
  }

  list_to_check <-
    if (all_numbered) {
      c(list_of_choices, initial_names, initial_names)
    } else {
      c(list_of_choices, initial_names)
    }

  list_to_check %>%
    str_to_lower() %>%
    make.unique(sep = "_") %>%
    tail(n = n_names) %>%
    set_multi_ext(ext)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
unique_colnames <- function(names = "",
  prefix = "",
  suffix = "",
  list_of_choices = listVariables(),
  all_numbered = FALSE) {

  unique_obj_names(names, prefix, suffix, list_of_choices, all_numbered)
}
#' @rdname Helper-functions
#' @export
#' @keywords internal
unique_colnames_2 <- function(names = "",
  prefix = "",
  suffix = "",
  list_of_choices = listVariables(),
  all_numbered = TRUE) {

  unique_obj_names(names, prefix, suffix, list_of_choices, all_numbered)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Helper-functions
#' @export
#' @keywords internal
# Adds `` arround string with name, if that name does not follow the rules for
# R names.
safe_names <- function(str) {
  need_q <- (make.names(str) != str)
  q <- ifelse(need_q, "`", "")
  str_c(q, str, q)
}

quote_names <- function(str, q = '"', as_single_string = FALSE) {
  str <- str_c(q, str, q)
  if (as_single_string) {
    str <- str_c(str, collapse = ", ")
  }
  str
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
# path <- file.path("c:", "p1p1p1p1p1", "p2p2p2p2p2p2", "p3p3p3p3p3p3p3",
#  "file2.xlsx")
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
#' @name extract-fileparts
#' @param str (character) Path to file (with filename and extension).
#'
#' @keywords internal
#' @export
extract_filename <- function(str) {
  str %>% fs::path_file() %>% fs::path_ext_remove()
  # sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", str)
}

fs_path_ext_remove <- function(path) {
  fs::path(fs::path_dir(path), fs::path_ext_remove(fs::path_file(path)))
}

fs_path_ext_set <- function(path, ext) {
  fs::path(fs::path_dir(path), fs::path_ext_set(fs::path_file(path), ext = ext))
}

# More robust version of fs::path_ext_set()
set_multi_ext  <- function(path, ext) {
  # Prepares `ext` or results in error.
  if (length(ext) == 1) {
    ext <- rep(ext, length(path))
  } else if (length(ext) != length(path)) {
    stop("The number of extensions (`ext`) should be equal to 1 or match the number of paths (`path`).")
  }

  ext <- sub("^\\.(.*)$", "\\1", ext) # Removes leading dot from the extension, if present

  cond <- !is.na(path) & ext != "" # To exclude NA strings and empty extensions
  path[cond] <-  paste0(fs::path_ext_remove(path[cond]), ".", ext[cond])
  fs::path_tidy(path)
}


#' Check if file is writable.
#' @param file (character) Path to file.
#'
#' @return Logical. If \code{FALSE} it means that file is busy, locked or open.
#'
#' @export
is_file_writable <- function(file = "") {
  # If fais to rename (to the same name), it means that file is
  # busy, locked or open
  rez <- try(fs::file_move(file, file), silent = TRUE)
  !is_try_error(rez)
}

# ___ Messages and message boxes ____ ========================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname Helper-functions
#' @export
#' @keywords internal
# show_error_messages <- function(message, message2 = message, title = "") {
#     Message(message = message,  type = "error")
#     tk_messageBox(parent = CommanderWindow(), message2, icon = "error",
#                  caption = title, type = "ok")
# }
show_error_messages <- function(message, popup_msg = message, title = "Error",
  parent = CommanderWindow()) {
  Message(message = message, type = "error")
  # RcmdrTkmessageBox(popup_msg, icon = "error", title = title, type = "ok")
  tk_messageBox(parent = parent, message = popup_msg, caption = title,
    type = "ok", icon = "error")
}


msg_box_clear_input <- function(parent = CommanderWindow()) {
  tk_messageBox(
    parent = parent,
    type = "yesno",
    default = "no",
    icon = "warning",
    message = str_c(
      'The contents of the Input window will be deleted. \n',
      'Do you agree?'
    ),
    caption = "Clear Input")
}

msg_box_import_file_not_found <- function(parent = CommanderWindow()) {
  tk_messageBox(
    parent = parent,
    type = "ok",
    icon = "error",
    message = str_c(
      'The file was not found. Check if the name and \n',
      'the path in the box "File, URL" are correct and\n',
      'not empty.'),
    caption = "File Not Found")
}

msg_box_check_internet_connection <- function(parent = CommanderWindow()) {
  tk_messageBox(
    parent = parent,
    message = str_c(
      "It seems that your file is on the Internet, but you are offline.\n",
      "Please, check Internet connection."
    ),
    icon  = "warning",
    caption = "No Internet Connection",
    type  = "ok")
}

# ____ Other functions ____ ==================================================

#' @rdname extract-fileparts
#' @keywords internal
#' @export
do_nothing <- function(...) {}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Show Windows locale:
#   shell("systeminfo", intern = TRUE)
#   shell("systeminfo", intern = TRUE) %>% str_subset("Locale")
#   shell("systeminfo", intern = TRUE) %>% str_subset(fixed("locale", ignore_case = TRUE))
#   https://superuser.com/questions/1354256/how-can-i-programmatically-access-the-region-of-a-windows-computer
get_system_info <- function() {

  if (is.null(biostat_env$systeminfo)) {
    if (.Platform$OS.type == "windows") {
      # For Windows: get information about Windows.
      # Administrator password may be required.
      biostat_env$systeminfo <-
        shell("systeminfo", intern = TRUE) %>%
        structure(class = c("glue", "character"))

    } else {
      # TODO:
      #  get_system_info() code for other OS'es is needed

    }
  }

  biostat_env$systeminfo
}

# ___ Check ___ ==============================================================


#' @rdname extract-fileparts
#' @keywords internal
#' @export
is_url <- function(str) {
  str_detect(str, "^(http://|https://|ftp://|ftps://)")
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
#' @param parent (tkwin object) a parent Tcl/Tk window.
#'
#' @return Logical value TRUE if string meets requirements, FALSE - otherwise.
#' @export
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{\donttest{
#' is_valid_name("a")
#' is_valid_name("")
#' is_valid_name("|||")
#'
#' is_empty_name("a")
#' is_empty_name("")
#' is_empty_name("|||")
#' }}
is_valid_name <- function(name, parent = CommanderWindow()) {

  if (is_empty_name(name)) {
    return(FALSE) # is not valid name

  } else if (name != make.names(name)) {
    # message  <- str_glue('"{name}" {gettext_bs("is not a valid name.")}')
    message  <- str_glue('Name "{name}" is not valid.')
    message2 <- str_glue(
      "{message} \n\n",
      "Valid names must start with a letter and contain only \n ",
      "letters, numbers, underscores (_) and periods (.). ")

    show_error_messages(
      message, message2,
      title = "Invalid Name",
      parent = parent)

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
is_not_valid_name <- function(name, parent = CommanderWindow()) {
  !is_valid_name(name, parent = parent)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
is_empty_name <- function(name, which_name = "name", parent = CommanderWindow()) {
  !is_not_empty_name(name, which_name = which_name, parent = parent)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
# is_not_empty_name <- function(name) {
#
#     if (length(name) < 1) {
#         message  <- "The object does not contain any strings.\n Please, enter the name."
#         show_error_messages(message, message, title = "Missing Name")
#
#         return(FALSE)
#
#     } else if (length(name) > 1) {
#         message  <- "The object cotains more than one string."
#         show_error_messages(message, message, title = "Too Many Names")
#
#         return(FALSE)
#
#     } else if (!(is.character(name))) {
#         message  <- str_c('The class of the object with \n',
#                           'the name must be "character".')
#         show_error_messages(message, message, title = "Invalid Class")
#
#         return(FALSE)
#
#     } else if (name == "") {
#         message  <- str_glue('The name field must not be empty.\n',
#                              'Please, enter a name.')
#         show_error_messages(message, message, title = "Empty Name")
#
#         return(FALSE)
#
#     } else {
#         # is_valid_name
#         return(TRUE)
#     }
#
# }

is_not_empty_name <- function(name, which_name = "name",
  parent = CommanderWindow(),
  article = "a") {

  if (length(name) < 1) {
    message  <-
      str_glue("The object does not contain any strings.\n",
        " Please, enter {article} {which_name}.")

    show_error_messages(
      message, message,
      title = str_glue("Missing {str_to_title(which_name)}"),
      parent = parent)

    return(FALSE)

  } else if (length(name) > 1) {
    message  <- "The object cotains more than one string."

    show_error_messages(
      message, message,
      title = str_glue("Too Many {str_to_title(which_name)}s"),
      parent = parent)

    return(FALSE)

  } else if (!(is.character(name))) {
    message  <- str_glue('The class of the object with \n',
      'the {which_name} must be "character".')
    show_error_messages(
      message, message,
      title = "Invalid Class",
      parent = parent)

    return(FALSE)

  } else if (name == "") {
    message  <- str_glue('The {which_name} field must not be empty.\n',
      'Please, enter {article} {which_name}.')
    show_error_messages(
      message, message,
      title = str_glue("Empty {str_to_title(which_name)}"),
      parent = parent)

    return(FALSE)

  } else {
    # is_valid_name
    return(TRUE)
  }

}



#' @rdname Helper-functions
#' @export
#' @keywords internal
variable_is_not_selected <- function(obj, obj_type = "variable",
  parent = CommanderWindow(),
  article = "a") {

  if (length(obj) < 1 || (length(obj) == 1 && any(obj == ""))) {
    message  <- str_glue(
      "No {obj_type} is selected.\n",
      "Please, select {article} {obj_type}.")

    show_error_messages(
      message, message,
      title = str_glue("Select {article} {obj_type}"),
      parent = parent)

    return(TRUE)

  } else {
    return(FALSE)
  }
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
object_is_not_selected <- function(obj, obj_type = "object",
  parent = CommanderWindow(),
  article = "an") {

  if (length(obj) < 1 || (length(obj) == 1 && any(obj == ""))) {
    message  <- str_glue(
      "No {obj_type} is selected.\n",
      "Please, select {article} {obj_type}.")

    show_error_messages(
      message, message,
      title = str_glue("Select {article} {obj_type}"),
      parent = parent)
    return(TRUE)

  } else {
    return(FALSE)
  }
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
are_not_valid_names <- function(name, parent = CommanderWindow()) {
  # Checks if variable names are valis.
  #
  # Returns TRUE if any of the names are invalid

  if (length(name) < 1 || !is.character(name)) {
    message <- "Invalid (empty) name. \nPlease check and correct the name."

    show_error_messages(
      message, message,
      title = "Invalid (Empty) Name",
      parent = parent)
    return(TRUE) # is in valid name
  }

  invalid_names <- name[make.names(name) != name]

  if (length(invalid_names) == 0) {
    return(FALSE) # is valid name

  } else if (length(invalid_names) == 1) {
    msg_box_confirm_to_replace(
      invalid_names, "Variable", parent = parent) == "no"

  } else if (length(invalid_names) > 1) {
    # message  <- str_glue('"{name}" {gettext_bs("is not a valid name.")}')
    message  <- str_glue('Invalid names: \n{str_c(name, collapse = ", ")}')
    message2 <- str_glue(
      "The following names are invalid:\n\n",
      "{str_c(name, collapse = '\n')} \n\n",
      "Valid names must start with a letter and contain only \n",
      "letters, numbers, underscores (_) and periods (.). ")

    show_error_messages(
      message, message2,
      title = "Invalid Names",
      parent = parent)

    # is not valid name
    return(TRUE)
  }
}


# + Duplicated name -----------------------------------------------------------


#' Message box to confirm replacement
#'
#' @param name string - name of the object to repace.
#' @param type string - type of the object to repace.
#' @param parent Parent Tcl/Tk frame..
#'
#' @return Sring eihter "yes" or "no"
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' msg_box_confirm_to_replace()
#' }}
msg_box_confirm_to_replace <- function(name, type = "Variable",
  parent = CommanderWindow()) {
  Type <- stringr::str_to_title(type)

  tk_messageBox(
    parent = parent,
    caption = str_glue("Overwrite {Type}"),
    message = sprintf(
      str_c('%s "%s" already exists.\n\n',
        'Do you agree to DELETE existing %s and \n',
        'REPLACE it with the new one?'),
      Type, name, tolower(type)),
    icon = "warning",
    type = "yesno",
    default = "no")
}

#' @rdname msg_box_confirm_to_replace
#' @export
msg_box_confirm_to_replace_all <- function(name, type = "Variables",
  parent = CommanderWindow()) {
  Type <- stringr::str_to_title(type)
  vars <- str_c(name, collapse = "\n")

  tk_messageBox(
    parent = parent,
    caption = str_glue("Overwrite All {Type}"),
    message = str_glue(
      'The following {tolower(type)} already exist:\n\n',
      '{vars}\n\n',
      'Do you agree to DELETE ALL the {tolower(type)} and\n',
      'REPLACE them with the new ones?'),
    icon = "warning",
    type = "yesno",
    default = "no")
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
forbid_to_replace_variable <- function(name, parent = CommanderWindow()) {
  # Checks if variable exists in active dataset.
  #
  # Returns FALSE if:
  #     - variable does not exist.
  #     - variable exists but user agrees to overvrite it.
  #
  # Otherwise TRUE

  if (name %in% listVariables()) {
    msg_box_confirm_to_replace(name, "Variable", parent = parent) == "no"

  } else {
    FALSE
  }
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
forbid_to_replace_variables <- function(name, parent = CommanderWindow()) {
  # Checks if variable exists in active dataset.
  #
  # Returns FALSE if:
  #     - variable does not exist.
  #     - variable exists but user agrees to overvrite it.
  #
  # Otherwise TRUE

  vars_to_replace <- Variables()[Variables() %in% name]

  if (length(vars_to_replace) == 1) {
    msg_box_confirm_to_replace(
      vars_to_replace, "Variable", parent = parent) == "no"

  } else if (length(vars_to_replace) > 1) {
    msg_box_confirm_to_replace_all(
      vars_to_replace, "Variables", parent = parent) == "no"

  } else {
    FALSE
  }
}


#' @rdname Helper-functions
#' @export
#' @keywords internal
forbid_to_replace_object <- function(name, envir = .GlobalEnv,
  parent = CommanderWindow()) {
  # Checks if object exists in (Global) environment
  #
  # Returns FALSE if:
  #     - object does not exist.
  #     - object exists but user agrees to overvrite (replace) it.
  #
  # Othervise TRUE

  if (name %in% listDataSets(envir = envir)) {
    msg_box_confirm_to_replace(name, "Dataset", parent = parent) == "no"

  } else if (name %in% objects(envir = envir, all.names = TRUE)) {
    msg_box_confirm_to_replace(name, "Object", parent = parent) == "no"

  } else {
    FALSE
  }
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
forbid_to_replace_file <- function(name, parent = CommanderWindow()) {
  # Checks if file exists
  #
  # Returns FALSE if:
  #     - file does not exist.
  #     - file exists but user agrees to overvrite (replace) it.
  #
  # Othervise returns TRUE

  name_short <- fs::path_file(name)

  if (fs::file_exists(name)) {
    msg_box_confirm_to_replace(name_short, "File", parent = parent) == "no"

  } else {
    FALSE
  }
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
show_code_evaluation_error_message <- function(parent = CommanderWindow(),
  add_msg = "",
  add_note = "") {
  show_error_messages(
    str_c("Something went wrong. Please, try to fix the issue."),

    str_c(
      "Something went wrong.  \n\n",
      "To solve the issue, you may try to: \n",
      "  1. choose the other more appropriate options. \n",
      "  2. use standard variable (object) names, if they are not.  \n",
      "  3. fix an error in R code, if you wrote the code.  \n",
      "  4. choose a more appropriate model for your data. \n",
      "\n",
      c(add_msg),
      c(add_note)

      # "If no error was found, you may consider reporting\n",
      # "the bug in the package `RcmdrPlugin.biostat`\n",
      # '(see link in "About").\n'
    ),
    parent = parent,
    title = "Code Evaluation Error")
}


#' @rdname Helper-functions
#' @export
#' @keywords internal
dataset_not_persent <- function(parent = CommanderWindow()) {

  dataSets <- listDataSets()

  if (length(dataSets) == 0) {
    tk_messageBox(
      parent = parent,
      "There are no datasets in R memory.\nPlease, create or import a dataset.",
      icon = "warning",
      title = "No Datasets in R",
      type = "ok")

    return(TRUE)

  } else {
    return(FALSE)
  }
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
active_dataset_not_persent <- function(parent = CommanderWindow()) {

  .ds <- active_dataset_0()

  if (is.null(.ds)) {
    tk_messageBox(
      parent = parent,
      "There is no active dataset. \nPlease, select one.",
      icon = "warning",
      title = "Active Dataset Not Selected",
      type = "ok")

    return(TRUE)

  } else {
    return(FALSE)
  }
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
#' Get properties of variables in active dataset
#'
#' @param fun (function|character) function that will be applied on each
#'        variable in a dataset.
#'
#' @return A named verctor with the result of the function `fun` for each
#'         variable in a dataset.
#' @noRd
#' @md
#'
get_ds_var_prop <- function(fun) {
  fun_name <- deparse(substitute(fun))
  str_glue_eval(
    "mapply({fun_name}, {active_dataset()})",
    envir_eval = .GlobalEnv
  )
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
  activeDataSetP() && (sum(get_ds_var_prop(is.character)) >= n)
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
  activeDataSetP() && (sum(get_ds_var_prop(is.factor)) >= n)
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
  activeDataSetP() && (sum(get_ds_var_prop(is.logical)) >= n)
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
    ("data.frame" == str_glue_eval(
      "class({active_dataset()})[1]",
      envir_eval = .GlobalEnv
    ))
}
#' [!] Is the first class the same as in brackets?
#'
#' Check if the first class of active Rcmdr dataset is the same as determined.
#' @keywords internal
#' @export
first_class_isP <- function(df_class) {
  activeDataSetP() &&
    (df_class == str_glue_eval(
      "class({active_dataset()})[1]",
      envir_eval = .GlobalEnv
    ))
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
variables_with_unique_values_P <- function(n = 1) {

  activeDataSetP() && length(variables_with_unique_values() >= n)
}


# + Plots --------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
gg_objects_exist <- function(n = 1) {
  gg_objects <- list_objects_of_class("gg", envir = .GlobalEnv)
  length(gg_objects) >= n
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
gg_lastplot_exists <- function() {
  !is.null(ggplot2::last_plot())
}

# + Input validation ---------------------------------------------------------


is_numeric_str <- function(str) {
  str_detect(str, "^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$")
}

validate_num_0_0.5 <- function(P, W) {
  # P - value
  res <-
    if (is_numeric_str(P)) {
      dplyr::between(as.numeric(P), 0, 0.5)
    } else {
      FALSE
    }

  if (res == TRUE) {
    tkconfigure(W, foreground = "black")
    return(tcl("expr", "TRUE"))
  } else {
    return(tcl("expr", "FALSE"))
  }
}

validate_num_0_1 <- function(P, W) {
  # P - value
  res <-
    if (is_numeric_str(P)) {
      dplyr::between(as.numeric(P), 0, 1)
    } else {
      FALSE
    }

  if (res == TRUE) {
    tkconfigure(W, foreground = "black")
    return(tcl("expr", "TRUE"))
  } else {
    return(tcl("expr", "FALSE"))
  }
}


validate_numeric <- function(P, W) {
  # P - value
  res <- is_numeric_str(P)

  if (res == TRUE) {
    tkconfigure(W, foreground = "black")
    return(tcl("expr", "TRUE"))
  } else {
    return(tcl("expr", "FALSE"))
  }
}


is_pos_integer_str <- function(str) {
  str_detect(str, "^\\d+$")
}

validate_pos_int <- function(P, W) {
  # P - value
  res <- is_pos_integer_str(P)

  if (res == TRUE) {
    tkconfigure(W, foreground = "black")
    return(tcl("expr", "TRUE"))
  } else {
    return(tcl("expr", "FALSE"))
  }
}

is_integer_0_inf <- function(str) {
  str_detect(str, "^(\\d+|[Ii]nf)$")
}

validate_int_0_inf <- function(P, W) {
  # P - value
  res <- is_integer_0_inf(P)

  if (res == TRUE) {
    tkconfigure(W, foreground = "black")
    return(tcl("expr", "TRUE"))
  } else {
    return(tcl("expr", "FALSE"))
  }
}

validate_int_0_inf_empty <- function(P, W) {
  # P - value
  res <- is_integer_0_inf(P) || (str_trim(P) == "")

  if (res == TRUE) {
    tkconfigure(W, foreground = "black")
    return(tcl("expr", "TRUE"))
  } else {
    return(tcl("expr", "FALSE"))
  }
}


make_red_text <- function(P, W, S, v) {
  tkconfigure(W, foreground = "red2")
  tcl("expr", "TRUE")
}


make_red_text_reset_val <- function(to = "Inf") {
  function(P, W, S, v, s) {
    tcl("after", "idle", function() {tkconfigure(W, validate = v)})
    tkconfigure(W, foreground = "red2")
    tkdelete(W, "0", "end")
    tkinsert(W, "0", to)

    tcl("expr", "TRUE")
  }
}

is_valid_var_name_string <- function(str) {
  str_detect(str, "^[\\.]?[a-zA-Z][\\.0-9a-zA-Z_]*$")
}

validate_var_name_string <- function(P, W) {
  # P - value
  res <- is_valid_var_name_string(P)

  if (res == TRUE) {
    tkconfigure(W, foreground = "black")
    return(tcl("expr", "TRUE"))
  } else {
    return(tcl("expr", "FALSE"))
  }
}

# ___ Rcmdr ___ ==============================================================


#' Restart R commander.
#'
#' Helper function to restart R Commander.
#'
#' @export
rcmdr_restart_commander <- function() {
  if (packageVersion("Rcmdr") >= "2.7") {
    Rcmdr:::restartCommander()

  } else {
    ans <- command_rcmdr_close()
    if (ans != "cancel") {
      Rcmdr::Commander()
    }
  }
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
command_restart_rs_session <- function() {
  ans <- command_rcmdr_close()
  if (ans != "cancel") {
    rstudioapi::restartSession(command = "library(Rcmdr)")
  }
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
command_rcmdr_close <- function() {
  Rcmdr::closeCommander(
    ask = Rcmdr::getRcmdr("ask.to.exit"),
    ask.save = Rcmdr::getRcmdr("ask.on.exit")
  )
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
command_rcmdr_close_and_update_cran <- function() {
  ans <- command_rcmdr_close()
  if (ans != "cancel") {
    rstudioapi::restartSession(command =
        'update.packages(checkBuilt = TRUE, ask = "graphics")')
  }
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
command_rcmdr_close_r <- function() {
  response <- command_rcmdr_close()
  if (response == "cancel") {
    return()
  } else {
    cat("\n")
    quit(save = "no")
  }
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
is_console_output <- function() {
  isTRUE(options()$Rcmdr$console.output)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
command_rcmdr_use_1_window <- function() {
  command_rcmdr_set_output_mode(console.output = TRUE)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
command_rcmdr_use_3_windows <- function() {
  command_rcmdr_set_output_mode(console.output = FALSE)
}

#' @rdname Helper-functions
#' @export
#' @keywords internal
command_rcmdr_set_output_mode <- function(console.output = NULL) {
  # Current options
  Rcmdr_opts <- options()$Rcmdr

  if (is.null(Rcmdr_opts)) {
    Rcmdr_opts <- list(console.output = console.output)
  }

  updated_opts <-
    modifyList(Rcmdr_opts, list(console.output = console.output))

  if (!identical(Rcmdr_opts, updated_opts)) {
    # Set new options and restart R Commander
    options(Rcmdr = updated_opts)
    rcmdr_restart_commander()
  }
}

#' Is R Session in RStudio?
#' @export
#' @return Logical value indicating if GUI/IDE is RStudio.
#' @keywords internal
is_rstudio <- function() {
  .Platform$GUI == "RStudio"
}

#' @name always_on_top
#'
#' @title Always on Top.
#'
#' @description
#' Set (enable/disable) or get "always on top" mode.
#' The "always on top" mode puts Tcl/Tk window in front of windows of the other programs.
#' Functions that set (enable/disable) the mode:
#' \itemize{
#'   \item `set_always_on_top()` -- for any Tcl/Tk window;
#'   \item `rcmdr_set_always_on_top()` -- for any R Commander window.
#'  }
#' Functions that get current mode:
#' \itemize{
#'   \item `get_always_on_top()` -- for any Tcl/Tk window;
#'   \item `rcmdr_get_always_on_top()` -- for any R Commander window.
#'  }
#' @param obj Tcl/Tk window object.
#' @param flag (logical) Flag to enable (if `TRUE`) or disable(if `FALSE`)
#'        "always on top" mode.
#'
#' @return
#' The "get" functions return logical value that indicates if "always on top"
#' mode is enabled.
#'
#' @md
#' @export
set_always_on_top <- function(obj, flag = TRUE) {
  tcl("wm", "attributes", obj, "-topmost", flag)
}

#' @rdname always_on_top
#' @export
rcmdr_set_always_on_top <- function(flag = TRUE) {
  set_always_on_top(CommanderWindow(), flag)
}

#' @rdname always_on_top
#' @export
get_always_on_top <- function(obj) {
  tclvalue_lgl(tcl("wm", "attributes", obj, "-topmost"))
}

#' @rdname always_on_top
#' @export
rcmdr_get_always_on_top <- function(flag = TRUE) {
  get_always_on_top(CommanderWindow())
}

#' @rdname always_on_top
#' @export
#' @description `toggle_always_on_top()` toggles "always on top" state for Rcmdr.
toggle_always_on_top <- function() {
  if (isTRUE(rcmdr_get_always_on_top())) {
    rcmdr_set_always_on_top(FALSE)

  } else {
    rcmdr_set_always_on_top(TRUE)
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
print.tk2frame <- function(x, ...) {

  cat("A tk2widget of class '", class(x)[1], "'", "\n", sep = "")
  cursize <- size(x)
  if (cursize > 0)
    cat("Size: ", cursize, "\n", sep = "")
  val <- value(x)
  if (!is.null(val)) {
    cat("Value:\n")
    print(value(x))
  }
  return(invisible(x))
}
