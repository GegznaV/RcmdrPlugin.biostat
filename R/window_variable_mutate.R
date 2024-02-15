# TODO:
# 1. In variable box text "[factor]" should be differentiated
#    to "[character]", "[logical]", "[factor]":
#    a) Add combobox to show only variables of certain class:
#       all, numeric, character, factor, logical, other
#
# 2. Add buttons:
#     a)  "+", "-", "*", etc. in style as used in "fit linear model" window
#     b)  numbers 0-9 for (Mac users)
#     c) add buttons to insert common functions.
#
# 3. Enable undo/redo for code writing box.



#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Correctly initializes window `window_variable_mutate()`
window_variable_mutate0  <- function() {
  window_variable_mutate()
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_variable_mutate <- function() {
  # Functions --------------------------------------------------------------
  insert_selected_variable <- function() {
    # var <- trim.blanks(getSelection(variablesBox))
    var <- get_selection(f1_vars$y)

    word <- str_glue('\\[{gettext_bs("factor")}\\]')

    if (length(grep(word, var, useBytes = TRUE)) == 1)
      var <- trim.blanks(sub(word, "", var))

    tkfocus(f2_entry_expr$obj_text)

    expr <- get_values(f2_entry_expr)

    new_expr <-
      if (expr == "") {
        var
      } else {
        last_chr <- stringr::str_sub(expr, -1)
        expr_sep <- if (last_chr %in% c("(", "[")) "" else " "
        paste(expr, var, sep = expr_sep)
      }

    set_values(f2_entry_expr, new_expr)

    tkicursor(f2_entry_expr$obj_text,      "end")
    tkxview.moveto(f2_entry_expr$obj_text, "1")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_labeled_variables <- function() {

    vars <- Variables()
    paste(vars,
      ifelse(vars %in% Factors(),
        yes = gettext_bs("[factor]"),
        no  = ""
    ))
  }

  # onOK -------------------------------------------------------------------
  onOK <- function() {
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    new_name   <- get_values(f2_name_entry)
    express    <- get_values(f2_entry_expr)
    use_groups <- get_values(f1_vars$checkbox)
    gr_var     <- get_selection(f1_vars$gr)

    # Check if expression is not empty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    check_empty <- gsub(";", "", gsub(" ", "", express))

    if (check_empty == "") {
      show_error_messages(
        gettext_bs("No expression was specified!"),
        # "No ???  was selected.\nPlease select a ???.",
        title = "Expression Is Missing",
        parent = top)

      tkfocus(f2_entry_expr$obj_text)
      tkselection.range(f2_entry_expr$obj_text, "0", "end")
      return()
    }

    # Add linting for the expression [???]
    # Message box should contain
    if (is_try_error(try_command(check_empty))) {
      show_error_messages(
        str_c(
          "The expression is incomplete or contains error(s)!\n",
          "Please, correct the expression."),
        title = "Invalid Expression",
        parent = top)
      tkfocus(f2_entry_expr$obj_text)
      tkselection.range(f2_entry_expr$obj_text, "0", "end")
      return()
    }

    # Check validity of var name ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is_empty_name(new_name, parent = top)) {
      tkfocus(f2_name_entry$obj_text)
      return()
    }

    if (is_not_valid_name(new_name, parent = top)) {
      tkfocus(f2_name_entry$obj_text)
      tkselection.range(f2_name_entry$obj_text, "0", "end")
      return()
    }

    if (forbid_to_replace_variables(new_name, parent = top)) {
      tkfocus(f2_name_entry$obj_text)
      tkselection.range(f2_name_entry$obj_text, "0", "end")
      return()
    }

    # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    new_name <- safe_names(new_name)


    if (isTRUE(use_groups)) {

      gr_var <- safe_names(gr_var)
      gr_text <- str_c(gr_var, collapse = ", ")
      cmd_group <- str_glue("group_by({gr_text}) %>% \n", .trim = FALSE)
      cmd_ungroup <- " %>% \n    ungroup()"

    } else {
      ds <- get(.ds, envir = globalenv())
      cmd_group <- if (dplyr::is_grouped_df(ds)) "ungroup() %>% \n" else ""

      cmd_ungroup <- ""
    }

    command <- str_glue(
      "## Computed variable: {new_name}\n",
      "{.ds} <- \n",
      "    {.ds} %>% \n",
      "    {cmd_group}",
      "    mutate({new_name} = {express})",
      "{cmd_ungroup}")

    # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Library("tidyverse")

    result <- justDoIt(command)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (class(result)[1] != "try-error") {
      logger(style_cmd(command))

    } else {
      logger_error(command, error_msg = result)
      show_code_evaluation_error_message(parent = top)
      return()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    command_dataset_refresh()
    tkfocus(CommanderWindow())
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Announce about the success to run the function `onOk()`
    TRUE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  }



  # Initial values ---------------------------------------------------------

  # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  .ds <- active_dataset()

  dialogue_title <- gettext_bs("Mutate: Compute or Transform Variable Values")
  initializeDialog(title = dialogue_title)
  tk_title(top, dialogue_title)

  # Widgets ----------------------------------------------------------------
  f1 <- tkframe(top)

  f1_vars <-
    bs_listbox_y_gr(
      parent      = f1,
      y_title     =
        gettext_bs("Current variables \n(double-click to add to expression)"),
      list_height = 7,
      y_vars      = get_labeled_variables(),
      y_params    = list(on_double_click = insert_selected_variable),
      gr_var_type = "fct_like",
      ch_label    = "Compute by group",
      ch_tip      = str_c(sep = "\n",
        "This option affects the results of statistical ",
        "functions such as mean(), median(), and sd(), as ",
        "the computations are carried out by group. ")
    )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2 <- tkframe(top)

  f2_name_entry <- bs_entry(
    parent = f2,
    width = "20",
    label = "New variable name",
    label_position = "above",
    tip = "The name for the new variable",
    value = unique_colnames_2("new_variable")
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_entry_expr <- bs_entry(
    f2,
    label = "Expression to compute",
    label_position = "above",
    width = 62,
    value = "",
    scroll_x = TRUE)

  tip(f2_entry_expr$obj_text) <- str_c(
    sep = "\n",
    "Standard operations: ",
    "+   -   *   /   ^   sqrt()    log()   abs()",
    "",
    "Examples: ",
    "    log(age)",
    "    length + width",
    "    as.factor(color)",
    "    weight / (height^2)",
    '    ifelse(age < 50, "young", "old")',
    "",
    'Browse "Help" for more information.')

  # Help menus -------------------------------------------------------------
  help_menu <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

    tkadd(menu_main, "command",
      label    = "Function mutate()",
      command  = open_help("mutate", package = "dplyr"))

    tkadd(menu_main, "command",
      label    = "Function group_by()",
      command  = open_help("group_by", package = "dplyr"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkadd(menu_main, "separator")

    tkadd(menu_main, "command",
      label    = "Operators in R",
      command  = open_help("Syntax", package = "base"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_compute <- tk2menu(menu_main, tearoff = FALSE)

    tkadd(menu_main, "cascade",
      label    = "Calculate values",
      menu     = menu_compute)

    tkadd(menu_compute, "command",
      label    = "Arithmetic operations: +, -, *, /, ^",
      command  = open_help("Arithmetic", package = "base"))

    tkadd(menu_compute, "command",
      label    = "Square root and absolute values: sqrt(), abs()",
      command  = open_help("sqrt", package = "base"))

    tkadd(menu_compute, "command",
      label    = "Logarithms and exponentials: log(), log10(), exp()",
      command  = open_help("log", package = "base"))

    tkadd(menu_compute, "command",
      label    = "Ranking: rank()",
      command  = open_help("rank", package = "base"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_compare <- tk2menu(menu_main, tearoff = FALSE)

    tkadd(menu_main, "cascade",
      label    = "Compare data",
      menu     = menu_compare)

    tkadd(menu_compare, "command",
      label    = "Logical operators: &, |, !, isTRUE()",
      command  = open_help("Logic", package = "base"))

    tkadd(menu_compare, "command",
      label    = "Compare: <, >, <=, >=, ==, !=",
      command  = open_help("Comparison", package = "base"))

    tkadd(menu_compare, "command",
      label    = "Value matching: %in%",
      command  = open_help("match", package = "base"))

    tkadd(menu_compare, "command",
      label    = "Are some values TRUE?: any()",
      command  = open_help("any", package = "base"))

    tkadd(menu_compare, "command",
      label    = "Are all values TRUE?: all()",
      command  = open_help("all", package = "base"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_convert_type   <- tk2menu(menu_main, tearoff = FALSE)

    tkadd(menu_main, "cascade",
      label    = "Change data types",
      menu     = menu_convert_type)

    tkadd(menu_convert_type, "command",
      label    = "Convert to factor: as.factor()",
      command  = open_help("factor", package = "base"))

    tkadd(menu_convert_type, "command",
      label    = "Convert to factor: as_factor()",
      command  = open_help("as_factor", package = "forcats"))

    tkadd(menu_convert_type, "command",
      label    = "Convert to integer: as.integer()",
      command  = open_help("integer", package = "base"))

    tkadd(menu_convert_type, "command",
      label    = "Convert to number: as.numeric()",
      command  = open_help("numeric", package = "base"))

    tkadd(menu_convert_type, "command",
      label    = "Extract numeric part: parse_number()",
      command  = open_help("parse_number()", package = "readr"))

    tkadd(menu_convert_type, "command",
      label    = "Extract numeric part: parse_number() + locale()",
      command  = open_help("locale()", package = "readr"))

    tkadd(menu_convert_type, "separator")

    tkadd(menu_convert_type, "command",
      label    = "Convert data to appropriate type: type.convert()",
      command  = open_help("type.convert", package = "utils"))

    tkadd(menu_convert_type, "command",
      label    = "Convert data to appropriate type: type_convert()",
      command  = open_help("type_convert", package = "readr"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_discretize   <- tk2menu(menu_main, tearoff = FALSE)

    tkadd(menu_main, "cascade",
      label    = "Discretize numbers",
      menu     = menu_discretize)

    tkadd(menu_discretize, "command",
      label    = "Create intervals of values: cut_interval(), cut_number(), cut_width()",
      command  = open_help("cut_interval", package = "ggplot"))

    tkadd(menu_discretize, "command",
      label    = "Create custom intervals of values: cut()",
      command  = open_help("cut", package = "base"))

    tkadd(menu_discretize, "command",
      label    = "Split into two groups by a condition: if_else()",
      command  = open_help("if_else", package = "dplyr"))

    tkadd(menu_discretize, "command",
      label    = "Split into two groups by a condition: ifelse()",
      command  = open_help("ifelse", package = "base"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    menu_statistical   <- tk2menu(menu_main, tearoff = FALSE)

    tkadd(menu_main, "cascade",
      label    = "Statistical functions",
      menu     = menu_statistical)

    tkadd(menu_statistical, "command",
      label    = "Mean: mean()",
      command  = open_help("mean", package = "base"))

    tkadd(menu_statistical, "command",
      label    = "Standard deviation: sd()",
      command  = open_help("sd", package = "stats"))

    tkadd(menu_statistical, "command",
      label    = "Median: median()",
      command  = open_help("median", package = "stats"))

    tkadd(menu_statistical, "command",
      label    = "Median absolute deviation: mad()",
      command  = open_help("mad", package = "stats"))

    tkadd(menu_statistical, "command",
      label    = "Interquartile range: IQR()",
      command  = open_help("IQR", package = "stats"))

    tkadd(menu_statistical, "command",
      label    = "Smallest and largest values: min(), max()",
      command  = open_help("min", package = "base"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }

  # Grid -------------------------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ok_cancel_help(
    close_on_ok = TRUE,
    on_help = help_menu,
    reset = "window_variable_mutate",
    apply = "window_variable_mutate",
    after_apply_success_fun = function() {
      # Get value before it is changed
      created_variable <- get_values(f2_name_entry)

      # Update values
      set_values(f2_name_entry, unique_colnames_2("new_variable"))
      set_values(f1_vars$y, get_labeled_variables())

      # Highlight created variable and variable name to change
      ind <- which(Variables() %in% created_variable) - 1

      tclAfter(1, function() {
        tk_see(f1_vars$y, ind)
        tkselection.clear(f1_vars$y$listbox, "0", "end")
        tkselection.set(f1_vars$y$listbox, ind)

        tkselection.range(f2_name_entry$obj_text, "0", "end")
        tkfocus(f2_name_entry$obj_text)
        tkicursor(f2_name_entry$obj_text, "0")
      })
    }
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f1_vars$frame,  # examples_Frame,
    sticky = "nw",
    columnspan = 2)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f1, sticky = "nw")
  tkgrid(f2, sticky = "nw")

  f2_eq <- tk_label(f2, text = " = ")

  tkgrid(f2_name_entry$frame, f2_eq, f2_entry_expr$frame,
    pady = c(15, 0),
    sticky = "new")

  tkgrid.configure(f2_eq, sticky = "ns")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(buttonsFrame, sticky = "ew", columnspan = 2)
  dialogSuffix(rows = 3, columns = 2, focus = f2_entry_expr$obj_text)
}

# ============================================================================
# Old way to present examples ================================================
# ============================================================================

# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# tkgrid_text <- function(text = "", frame = examples_Frame, fg = "black",
#                         sticky = "w", padx = 20, pady = 0, ...) {
#     tkgrid(labelRcmdr(frame, text = gettext_bs(text), fg = fg),
#            sticky = sticky, padx = padx, pady = pady, ...)
# }
#
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# tkgrid_text("\nExamples of expressions", fg = getRcmdr("title.color"))
# tkgrid_text('Common operations:   +   -   *   /   ^   sqrt()    log()   rank()',
#             fg = "darkgreen")
#
# tkgrid_text("Example 1: log(age)")
# tkgrid_text("Example 2: a + b")
# tkgrid_text("Example 3: as.factor(color)")
# tkgrid_text("Example 4: weight / (height^2)")
# tkgrid_text('Example 5: ifelse(age < 50, "young", "old")')
