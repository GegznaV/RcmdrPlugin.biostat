
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_import_from_excel <- function() {
  # Fonts ------------------------------------------------------------------
  font_consolas_regular <- tkfont.create(family = "Consolas", size = 8)

  # Variables --------------------------------------------------------------
  previous_file_name        <- tclVar("")
  previous_nrows_to_preview <- tclVar("")
  previous_sheet            <- tclVar("")

  biostat_env$file_contents      <- ""
  biostat_env$worksheets         <- NULL
  biostat_env$possibly_more_rows <- NULL

  on.exit({
    biostat_env$file_contents      <- ""
    biostat_env$worksheets         <- NULL
    biostat_env$possibly_more_rows <- NULL
  })


  # Functions ==============================================================
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ~ Read import options --------------------------------------------------

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_xl_sheet <- function() {
    val <- get_selection(f1_box_sheet)
    if (val == "") {
      NULL
    } else {
      val
    }
  }

  get_code_xl_sheet <- function() {
    val <- get_selection(f1_box_sheet)
    if (val == "") {
      ""
    } else {
      str_glue(', sheet = "{val}"')

    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  read_range <- function() {
    val <- get_values(f1_ent_range)
    if (val == "") {
      NULL
    } else {
      val
    }
  }

  get_code_xl_range <- function() {
    val <- get_values(f1_ent_range)
    if (val == "") {
      ""

    } else {
      str_glue(', range = "{val}"')
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_header <- function() {
    as.logical(get_values(f2_but_head))
  }

  get_code_header <- function() {
    val <- get_values(f2_but_head)
    switch(val,
      "TRUE"  = "",
      "FALSE" = ", col_names = FALSE",
      stop("Value '", val, "' is unknown (f2_but_head)."))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_skip <- function() {
    as.integer(get_values(f2_ent_skip))
  }

  get_code_skip <- function() {
    val <- get_values(f2_ent_skip)
    switch(val,
      "0" = "",
      str_c(", skip = ", val)
      # str_c(', skip = "', get_skip(), '"')
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_nrows_to_import <- function() {
    val <- get_values(f2_ent_max)
    if (val %in% c("Inf", "")) {
      Inf
    } else {
      as.integer(get_values(f2_ent_max))
    }
  }

  get_code_nrows <- function() {
    val <- get_values(f2_ent_max)
    if (val == "") {
      ""
    } else {
      str_c(", n_max = ", val)
    }
  }

  # Select value for f3_box_nrow_1 box ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_nrows_preview_input <- function() {
    val <- get_selection(f3_box_nrow_1)
    switch(val, "All" = Inf, as.integer(val))
  }

  get_nrows_preview_ds <- function() {
    val <- get_selection(f3_box_nrow_2)
    switch(val, "All" = Inf, as.integer(val))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_na_str <- function() {
    get_values(f2_ent_na)
  }

  get_code_na_str <- function() {
    val <- get_values(f2_ent_na)
    if (val == "") "" else str_c(',\n na = "', get_na_str(), '"')
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_name_repair <- function() {
    val <- get_selection(f2_box_rep)
    switch(val,
      "minimal"   = ,
      "unique"    = ,
      "universal" = val,
      "check unique" = "check_unique",
      "snake case"   = ~ janitor::make_clean_names(.),
      "make names"   = ~ make.names(., unique = TRUE),
      stop("Value '", val, "' is unknown (f2_box_out).")
    )
  }

  get_code_name_repair <- function() {
    val <- get_selection(f2_box_rep)
    switch(val,
      "minimal"      = ', .name_repair = "minimal"',
      "unique"       = "",
      "universal"    = ', .name_repair = "universal"',
      "check unique" = ', .name_repair = "check_unique"',
      "snake case"   = ", .name_repair = ~ janitor::make_clean_names(.)",
      "make names"   = ", .name_repair = ~ make.names(., unique = TRUE)",
      stop("Value '", val, "' is unknown (f2_box_out).")
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_output_type <- function(x) {
    val <- get_selection(f2_box_out)
    switch(val,
      "Data frame" = as.data.frame(x, stringsAsFactors = FALSE),
      "Data table" = data.table::as.data.table(x),
      "Tibble"     = tibble::as_tibble(x),
      stop("Value '", val, "' is unknown (f2_box_out)."))
  }

  get_code_output_type <- function() {
    val <- get_selection(f2_box_out)
    switch(val,
      "Data frame" = " %>% \n as.data.frame(stringsAsFactors = FALSE)",
      "Data table" = " %>% \n data.table::as.data.table()",
      "Tibble"     = "",
      # "Tibble"     = " %>% \n tibble::as_tibble()",
      stop("Value '", val, "' is unknown (f2_box_out).")
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_stringsAsFactors <- function(x) {
    val <- get_values(f2_opts, "stringsAsFactors")
    if (isTRUE(val)) {
      dplyr::mutate_if(x, is.character, forcats::as_factor)
    } else {
      force(x)
    }
  }

  get_code_stringsAsFactors <- function() {
    val <- get_values(f2_opts, "stringsAsFactors")
    if (isTRUE(val)) {
      "%>% \n dplyr::mutate_if(is.character, forcats::as_factor)"
    } else {
      ""
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_code_strip_white <- function() {
    val <- get_values(f2_opts, "strip_white")
    if (isTRUE(val)) {
      ""
    } else {
      str_c(",\n trim_ws = ", val)
    }
  }

  # ~ File -----------------------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Open file select dialogue
  get_path_to_file <- function() {
    initialdir <- read_path_to_file() %>% fs::path_dir()

    if (initialdir %in% c("", ".") || !fs::dir_exists(initialdir)) {
      initialdir <- getwd()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    filename <- tclvalue(
      tkgetOpenFile(
        # parent = top,
        initialdir = initialdir,
        title = "Choose Excel File to Import",
        filetypes = gettext_bs(
          "{{Excel files}          {.xlsx .xls}}
                    {{Excel open XML files}   .xlsx}
                    {{Excel 97-2003 files}    .xls}
                 {{All Files} *}")
        )
      )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (filename == "") {
      return()
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    set_values(f1_ent_file, filename)

    tkicursor(f1_ent_file$obj_text, "end")
    tkxview.moveto(f1_ent_file$obj_text, "1") # 0 - beginning, 1 - end.

    if (fs::is_file(filename)) {
      update_all()
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  open_file <- function() {
    browseURL(url = read_path_to_file())
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read value of file name entry box
  read_path_to_file <- function() {
    get_values(f1_ent_file)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check, if file exist or is URL
  # Return TRUE on success
  #        FALSE on failure.
  check_file_name <- function(on_success = do_nothing,
      on_failure = do_nothing,
      silent = FALSE) {
    filename <- read_path_to_file()

    if (fs::is_file(filename) || is_url(filename)) {
      on_success()
      return(TRUE)

    } else {
      # Delete text
      if (!isTRUE(silent)) {
        msg_box_import_file_not_found(top)
      }
    }

    on_failure()
    return(FALSE)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check if file contents need to be updated and return TRUE or FALSE
  need_update_from_file <- function() {
    filename       <- read_path_to_file()
    selected_sheet <- get_xl_sheet()

    changed_filename <- tclvalue_chr(previous_file_name) != filename

    changed_nrows_to_preview <-
      tclvalue_chr(previous_nrows_to_preview) != get_selection(f3_box_nrow_1)

    changed_sheet <-
      !isTRUE(tclvalue_chr(previous_sheet) == selected_sheet)

    any(changed_nrows_to_preview, changed_filename, changed_sheet)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Values used to check if update from file is needed
  update_previous_values <- function() {
    tclvalue(previous_nrows_to_preview) <- get_selection(f3_box_nrow_1)
    tclvalue(previous_file_name)        <- read_path_to_file()
    tclvalue(previous_sheet)            <- get_selection(f1_box_sheet)
  }


  # ~ Preview window ---------------------------------------------------------

  write_dataset_window <- function(contents, ...) {
    set_values(f3_dataset, values = contents, ...)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  clear_dataset_window <- function() {
    write_dataset_window("")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



  # ~ Read data ------------------------------------------------------------

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # `read_excel` with options from dialogue window
  do_read_excel <- function(input = read_path_to_file(), nrows = get_nrows_to_import()) {

    suppressMessages({
      readxl::read_excel(
        input,
        sheet        = get_xl_sheet(), # NULL
        range        = read_range(),   # NULL  # Takes precedence over skip, n_max and sheet.
        col_names    = get_header(),
        skip         = get_skip(),     # 0
        n_max        = nrows,          # Inf
        na           = get_na_str(),
        trim_ws      = get_values(f2_opts, "strip_white"),
        .name_repair = get_name_repair()
      ) %>%
        # get_output_type() %>%
        get_stringsAsFactors()

    })
  }


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  read_sheets_from_file <- function() {

    if (check_file_name(silent = TRUE)) {
      xl_file <- read_path_to_file()
      biostat_env$worksheets <- readxl::excel_sheets(xl_file)

    } else {
      biostat_env$worksheets <- NULL
    }
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  put_sheets_in_gui <- function() {

    worksheets <- biostat_env$worksheets

    if (is.null(worksheets)) {
      set_values(f1_box_sheet, "{}")
      return()
    }

    # if (length(worksheets) == 1) {
    #     # correct_input
    #     # { } prevents from a bug
    #     # which appears when an excel workbook
    #     # contains only one sheet and the name
    #     # of that sheet contains a space, e.g., "a b".
    #     worksheets <- str_c("{", worksheets, "}")
    # }

    set_values(f1_box_sheet, c(worksheets, ""))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  read_data_from_file <- function() {
    # [???]
    filename <- read_path_to_file()

    n_rows       <- get_nrows_preview_input()

    # Read data
    file_contents <- try(
      readr::read_lines(filename, n_max = n_rows),
      silent = TRUE)

    biostat_env$file_contents       <- file_contents
    biostat_env$possibly_more_rows  <- length(file_contents) >= n_rows

    # Update previous values
    update_previous_values()
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Format fread error for display
  parse_fread_error <- function(err) {
    err %>%
      str_replace("Error in .*\n", "") %>%
      str_replace("(does not exist)", "\n\\1") %>%
      str_replace("\\. ", ".\n") %>%
      str_trim()
  }

  # ~ Preview --------------------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Update contents of dataset preview window
  refresh_dataset_window <- function() {

    if (is_nothing_to_import()) {
      clear_dataset_window()
      return()

    } else if (need_update_from_file()) {
      read_sheets_from_file()
    }

    filename <- read_path_to_file()
    n_rows   <- min(get_nrows_preview_input(),
      get_nrows_to_import())

    # Get data from input
    suppressWarnings({
      ds_contents <- try(
        do_read_excel(filename, nrows = n_rows),
        silent = TRUE)
    })

    if (!is_try_error(ds_contents)) {
      # biostat_env$file_contents     <- file_contents
      biostat_env$file_contents       <- ds_contents   # [???] What about big files. md5 sum?
      biostat_env$possibly_more_rows  <- nrow(ds_contents) >= n_rows

      # Update previous values
      update_previous_values()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Default function
    refresh_dataset_window_0(
      widget           = f3_dataset,
      ds_contents      = ds_contents,
      preview_type     = get_selection(f3_box_type),
      nrow_preview_ds  = get_nrows_preview_ds(),
      expect_more_rows = possibly_more_rows())
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Update contents of dataset entry box.
  update_name_entry <- function() {
    filename  <- read_path_to_file()
    sheetname <- get_selection(f1_box_sheet)

    if (filename != "") {
      new_name <-
        filename %>%
        fs::path_file() %>%
        fs::path_ext_remove() %>%
        str_c("_", sheetname) %>%
        clean_str() %>%
        str_trunc(77, ellipsis = "") %>%
        unique_df_name()

      set_values(f1_ent_ds_name, new_name)
    }
  }
  # Update the the last number in name entry field
  update_name_entry_number <- function() {
    current_name <- get_values(f1_ent_ds_name)
    new_name <- unique_df_name(str_remove(current_name, "_\\d+$"))
    set_values(f1_ent_ds_name, new_name)

  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clear preview window
  clear_preview <- function() {
    clear_dataset_window()
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Update input preview, dataset (DS) preview, and DS name boxes.
  update_all <- function() {
    read_sheets_from_file()
    put_sheets_in_gui()
    update_name_entry()
    read_data_from_file()
    highlight_update_button()
    activate_f_open_button()
    # update_from_file()
    refresh_dataset_window()
    check_file_name()
  }

  # ~ Change properties ----------------------------------------------------

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  highlight_update_button <- function() {

    if (is_file_name_missing()) {
      tk_disable(f1_but_update)

    } else {
      tk_normalize(f1_but_update)

      if (need_update_from_file()) {
        tk_activate(f1_but_update)
        tkconfigure(f1_but_update, default = "active")

      } else {
        tkconfigure(f1_but_update, default = "normal")
      }
    }

    # path <- read_path_to_file()
    # if (fs::file_exists(path) || is_url(path)) {
    #   tk_normalize(f1_but_update)
    #
    #   if (need_update_from_file()) {
    #     tk_activate(f1_but_update)
    #     tkconfigure(f1_but_update, default = "active")
    #
    #   } else {
    #     tkconfigure(f1_but_update, default = "normal")
    #   }
    #
    # } else {
    #   tk_disable(f1_but_update)
    # }
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  activate_f_open_button <- function() {
    path <- read_path_to_file()
    if (fs::file_exists(path) || is_url(path)) {
      tk_normalize(f1_but_f_open)

    } else {
      tk_disable(f1_but_f_open)
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  range_activation <- function() {

    rng_val <- read_range()

    if (!is.null(rng_val)) {
      set_values(f2_ent_skip, 0)
      tk_disable(f2_ent_skip)
      set_values(f2_ent_max, "")
      tk_disable(f2_ent_max)

      if (str_detect(rng_val, "!")) {
        set_selection(f1_box_sheet, "")
        tk_disable(f1_box_sheet)

      } else {
        tk_read_only(f1_box_sheet)
        if (get_size(f1_box_sheet) > 0) {
          set_selection(f1_box_sheet, 1)
        }
      }

    } else {
      tk_normalize(f2_ent_skip)
      tk_normalize(f2_ent_max)
      tk_read_only(f1_box_sheet)
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  range_to_upper <- function() {
    set_values(f1_ent_range, str_to_upper(get_values(f1_ent_range)))
  }

  # ~ Input validation -----------------------------------------------------
  make_red_text_reset_val <- function(to = "") {
    function(P, W, S, v, s) {
      tcl("after", "idle", function() {
        tkconfigure(W, validate = v)
      })
      tkconfigure(W, foreground = "red2")

      if (!is.null(to)) {
        tkdelete(W, "0", "end")
        tkinsert(W, "0", to)
      }

      tcl("expr", "TRUE")
    }
  }

  is_excel_range <- function(P, W) {
    # P - value
    res <- str_detect(
      P, "^(.+!)?[[:alpha:]]+[[:digit:]]+:[[:alpha:]]+[[:digit:]]+$") ||
      (str_trim(P) == "")
    if (res == TRUE) {
      tkconfigure(W, foreground = "black")
      return(tcl("expr", "TRUE"))
    } else {
      return(tcl("expr", "FALSE"))
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  is_file_name_missing <- function() {
    str_trim(read_path_to_file()) == ""
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  is_nothing_to_import <- function() {
    if (is_file_name_missing()) {
      clear_preview()
      return(TRUE)

    } else {
      FALSE
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Checks possibility that file has more rows than previewed
  possibly_more_rows <- function() {         # [???]
    isTRUE(biostat_env$possibly_more_rows)
  }


  # ~ onOK -------------------------------- --------------------------------
  onOK <- function() {
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    new_name  <- get_values(f1_ent_ds_name)
    file_name <- read_path_to_file()

    # Reset widget properties before checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tkconfigure(name_entry, foreground = "black")

    # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    putDialog("window_import_from_excel", list(
      preview_ds_type = get_selection(f3_box_type)
    ))

    # Check if file exists or is URL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!check_file_name()) {
      return()
    }

    # If URL, check if internet connection is present.
    if (is_url(file_name) && !pingr::is_online()) {
      msg_box_check_internet_connection(top)
      return()
    }

    # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is_empty_name(new_name)) {
      return()
    }

    if (is_not_valid_name(new_name)) {
      return()
    }

    if (forbid_to_replace_object(new_name)) {
      return()
    }

    #  Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Library("tidyverse")
    Library("readxl")

    command <- str_glue(.trim = FALSE,
      "## Import data from Excel file \n",
      "{new_name} <- \n ",
      "  readxl::read_excel(\n",
      '  "{file_name}"',

      get_code_xl_sheet(),
      get_code_xl_range(),
      get_code_header(),
      get_code_skip(),
      get_code_nrows(),
      get_code_na_str(),
      get_code_strip_white(),
      get_code_name_repair(),
      "  )",

      get_code_output_type(),
      get_code_stringsAsFactors()
    )

    # ~~ Apply commands --------------------------------------------------
    result <- justDoIt(command)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (class(result)[1] != "try-error") {
      logger(style_cmd(command))
      active_dataset(new_name, flushModel = FALSE, flushDialogMemory = FALSE)

      # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # closeDialog()

    } else {
      logger_error(command, error_msg = result)
      show_code_evaluation_error_message()
      return()
    }

    # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # closeDialog()

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

  # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  initialize_dialog(title = gettext_bs("Import from Excel"))
  # , suppress.window.resize.buttons = FALSE)
  # FIXME: option FALSE does not work with "always-on-top",
  # NOTE:  option TRUE may cause issues for small resulution motnitors.

  tk_title(top, "Import Data from Excel File")

  # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  defaults <- list(
    preview_ds_type = "Data table"
  )
  initial <- getDialog("window_import_from_excel", defaults)


  # Widgets ================== =============================================

  # F1, Frame 1, choose file and name --------------------------------------
  f1 <- tk2frame(top)

  f1_lab_file <- tk_label_blue(f1, text = "File, URL: ")
  f1_ent_file <- bs_entry(
    f1, width = 90, sticky = "we", tip = "Path to file or URL.",
    on_key_release = function() {
      highlight_update_button()
      activate_f_open_button()
    })

  f1_but_set_1 <- tk2frame(f1)

  f1_but_paste <- tk2button(
    f1_but_set_1,
    # width = 7,
    # text = "Paste",
    image = "::image::bs_paste",
    command = function() {
      set_values(f1_ent_file,
        str_c(read_path_to_file(), read_clipboard()))
      tkicursor(f1_ent_file$obj_text, "end")
      tkxview.moveto(f1_ent_file$obj_text, "1")

      highlight_update_button()
      activate_f_open_button()
    },
    tip = "Paste file name or URL."
  )

  f1_but_clear <- tk2button(
    f1_but_set_1,
    # width = 7,
    # text = "Delete",
    image = "::image::bs_delete",
    command = function() {
      set_values(f1_ent_file, "")
      highlight_update_button()
      activate_f_open_button()
    },
    tip = "Clear file name or URL."
  )

  f1_but_update <- tk2button(
    f1_but_set_1,
    # width = 6,
    # text = "Update",
    # compound = "right",
    image = "::image::bs_down",
    command = update_all,
    tip = str_c("Read file (URL) and update preview.")
  )

  f1_but_f_choose <- tk2button(
    f1_but_set_1,
    # width = 7,
    # text = "Browse",
    image = "::image::bs_choose_file",
    command = get_path_to_file,
    tip = "Choose file to import."
  )

  f1_but_f_open <- tk2button(
    f1_but_set_1,
    image = "::image::bs_open_file",
    command = open_file,
    tip = "Try to open the file/URL."
  )

  f1_lab_ds_name <- tk_label_blue(f1, text = "Name: ")

  f1_ent_ds_name <- bs_entry(
    f1, width = 30, sticky = "ew",
    tip = "The name for the dataset.")

  f1_box_sheet <- bs_combobox(
    parent = f1,
    label = "Sheet: ",
    values = "",
    tip = "Choose worksheet to import data from.",
    # on_select     = correct_worksheet_selection,
    on_select     = function() {
      refresh_dataset_window()   # [???] Possible issues for URL and
      # big files, as multiple times of
      # downloading or import is needed.
      update_name_entry()
      highlight_update_button()
      activate_f_open_button()
    },
    width = 25
  )

  f1_ent_range <- bs_entry(
    label = "Range:",
    f1, width = 15, sticky = "we",
    on_key_release = function() {
      highlight_update_button()
      activate_f_open_button()
      range_activation()
      range_to_upper()
    },
    tip = str_c("(Optional)\n",
      "Range of cells in Excel sheet, e.g., B2:F18 or Sheet2!B2:F18. \n",
      "Overrides 'Skip lines', 'Max. lines' and possibly 'Sheet'."),
    validate = "focus",
    validatecommand = is_excel_range,
    invalidcommand  = make_red_text
  )

  # F2-3, Middle frame -----------------------------------------------------
  f_middle <- tk2frame(top)

  # F2, Frame 2, parameters ------------------------------------------------
  f2 <- tk2labelframe(f_middle, relief = "flat",
    borderwidth = 5, padding = 5, text = "Import options")
  f2a <- tk2frame(f2)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_txt_head <- tk_label(f2a, text = "Header", tip = tip_header)
  f2_but_head <- bs_radiobuttons(
    parent  = f2a,
    buttons = c("TRUE"  = "Yes",
      "FALSE" = "No"),
    layout  = "horizontal",
    commands = list(
      "TRUE"  = function() {
        set_selection(f2_box_rep, "unique")
        refresh_dataset_window()
      },
      "FALSE" = function() {
        set_selection(f2_box_rep, "universal")
        refresh_dataset_window()
      }
    ),
    tips = list(
      "TRUE"  = str_c("First row is treated as column names."),
      "FALSE" = str_c("There are no column name data.\n",
        "The names should be created.")
    )
  )

  tkgrid(f2a,         columnspan = 3,    sticky = "ew", pady = c(0, 2))
  tkgrid(f2_txt_head, f2_but_head$frame, sticky = "ew")
  tkgrid.configure(f2_txt_head, padx = c(2, 7))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_lab_skip <- tk2label(f2, text = "Skip lines")
  f2_lab_max  <- tk2label(f2, text = "Max. lines")
  f2_lab_na   <- tk2label(f2, text = "NA string")
  f2_lab_rep  <- tk2label(f2, text = "Repair names")
  f2_lab_out  <- tk2label(f2, text = "Import as")

  tip_box_skip <- "Number of rows to skip. \n Either positive integer or 0."
  tip_box_max  <- "Maximum number of rows to read. \nEither positive integer or infinity (Inf).\nBlank means that all rows will be read."
  tip_box_na   <- "A text, which is interpreted as missing (NA) values."
  tip_box_out  <- "The class of imported data set."

  tip_box_rep  <- str_c(
    "Check variable names to ensure that they are syntactically valid\n",
    "(start with a letter, do not contain spaces and other special symbols)\n",
    "and unique. \n",
    "Options: \n",
    # '  minimal  \t- Check if names exist, but do not repair;\n',
    "  check unique \t- Check if names are unique, but do not repair; \n",
    "  unique    \t- Names are made unique and not empty;\n",
    "  universal \t- Names are made unique and syntactic;\n",
    "  make names \t- Function `make.names` is applied.")

  # Possible options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  entry_width <- 16

  f2_ent_skip <- bs_entry(
    f2,
    width           = entry_width,
    tip             = tip_box_skip,
    value           = 0,
    validate        = "focus",
    validatecommand = validate_pos_int,
    invalidcommand  = make_red_text_reset_val(to = "0"))

  f2_ent_max  <- bs_entry(
    f2,
    width           = entry_width,
    tip             = tip_box_max,
    validate        = "focus",
    validatecommand = validate_int_0_inf_empty,
    invalidcommand  = make_red_text_reset_val(to = ""))

  f2_ent_na   <- bs_entry(
    f2,
    width          = entry_width,
    tip            = tip_box_na)

  f2_box_rep  <- bs_combobox(
    f2, width = entry_width - 3,
    values    = c("check unique", "unique", "universal", "snake case", "make names"),
    value = "unique",
    tip       = tip_box_rep,
    selection = 1,
    on_select = refresh_dataset_window)

  f2_box_out  <- bs_combobox(
    f2, width = entry_width - 3,
    values    = c("Data frame", "Tibble", "Data table"),
    tip       = tip_box_out,
    selection = 1)


  tkgrid(f2_lab_skip, f2_ent_skip$frame, pady = c(2, 0))
  tkgrid(f2_lab_max,  f2_ent_max$frame,  pady = c(2, 0))
  tkgrid(f2_lab_na,   f2_ent_na$frame,   pady = c(2, 0))
  tkgrid(f2_lab_rep,  f2_box_rep$frame,  pady = c(2, 0))
  tkgrid(f2_lab_out,  f2_box_out$frame,  pady = c(2, 0))

  tkgrid.configure(
    # f2_lab_head,
    f2_lab_skip, f2_lab_max, f2_lab_na, f2_lab_rep, f2_lab_out,
    padx = c(3, 5), sticky = "w"
  )

  tkgrid.configure(
    f2_ent_skip$frame,
    f2_ent_max$frame,
    f2_ent_na$frame,
    padx = c(2, 0)
  )

  # Check boxes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f2_opts <- bs_checkboxes(
    parent = f2,
    boxes = c(
      "stringsAsFactors",
      "strip_white"
    ),
    default_command = refresh_dataset_window,
    values = c(0, 1),
    labels = gettext_bs(c(
      "Convert strings to factors",
      "Strip leading and tailing spaces"
    )),

    tips = list(
      "stringsAsFactors" = str_c(
        "Convert strings (text variables) \n",
        "to factors (categorical variables)."
      ),

      "strip_white" = str_c(
        "Strip leading and trailing  \n",
        "whitespaces of unquoted fields."
      )
    ),
  )

  tkgrid(f2_opts$frame,
    padx = c(3, 0), pady = c(4, 2), columnspan = 3, sticky = "w")

  # F3, Frame 3, Preview ---------------------------------------------------
  f3 <- tk2labelframe(f_middle, relief = "flat", text = "Preview")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f3_but_set <- tk2frame(f3)
  f3_but_w   <- tk2frame(f3_but_set)
  f3_but_e   <- tk2frame(f3_but_set)

  f3_lab_nrows <- tk_label(f3_but_w, text = "Options:", tip = str_c(
    "Preview options: number of rows to\n",
    "display in each window and preview\n",
    "type."
  ))

  f3_box_nrow_1 <- bs_combobox(
    f3_but_w,
    width = 4,
    values = c("10", "100", "1000", "5000", "9999", "All"),
    tip = str_c(
      "Max. number of rows to read from the file for preview.\n",
      "Changing this option does not automatically update the preview."),
    selection = 2,
    on_select = function() {
      highlight_update_button()
      activate_f_open_button()
    })

  f3_box_nrow_2 <- bs_combobox(
    f3_but_w,
    width = 3,
    values = c("6", "8", "10", "14", "50", "100", "All"),
    tip = str_c("Max. number of rows to \n",
      "preview in window below."),
    selection = 2,
    on_select = refresh_dataset_window)

  f3_box_type <- bs_combobox(
    f3_but_w,
    width = 9,
    values = c("Data table", "Tibble", "Structure"),
    tip = str_c(
      "Type of preview: \n",
      " - Data table: top and bottom rows. \n",
      " - Tibble (tbl): compact display of the top rows.\n",
      " - Structure (str): column names, column types and a few values."),
    value = initial$preview_ds_type,
    on_select = refresh_dataset_window)

  f3_but_2 <- tk2button(
    f3_but_e,
    # text = "Clear",
    # width = 7,
    image = "::image::bs_delete",
    command = clear_preview,
    tip = "Clear preview window."
  )

  f3_but_3 <- tk2button(
    f3_but_e,
    # width = 7,
    # text = "Refresh",
    image = "::image::bs_refresh",
    command = function() {
      refresh_dataset_window()
      highlight_update_button()
      activate_f_open_button()
    },

    tip = str_c("Refresh Dataset's window.")
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  f3_dataset <- bs_text(
    f3, width = 75, height = 11, wrap = "none",
    undo = FALSE, state = "disabled",
    font = font_consolas_regular,
    label = "Dataset",
    tip = tip_variable_types)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Grid -------------------------------------------------------------------
  tkgrid(f1, padx = 10, sticky = "we")

  tkgrid(f1_lab_file, f1_ent_file$frame, "x", "x", "x", "x",  f1_but_set_1,
    pady = c(10, 2), sticky = "we")

  tkgrid(f1_lab_ds_name, f1_ent_ds_name$frame, "x", "x",
    f1_box_sheet$frame, f1_ent_range$frame,
    pady = c(0,  10), sticky = "we")
  tkgrid(f1_ent_range$frame)

  tkgrid(f1_but_f_choose, f1_but_f_open, f1_but_paste, f1_but_clear,
    f1_but_update, sticky = "e")

  tkgrid.configure(
    f1_lab_file, f1_lab_ds_name,
    f1_ent_range$frame_label, f1_ent_range$obj_label,
    sticky = "e")

  tkgrid.configure(
    f1_ent_file$frame, f1_ent_file$obj_text, f1_ent_file$frame_text,
    f1_ent_ds_name$frame, f1_ent_ds_name$obj_text, f1_ent_ds_name$frame_text,
    f1_ent_range$obj_text, f1_ent_range$frame_text,
    sticky = "we")

  tkgrid.configure(f1_ent_file$frame, f1_ent_ds_name$frame, padx = 2)
  tkgrid.configure(f1_ent_range$frame, f1_box_sheet$frame,  padx = c(10, 2))

  tkgrid.configure(f1_ent_file$frame, columnspan = 5)
  tkgrid.configure(
    f1_ent_file$frame_text,  f1_ent_ds_name$frame_text,
    f1_ent_file$obj_text,    f1_ent_ds_name$obj_text,
    sticky = "we")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f_middle, sticky = "news")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f2, f3, sticky = "nsw", padx = c(0, 5), pady = c(0, 15))
  tkgrid.configure(f2, sticky = "ns")
  tkgrid.configure(f3, sticky = "news")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f3_but_set, sticky = "ew", columnspan = 2)
  tkgrid(f3_dataset$frame, sticky = "news")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f3_but_w, f3_but_e, sticky = "ew", pady = c(2, 4))

  tkgrid(f3_lab_nrows, f3_box_nrow_1$frame, f3_box_nrow_2$frame,
    f3_box_type$frame, sticky = "w")

  tkgrid(f3_but_2, f3_but_3,
    # f3_but_4,
    sticky = "e")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid.configure(f3_lab_nrows,        padx = c(10, 2))
  tkgrid.configure(f3_box_nrow_2$frame, padx = c(2, 2))
  # tkgrid.configure(f3_but_4,            padx = c(0, 10))


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Help menus -------------------------------------------------------------
  help_menu <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

    tkadd(menu_main, "command",
      label    = "Specify cells for reading",
      command  = open_help("cell-specification", package = "readxl"))

    tkadd(menu_main, "command",
      label    = "Function `read_excel()`",
      command  = open_help("read_excel", package = "readxl"))

    tkadd(menu_main, "command",
      label    = "Function `excel_sheets()`",
      command  = open_help("excel_sheets", package = "readxl"))

    tkadd(menu_main, "separator")

    tkadd(menu_main, "command",
      label    = "Function `mutate_if()`",
      command  = open_help("mutate_if", package = "dplyr"))

    tkadd(menu_main, "command",
      label    = "Function `as_factor()`",
      command  = open_help("as_factor", package = "forcats"))

    tkadd(menu_main, "command",
      label    = "Function `is.character()`",
      command  = open_help("is.character", package = "base"))

    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }

  # Finalize ---------------------------------------------------------------

  # Help topic
  ok_cancel_help(
    close_on_ok = TRUE,
    on_help = help_menu,
    reset = "window_import_from_excel()",
    apply = "window_import_from_excel()",
    after_apply_success_fun = update_name_entry_number,
    ok_label = "Import")

  dialogSuffix(grid.buttons = TRUE, resizable = TRUE, bindReturn = FALSE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Configuration ----------------------------------------------------------
  set_values(f1_ent_ds_name, unique_obj_names("dataset", all_numbered = TRUE))
  highlight_update_button()
  activate_f_open_button()


  # Tags -------------------------------------------------------------------
  configure_tags(f3_dataset$text)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Make resizable window --------------------------------------------------

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rows (height)

  tkgrid.rowconfigure(top, 0, weight = 0)                # Title
  tkgrid.rowconfigure(top, 1, weight = 0, minsize = 2)   # F1 frame
  tkgrid.rowconfigure(top, 2, weight = 1)                # Middle frame
  tkgrid.rowconfigure(top, 3, weight = 0, minsize = 2)   # Buttons

  tkgrid.rowconfigure(f_middle, 0, weight = 1)
  tkgrid.rowconfigure(f3,       0, weight = 0)
  tkgrid.rowconfigure(f3,       1, weight = 1)
  tkgrid.rowconfigure(f3,       2, weight = 0)

  tkgrid.rowconfigure(f3_dataset$label, 0, weight = 0)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Columns (width)

  tkgrid.columnconfigure(top, 0, weight = 1, minsize = 50)

  tkgrid.columnconfigure(f1, 0, weight = 0) # Labels
  tkgrid.columnconfigure(f1, 1, weight = 1) # Text entries
  tkgrid.columnconfigure(f1, 2, weight = 0) # Text entries
  tkgrid.columnconfigure(f1, 3, weight = 0) # Text entries
  tkgrid.columnconfigure(f1, 4, weight = 0) # Buttons

  tkgrid.columnconfigure(f1_ent_file$frame_text,    0, weight = 1, minsize = 20)
  tkgrid.columnconfigure(f1_ent_ds_name$frame_text, 0, weight = 1, minsize = 20)
  tkgrid.columnconfigure(f1_ent_file$obj_text,      0, weight = 1, minsize = 20)
  tkgrid.columnconfigure(f1_ent_ds_name$obj_text,   0, weight = 1, minsize = 20)

  tkgrid.columnconfigure(f_middle, 0, weight = 0)
  tkgrid.columnconfigure(f_middle, 1, weight = 1)

  tkgrid.columnconfigure(f3,       0, weight = 1)

  tkgrid.columnconfigure(f3_but_set, 0, weight = 1)
  tkgrid.columnconfigure(f3_but_set, 1, weight = 0)
  tkgrid.columnconfigure(f3_but_w,   0, weight = 0)
  tkgrid.columnconfigure(f3_but_e,   0, weight = 1)


  # Interactive bindings ---------------------------------------------------

  # Prevents from closing window accidentally
  tkbind(top, "<Return>", refresh_dataset_window)

  tkbind(f1_ent_range$frame, "<<Paste>>", range_activation)
  tkbind(f1_but_paste$frame, "<<Paste>>", function() {
    highlight_update_button()
    activate_f_open_button()
  })

  # tkbind(f1_ent_range$frame, "<<Enter>>", range_activation)
  tkbind(f1_ent_range$obj_text, "<FocusOut>", refresh_dataset_window)

  tkbind(f2_ent_max$frame,  "<FocusOut>",  refresh_dataset_window)
  tkbind(f2_ent_skip$frame, "<FocusOut>",  refresh_dataset_window)
  tkbind(f2_ent_na$frame,   "<FocusOut>",  refresh_dataset_window)
  tkbind(top,               "<Control-S>", refresh_dataset_window)
  tkbind(top,               "<Control-s>", refresh_dataset_window)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  invisible()
}
