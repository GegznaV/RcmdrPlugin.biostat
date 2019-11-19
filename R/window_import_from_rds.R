
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_import_from_rds <- function() {

  # Functions ==============================================================
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
        parent = top,
        initialdir = initialdir,
        title = "Choose Rds File to Import",
        filetypes = gettext_bs(
          "{ {Rds data file} {.Rds .rds} }
                     { {All Files} * }")))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (filename == "") {
      tkfocus(CommanderWindow())
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

    if (fs::is_file(filename)) {
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
  # Update contents of dataset entry box.
  update_name_entry <- function() {
    filename <- read_path_to_file()

    if (filename != "") {
      new_name <-
        filename %>%
        fs::path_file() %>%
        fs::path_ext_remove() %>%
        clean_str() %>%
        unique_df_name()

    } else {
      new_name <- unique_df_name("dataset", all_numbered = TRUE)
    }

    set_values(f1_ent_ds_name, new_name)

  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Update input preview, dataset (DS) preview, and DS name boxes.
  update_all <- function() {
    update_name_entry()
    check_file_name()
  }

  # ~ onOK -------------------------------- --------------------------------
  onOK <- function() {
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    new_name  <- get_values(f1_ent_ds_name)
    file_name <- read_path_to_file()

    # Check if file exists or is URL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!check_file_name()) {
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
    command <- str_glue(
      '## Import data from Rds file\n',
      '{new_name} <- readRDS("{file_name}")'
    )

    # ~~ Apply commands --------------------------------------------------
    result <- justDoIt(command)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (class(result)[1] != "try-error") {
      logger(style_cmd(command))
      active_dataset(new_name)

      # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      closeDialog()

    } else {
      logger_error(command, error_msg = result)
      show_code_evaluation_error_message()
      return()
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    command_dataset_refresh()
    tkfocus(CommanderWindow())

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Announce about the success to run the function `onOk()`
    TRUE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  }

  # Initialize dialog window ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  initialize_dialog(title = gettext_bs("Import from Rds"))
  # , suppress.window.resize.buttons = FALSE)
  # FIXME: option FALSE does not work with "always-on-top"

  tk_title(top, "Import Data from Rds File")

  # Widgets ================== =============================================

  # F1, Frame 1, choose file and name --------------------------------------
  f1 <- tk2frame(top)

  f1_lab_file <- tk_label_blue(f1, text = "File: ")
  f1_ent_file <- bs_entry(
    f1, width = 90, sticky = "we", tip = "Path to file")

  f1_but_paste <- tk2button(
    f1,
    image = "::image::bs_paste",
    command = function() {
      set_values(f1_ent_file,
        str_c(read_path_to_file(), read_clipboard()))
      tkicursor(f1_ent_file$obj_text, "end")
      tkxview.moveto(f1_ent_file$obj_text, "1")
    },
    tip = "Paste file name"
  )

  f1_but_clear <- tk2button(
    f1,
    image = "::image::bs_delete",
    command = function() {
      set_values(f1_ent_file, "")
    },
    tip = "Clear file name"
  )

  f1_but_f_choose <- tk2button(
    f1,
    image = "::image::bs_open_file",
    command = get_path_to_file,
    tip = "Choose file to import."
  )

  f1_but_refresh <- tk2button(
    f1,
    image = "::image::bs_refresh",
    command = update_name_entry,
    tip = str_c("Choose automatic dataset's name")
  )

  f1_lab_ds_name <- tk_label_blue(f1, text = "Name: ")
  f1_ent_ds_name <- bs_entry(
    f1, width = 90, sticky = "ew", tip = "Create a name for the dataset.")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f1, padx = 10, sticky = "we")

  tkgrid(f1_lab_file, f1_ent_file$frame, f1_but_f_choose, f1_but_paste, f1_but_clear,
    pady = c(10, 2),  sticky = "we")

  tkgrid(f1_lab_ds_name, f1_ent_ds_name$frame, f1_but_refresh,
    pady = c(0,  10), sticky = "we")

  tkgrid.configure(f1_lab_file, f1_lab_ds_name,             sticky = "e")
  tkgrid.configure(f1_ent_file$frame, f1_ent_ds_name$frame, sticky = "we", padx = 2)
  tkgrid.configure(
    f1_ent_file$frame_text, f1_ent_ds_name$frame_text,
    f1_ent_file$obj_text,   f1_ent_ds_name$obj_text,
    sticky = "we")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Finalize ---------------------------------------------------------------

  # Help topic
  ok_cancel_help(helpSubject = "readRDS",
    reset = "window_import_from_rds()",
    ok_label = "Import")

  dialogSuffix(grid.buttons = TRUE, bindReturn = FALSE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Configuration ----------------------------------------------------------
  set_values(f1_ent_ds_name, unique_obj_names("dataset", all_numbered = TRUE))


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_path_to_file()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  invisible()
}

