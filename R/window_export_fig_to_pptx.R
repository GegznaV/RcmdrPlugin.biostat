# TODO:
# -   Add tips to fields.
# -   Add buttons (Paste, Clear, etc.) for code field.
# -   Add better code checking or linting messages are needed.
# -   Add syntax highlighting.
# -   Add radiobuttons for units of measurement: [+]inch, [+] cm.
# [+] Add button to choose the type of location (fullsize, Custom..., etc.)
# -   putDialogue(): make to remember options of location, filename, etc.
#
# - Due to changes in RVG v.0.2.1:
#   + ---> use function ph_with.dml()
#   + https://cran.rstudio.com/web/packages/rvg/NEWS
#   + https://davidgohel.github.io/officer/articles/offcran/graphics.html#powerpoint-documents-and-graphics
#   + https://www.rdocumentation.org/packages/rvg/versions/0.2.1/topics/dml
#   + https://www.rdocumentation.org/packages/rvg/versions/0.2.1/topics/ph_with.dml

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
window_export_fig_to_pptx <- function() {
  # Chooses a function basing on version of rvg.
  if (packageVersion("rvg") < "0.2.1") {
    window_export_fig_to_pptx__old()

  } else {
    window_export_fig_to_pptx_2()
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For "rvg" version "0.2.1" or newer
window_export_fig_to_pptx_2 <- function() {
  # Fonts ------------------------------------------------------------------
  font_consolas_regular <- tkfont.create(family = "Consolas", size = 8)

  # Functions --------------------------------------------------------------

  # Open file select dialogue
  open_file_selection_dialogue <- function(f_path = fs::path(getwd(), ".")) {

    initialdir <- fs::path_dir(f_path)
    if (initialdir %in% c("", ".") || !fs::dir_exists(initialdir)) {
      initialdir <- getwd()
    }

    initialfile <- fs::path_file(f_path)
    if (initialfile %in%  c("", ".")) {
      initialfile <- default_pptx_name
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    file_name <- tclvalue(
      tkgetSaveFile(
        parent = top,
        # typevariable = typevariable, # to capture selected type
        title = "Create or Choose Text File to Save Data to",
        confirmoverwrite = FALSE,
        initialfile = initialfile,
        initialdir  = initialdir,
        filetypes   = "{ {PowerPoint file} {.pptx} }"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    file_name
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_path_to_file <- function() {

    file_name <- open_file_selection_dialogue(f_path = read_path_to_file())

    if (get_use_relative_path()) { # make relative path
      file_name <- make_relative_path(file_name)
    }

    file_name <- fs::path_ext_set(file_name, "pptx")

    set_values(f1_ent_file, file_name)

    set_check_pptx_msg()
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read value of file name entry box
  read_path_to_file <- function() {
    get_values(f1_ent_file)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  set_msg <- function(msg, color = "darkred") {
    tkconfigure(file_msg, text = msg, foreground = color)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  set_check_pptx_msg <- function() {
    file <- read_path_to_file()

    if (tolower(fs::path_file(file)) == ".pptx") {
      set_msg("File name is missing.")
      return()
    }

    # If extension is not pptx
    if (tolower(fs::path_ext(file)) != "pptx") {

      set_msg("Please, add '.pptx' as the file name extension.")
      return()
    }

    file_exists <- fs::is_file(file)

    if (file_exists) {
      if (!is_file_writable(file)) {
        # If file is not writable
        set_msg("Please, CLOSE the file before saving the plot.")

      } else {
        set_msg("A new slide will be added to the 'PowerPoint' file.",
          color = "green")
      }

      return()

    } else {
      set_msg("A new 'PowerPoint' file will be created.",
        color = "green")
      return()
    }

    set_msg("")
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  get_code_options <- function() {
    rez <- get_selection(f3_code_options)

    switch(
      rez,
      "Base plot / Code as-is"  = "code_base",
      "'ggplot2' plot"          = "code_gg",
      "Other plot via plot()"   = "code_plot",
      "Other plot via print()"  = "code_print",
      stop("unknown option of `f3_code_options`: ", rez)
    )
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  activate_location <- function() {
    switch(
      get_selection(f3_location_type),
      "Custom..." = tkgrid(f3_left$frame, f3_top$frame, f3_width$frame, f3_height$frame),
      # If other options
      tkgrid.remove(f3_left$frame, f3_top$frame, f3_width$frame, f3_height$frame)
    )
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  activate_options <- function() {
    switch(
      get_values(f3_source_of_plot),

      code       = {
        tk_read_only(f3_code_options)
        tkgrid.remove(f3_gg) # List of available ggplot2 objects
        tkgrid(f4)           # Code input box
      },

      obj_gg     = {
        tk_disable(f3_code_options)
        tkgrid(f3_gg)
        # tk_disable(f3_gg_obj_name_box)
        tkgrid.remove(f4)
      },

      last_gg    = {
        tk_disable(f3_code_options)
        tkgrid.remove(f3_gg)
        tkgrid.remove(f4)
      }
    )

    # Important for the first time

    if (is.null(ggplot2::last_plot())) {
      tk_disable(f3_source_of_plot, "last_gg")

      if (get_values(f3_source_of_plot) == "last_gg") {
        # Deselect disabled value
        set_values(f3_source_of_plot, "code")
      }

    } else {
      tk_normalize(f3_source_of_plot, "last_gg")
    }

    gg_objects <- list_objects_of_class("gg", envir = .GlobalEnv)

    if (length(gg_objects) == 0) {

      tkgrid.remove(f3_gg)
      tk_disable(f3_source_of_plot, "obj_gg")

      if (get_values(f3_source_of_plot) == "obj_gg") {
        # Deselect disabled value
        set_values(f3_source_of_plot, "code")
      }

    } else {
      tkgrid(f3_source_of_plot$frame)
      tk_normalize(f3_source_of_plot, "obj_gg")
      set_values(f3_gg_obj_name_box, gg_objects)

    }
    set_check_pptx_msg()
  }

  # Function onOK ----------------------------------------------------------
  onOK <- function() {
    # Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cursor_set_busy(top)
    on.exit(cursor_set_idle(top))

    # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pptx_file           <- get_values(f1_ent_file)

    source_of_plot      <- get_values(f3_source_of_plot)
    code_options        <- get_code_options()
    code                <- get_values(f4_code_input)
    gg_object_name      <- get_selection(f3_gg_obj_name_box)

    open_after_saving   <- get_values(f2_open_file_box, "open_file")

    location_type       <- get_selection(f3_location_type)
    pos_width           <- as.numeric(get_values(f3_width))
    pos_height          <- as.numeric(get_values(f3_height))
    pos_left            <- as.numeric(get_values(f3_left))
    pos_top             <- as.numeric(get_values(f3_top))
    in_units            <- "inches" # "cm"


    # Reset widget properties before checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # tkconfigure(name_entry, foreground = "black")

    # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check input code, if appropriate

    switch(
      source_of_plot,

      obj_gg = {
        if (length(gg_object_name) < 1) {
          msg <- "No 'ggplot2' object is selected.\nPlease select one."
          tk_messageBox(
            parent = top,
            type = "ok",
            icon = "error",
            message = msg,
            caption = "Object is Not Selected")

          return()
        }
      },

      code = {

        if (str_trim(code) == "") {

          msg <- str_c(
            "Code field is empty.\n",
            "Please, enter the code that generates a plot.")

          tk_messageBox(
            parent = top,
            type = "ok",
            icon = "error",
            message = msg,
            caption = "Code is Missing")

          return()
        }
      }
    )

    # Check R syntax ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    switch(
      source_of_plot,

      code = {
        # code <- get_values(f4_code_input)

        code_error <- svTools::lint(text = code, type = "flat", sep = "|")

        if (code_error != "") {

          msg <- str_c(
            "Error in input code:", "\n\n",
            str_c(capture.output(code_error), collapse = "\n"))

          tk_messageBox(
            parent = top,
            type = "ok",
            icon = "error",
            message = msg,
            caption = "R Code Syntax Error")

          return()
        }
      }
    )

    # Check if file name is not empty ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is_empty_name(pptx_file)) {
      return()
    }

    # If file name is missing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (tolower(fs::path_file(pptx_file)) == ".pptx") {
      msg <-
        str_c(
          "Only extension '.pptx' is found but file name is missing.\n",
          "Please, create a file name.")

      tk_messageBox(
        parent = top,
        type = "ok",
        icon = "error",
        message = msg,
        caption = "File Name Is Missing")

      return()
    }

    # If extension is pptx ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (tolower(fs::path_ext(pptx_file)) != "pptx") {
      msg <-
        str_c(
          "Please, add '.pptx' as a file name extension.\n",
          "E.g., 'r_plots.pptx'.")

      tk_messageBox(
        parent = top,
        type = "ok",
        icon = "error",
        message = msg,
        caption = "Wrong File Name Extension")

      return()
    }


    if (fs::is_file(pptx_file)) {
      if (!is_file_writable(pptx_file)) {

        msg <- str_c(
          "It seems that PowerPoint file is open, busy or read-only.\n",
          "Please, CLOSE the file before saving the plot or choose \n",
          "another file.")

        tk_messageBox(
          parent = top,
          type = "ok",
          icon = "error",
          message = msg,
          caption = "Close the PowerPoint File")

        return()
      }
    }


    # if (is_not_valid_name(pptx_file)) {
    #     return()
    # }

    # if (forbid_to_replace_variables(new_name)) {
    #     return()
    # }
    #
    # if (variable_is_not_selected(new_name, "variable")) {
    #     return()
    # }
    #
    # if (variable_is_not_selected(new_name, "group variable")) {
    #     return()
    # }

    # if (forbid_to_replace_object(new_name)) {
    #     return()
    # }

    # if (is_empty_name(new_name))              {return()}
    # if (is_not_valid_name(new_name))          {return()}
    # if (forbid_to_replace_variable(new_name)) {return()}

    # if (object_is_not_selected(new_name))     {return()}
    # if (forbid_to_replace_object(new_name))   {return()}

    # if (??? == "") {
    # show_error_messages(
    #     "No ???  was selected.\nPlease select a ???.",
    #     title = "")
    # return()
    # }

    #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    putDialog("window_export_fig_to_pptx", list(
      pptx_file      = pptx_file,
      source_of_plot = source_of_plot,
      code           = code,
      code_options   = get_selection(f3_code_options),
      open_file      = open_after_saving,
      pos_width      = pos_width,
      pos_height     = pos_height,
      pos_left       = pos_left,
      pos_top        = pos_top,
      location_type  = location_type
    ))

    # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    gg_code <- switch(
      source_of_plot,

      "last_gg" =
        "ggobj = ggplot2::last_plot()",

      "obj_gg"  = {
        str_glue("ggobj = {gg_object_name}")
      },

      "code" = switch(
        code_options,
        "code_base" = {
          str_glue(
            "code = {{ \n",
            "    # Code that draws the plot \n",
            "    {code} \n",
            "    }}")
        },

        "code_print" = {
          str_glue(
            "code = {{ \n",
            "    print(\n",
            "      # Code that draws the plot \n",
            "      {code} \n",
            "    ) \n",
            "}}")
        },

        "code_plot" = {
          str_glue(
            "code = {{ \n",
            "    plot(\n",
            "      # Code that draws the plot \n",
            "      {code} \n",
            "    ) \n",
            "}}")
        },

        "code_gg" = {
          str_glue(
            "ggobj = {{ \n",
            "    # Code that draws the plot \n",
            "    {code} \n",
            "}}")
        },
        stop("Unknown option of `code_options`")
      ),

      stop("Unknown option of `source_of_plot`")
    )

    file_open <-
      if (file.exists(pptx_file)) {
        str_glue('"{pptx_file}" %>% \n')

      } else {
        ""
      }

    code__open_after_saving <-
      if (open_after_saving) {
        str_glue(
          '\n\n',
          '## Open PowerPoint file \n',
          'browseURL("{pptx_file}")'
        )
      } else {
        ""
      }

    code__dml_plot <- str_glue(.sep = "\n",
      'dml_plot <- rvg::dml(',
      '    {gg_code},',
      '    bg = NULL, # background color',
      '    pointsize = 12,',
      '    editable = TRUE',
      ')'
    )

    code__location <-
      switch(location_type,
        "Full size"  = "officer::ph_location_fullsize()",
        "Left side"  = "officer::ph_location_left()",
        "Right side" = "officer::ph_location_right()",
        "Center"     = str_glue(.sep = "\n",
          ' officer::ph_location(',
          '        left   = 1 ,  # {in_units}',
          '        top    = 1,   # {in_units}',
          '        width  = 8,   # {in_units}',
          '        height = 5.5, # {in_units}',
          '    )'
        ),
        "Custom..."    = str_glue(.sep = "\n",
          ' officer::ph_location(',
          '        left   = {pos_left}, # {in_units}',
          '        top    = {pos_top}, # {in_units}',
          '        width  = {pos_width}, # {in_units}',
          '        height = {pos_height}, # {in_units}',
          '    )'
        ),
        stop("Unknown value of 'location_type': ", location_type)

      )

    # Save plot
    command <- str_c(
      sep = "\n",
      '## Save plot',
      '    {code__dml_plot}\n',

      '    {file_open}officer::read_pptx() %>%',
      '    officer::add_slide(layout = "Blank", master = "Office Theme") %>%',
      '    officer::ph_with(dml_plot, location = {code__location}) %>%',
      '    print(target = "{pptx_file}")',

      '{code__open_after_saving}'
    ) %>%
      str_glue()

    # command %>% structure(class = c("glue", "character"))

    # Apply commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Library("tidyverse")
    Library("officer")
    Library("rvg")

    result <- justDoIt(command)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (class(result)[1] != "try-error") {
      logger(style_cmd(command))

      # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # closeDialog()

    } else {
      popup_msg <- str_c(
        result,
        "\nTry one of the following options:\n",
        "  1) select a more appropriate value of 'Desired result';\n",
        "  2) check if the code really creates a plot; \n",
        "  3) correct all the errors in the code.     \n\n",
        "Print additional information related to this error?"
      )

      ans <- tk_messageBox(
        parent  = top,
        message = popup_msg,
        caption = "Incorrent Options or Error in Code",
        type    = "yesno",
        default = "no",
        icon    = "error"
      )

      if (ans == "yes") {
        logger_error(command, error_msg = result)
        # show_code_evaluation_error_message()
      }
      return()
    }

    # Close dialog ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # closeDialog()

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkfocus(CommanderWindow())
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Announce about the success to run the function `onOk()`
    TRUE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  }

  # Initial values ---------------------------------------------------------

  # Set initial values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  default_pptx_name <- "editable_r_plots.pptx"

  # Initialize dialog window and title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  dialogue_title <- "Export Editable Plot to PowerPoint File"
  initializeDialog(title = gettext_bs(dialogue_title))
  tk_title(top, dialogue_title)

  # Get default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  defaults <- list(
    pptx_file      = default_pptx_name,
    source_of_plot = "code",
    code_options   = "Base plot / Code as-is",
    code           = "",
    open_file      = FALSE,
    location_type  = "Center",
    pos_left       = 1, # L = 1, T = 1, W = 8, H = 5.5 is centered position.
    pos_top        = 1,
    pos_width      = 8,
    pos_height     = 5.5
  )
  initial <- getDialog("window_export_fig_to_pptx", defaults)


  # Widgets ----------------------------------------------------------------

  file_msg <- bs_label(parent = top, text = "", fg = "darkred")


  # F1, Frame 1, choose file and name --------------------------------------
  f1 <- tk2frame(top)

  f1_lab_file <- bs_label_b(f1, text = "File: ")
  f1_ent_file <- bs_entry(
    f1, width = 60, sticky = "we",
    tip = "PowerPoint file name (new or existing)",
    value = initial$pptx_file,
    on_key_release = set_check_pptx_msg)

  f1_but_set_1 <- tk2frame(f1)

  f1_but_paste <- tk2button(
    f1_but_set_1,
    # width = 7,
    # text = "Paste",
    image = "::image::bs_paste",
    command = function() {
      set_values(f1_ent_file, read_clipboard())
      tkicursor(f1_ent_file$obj_text, "end")
      tkxview.moveto(f1_ent_file$obj_text, "1")

      set_check_pptx_msg()
    },
    tip = "Paste file name."
  )

  f1_but_clear <- tk2button(
    f1_but_set_1,
    # width = 7,
    # text = "Delete",
    image = "::image::bs_delete",
    command = function() {
      set_values(f1_ent_file, "")
      set_check_pptx_msg()
    },
    tip = "Clear file name."
  )

  f1_but_update <- tk2button(
    f1_but_set_1,
    # width = 6,
    # text = "Update",
    # compound = "right",
    image = "::image::bs_refresh",
    command = function() {

      f_name <- read_path_to_file()

      if (f_name == "") {
        f_name <- default_pptx_name
      }

      # Add extension
      set_values(f1_ent_file,
        values = fs::path_ext_set(f_name, "pptx"))
      set_check_pptx_msg()
      activate_options()
    },
    tip = str_c("Update window, e.g., \n",
      "check file name and  \n",
      "add '.pptx', if missing.")
  )

  f1_but_f_choose <- tk2button(
    f1_but_set_1,
    # width = 7,
    # text = "Browse",
    image = "::image::bs_open_file",
    command = function() {
      get_path_to_file()
    },
    tip = "Choose file to export to."
  )


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # F2 ---------------------------------------------------------------------

  f2 <- tk2frame(top)

  f2_open_file_box <- bs_checkboxes(
    parent = f2,
    boxes  = c(open_file = "Open file after saving the plot"),
    values = c(open_file = initial$open_file))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # F3 ---------------------------------------------------------------------

  f3     <- tk2frame(top)

  f3_pos <- tk2frame(f3)
  f3_but <- tk2frame(f3)
  f3_gg  <- tk2frame(f3)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f3_pos_lab <- bs_label_b(parent = f3_pos, text = "Size and position of plot:")

  f3_location_type <-
    bs_combobox(
      parent = f3_pos,
      # label  = "Purpose of code:",
      # label_position = "above",
      width  = 18,
      value  = initial$location_type,

      values =  c("Full size", "Center", "Left side", "Right side", "Custom..."),
      tip = str_c(sep = "\n", "Plot's position on a slide. "),
      on_select = activate_location
    )



  bs_size_entry <- purrr::partial(
    bs_entry, parent = f3_pos, width = 4, justify = "center",
    label_color = "black", tip = str_c(
      "Size / Position in inches.\n",
      "Usually full width of 'PowerPoint' is \n",
      "10 inches and full height is 8 inches."),
    validate = "focus",
    validatecommand = validate_numeric)

  f3_left   <- bs_size_entry(label = "Left side position", value = initial$pos_left,
    invalidcommand  = make_red_text_reset_val(to = 0))

  f3_top    <- bs_size_entry(label = "Top side position",  value = initial$pos_top,
    invalidcommand  = make_red_text_reset_val(to = 0))

  f3_width  <- bs_size_entry(label = "Width",              value = initial$pos_width,
    invalidcommand  = make_red_text_reset_val(to = 8))

  f3_height <- bs_size_entry(label = "Height",             value = initial$pos_height,
    invalidcommand  = make_red_text_reset_val(to = 5))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f3_source_of_plot <- bs_radiobuttons(
    parent = f3_but,
    title = "Source of plot:",
    value = initial$source_of_plot,
    buttons = c(
      last_gg    = "Last ggplot2 plot",
      obj_gg     = "'ggplot2' object",
      code       = "R code of plot"
    ),

    tips = list(
      code = "An 'R' code of a plot.",
      obj_gg     = str_c(
        "A 'ggplot2' object saved in 'R' workspace.\n",
        "This option is inactive if no 'ggplot2' \n",
        "objects are present."),
      last_gg    = str_c(
        "The last created 'ggplot2' plot.\n",
        "This option is inactive if no `ggplot`\n",
        "was created in this session.")
    ),

    default_command = activate_options)


  f3_code_options <-
    bs_combobox(
      parent = f3_but,
      label  = "Desired result:",
      label_position = "above",
      width  = 20,
      value  = initial$code_options,
      values = c(
        "Base plot / Code as-is",
        "'ggplot2' plot",
        "Other plot via plot()",
        "Other plot via print()"
      ),
      tip = str_c(
        sep = "\n",
        "Additional modification for the code. ",
        "You should try, which one works best  ",
        "(or works at all) for your plot:",
        "  - Code as-is - no modification.",
        "  - plot()  - additionally calls function `plot()`.",
        "  - print() - additionally calls function `print()`.",
        "  - ggplot2 - best for 'ggplot2' polots. "
      ))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f3_gg_obj_name_box <-
    bs_listbox(
      parent = f3_gg,
      values = list_objects_of_class("gg", envir = .GlobalEnv),
      title  = "List of ggplot2 objects:",
      width  = 25, height = 7)

  # F4, Frame 4, Preview ---------------------------------------------------
  # F4 <- tk2labelframe(top, relief = "flat", text = "Code input")

  f4 <- tk2frame(top)

  f4_code_input <- bs_text(
    f4, width = 80, height = 13, wrap = "none",
    # autoseparators = TRUE,
    undo = TRUE,
    state = "normal",
    font = font_consolas_regular,
    tip = str_c(
      "Code that generates a plot.\n",
      "Right-click to clear or paste the code."),
    label = "R Code of Plot to Export",
    context_menu = TRUE
  )

  # Widgets ================================================================

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(file_msg, sticky = "")
  # F1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f1, padx = 10, sticky = "we")

  tkgrid(f1_lab_file, f1_ent_file$frame, f1_but_set_1, pady = c(5, 5), sticky = "we")
  tkgrid(f1_but_f_choose, f1_but_paste, f1_but_clear, f1_but_update,   sticky = "e")

  tkgrid.configure(f1_lab_file,       sticky = "e")
  tkgrid.configure(f1_ent_file$frame, sticky = "we", padx = 2)
  tkgrid.configure(
    f1_ent_file$frame_text,
    f1_ent_file$obj_text,
    sticky = "we")

  # F3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f3,     sticky = "nw")
  tkgrid(f3_pos, f3_but, f3_gg, sticky = "nw", padx = c(7, 0))

  tkgrid(f3_pos_lab, sticky = "w")
  tkgrid(f3_location_type$frame,   padx = c(0, 10), sticky = "e", pady = c(0, 2))
  tkgrid(f3_left$frame,   padx = c(0, 10), sticky = "e", pady = c(0, 2))
  tkgrid(f3_top$frame,    padx = c(0, 10), sticky = "e", pady = c(0, 2))
  tkgrid(f3_width$frame,  padx = c(0, 10), sticky = "e", pady = c(0, 2))
  tkgrid(f3_height$frame, padx = c(0, 10), sticky = "e", pady = c(0, 2))

  tkgrid(f3_source_of_plot$frame, padx = 15)
  tkgrid(f3_code_options$frame,   padx = 15)

  tkgrid(f3_gg_obj_name_box$frame, sticky = "n")

  # F4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f4_code_input$frame, sticky = "news")
  tkgrid(f4, pady = c(10, 2))

  # F2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tkgrid(f2, padx = 10, sticky = "e")
  tkgrid(f2_open_file_box$frame,  pady = c(0, 8), sticky = "e")
  tkgrid.configure(f2_open_file_box$frame, sticky = "e", padx = c(15, 0))


  # Help menus -------------------------------------------------------------
  help_menu <- function() {

    menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tkadd(menu_main, "command",
      label    = "Read or create a 'PowerPoint' file",
      command  = open_help("read_pptx", package = "officer"))

    tkadd(menu_main, "command",
      label    = "Add a 'PowerPoint' slide",
      command  = open_help("add_slide", package = "officer"))

    tkadd(menu_main, "command",
      label    = "Add an editable plot into a 'PowerPoint' slide",
      command  = open_help("ph_with_vg_at", package = "rvg"))

    tkadd(menu_main, "command",
      label    = "Open file from 'R'",
      command  = open_help("browseURL", package = "utils"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkpopup(menu_main,
      tkwinfo("pointerx", top),
      tkwinfo("pointery", top))
  }


  # Finalize ---------------------------------------------------------------

  ok_cancel_help(
    # helpSubject = "ph_with_vg_at", helpPackage = "rvg",
    on_help = help_menu,
    close_on_ok = TRUE,
    reset_location = TRUE,
    reset = "window_export_fig_to_pptx()",
    apply = "window_export_fig_to_pptx()",
    after_apply_fun =  function() {
      set_check_pptx_msg()
      activate_options()
    },

    after_apply_success_fun = function() {
      tk_messageBox(
        parent = top,
        message = "The plot was saved.",
        caption = "Done",
        type = "ok",
        icon = "info")
    },

    ok_label = "Save")

  tkgrid(buttonsFrame, sticky = "ew")
  dialogSuffix(bindReturn = FALSE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply initial configuration functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  activate_options()
  activate_location()
  set_check_pptx_msg()

  # Paste code, if possible
  code_clip <- read_clipboard()
  code_clip <- try(style_cmd(code_clip), silent = TRUE)
  if (!is_try_error(code_clip)) {
    set_values(f4_code_input, code_clip)
  }
}
