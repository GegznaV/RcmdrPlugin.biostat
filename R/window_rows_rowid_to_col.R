# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal

# This row deletes row names:
# # new_df <- tibble::rowid_to_column({active_dataset_0()}, var = "rows_id")

window_rows_rowid_to_col <- function() {
    # Initialize -------------------------------------------------------------
    defaults <- list(initial_position = "first")
    initial  <- getDialog("window_rows_rowid_to_col", defaults)

    initializeDialog(title = gettext_bs("Create Row Numbers"))
    tk_title(top, gettext_bs("Create Column with Row Numbers"))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    .ds    <- active_dataset()
    fg_col <- Rcmdr::getRcmdr("title.color")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    upper_frame <- tkframe(top)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    name_variable <- tclVar(unique_colnames("row_number"))
    name_frame    <- tkframe(upper_frame)
    name_entry    <- ttkentry(name_frame, width = "28",
                              textvariable = name_variable)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rb_frame <- tkframe(upper_frame)
    radioButtons_horizontal(
        rb_frame,
        title = "Column position: ",
        title.color = fg_col,

        # right.buttons = FALSE,
        name = "position",
        sticky_buttons = "w",
        buttons = c("first",  "last"),
        values =  c("first",  "last"),
        labels =  c("First  ","Last  "),
        initialValue = initial$initial_position
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    onOK <- function() {

        # Get values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        new_name       <- trim.blanks(tclvalue(name_variable))
        which_position <- tclvalue(positionVariable)

        # Reset properties ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tkconfigure(name_entry, foreground = "black")

        # Check values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (is_empty_name(new_name)) {
            return()
        }

        if (is_not_valid_name(new_name)) {
            tkconfigure(name_entry, foreground = "red")
            return()
        }

        if (forbid_to_replace_variable(new_name)) {
            tkconfigure(name_entry, foreground = "red")
            return()
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Save default values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        putDialog("window_rows_rowid_to_col",
                  list(initial_position = which_position))
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        closeDialog()

        # Construct commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Library("tidyverse")

        cmd_position <-
            switch(which_position,
                   "first" = str_glue(
                       "%>% \n dplyr::select({new_name}, everything())"),
                   "last" = "")

        ds <- get(.ds, envir = globalenv())
        cmd_ungroup <- if (is_grouped_df(ds)) "ungroup() %>% \n" else ""

        command <- style_cmd(str_glue(
            '## Add column with row numbers \n',
            "{.ds} <- {.ds} %>% \n",
            "{cmd_ungroup}",
            "dplyr::mutate({new_name} = 1:n())",
            "{cmd_position}"))

        # Do commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        doItAndPrint(command)
        command_dataset_refresh()

        tkfocus(CommanderWindow())
    }

    # Help menus -------------------------------------------------------------
    help_menu <- function() {

      menu_main <- tk2menu(tk2menu(top), tearoff = FALSE)
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tkadd(menu_main, "command",
        label    = "Function `mutate`",
        command  = open_help("mutate", package = "dplyr"))

      tkadd(menu_main, "command",
        label    = "Function `rowid_to_column`",
        command  = open_help("rowid_to_column", package = "tibble"))

      tkadd(menu_main, "separator")

      tkadd(menu_main, "command",
        label    = "Function `ungroup`",
        command  = open_help("ungroup", package = "dplyr"))

      tkadd(menu_main, "command",
        label    = "Function `select`",
        command  = open_help("select", package = "dplyr"))

      tkadd(menu_main, "command",
        label    = "Function `everything`",
        command  = open_help("everything", package = "dplyr"))
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tkpopup(menu_main,
        tkwinfo("pointerx", top),
        tkwinfo("pointery", top))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ok_cancel_help(on_help = help_menu)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(upper_frame, pady = c(0, 5))
    tkgrid(name_frame, rb_frame, sticky = "w")

    lab_1 <- bs_label_b(name_frame, text = gettext_bs("Column name:"))
    tkgrid(lab_1, sticky = "w")
    tkgrid(name_entry, sticky = "w")
    tkgrid(positionFrame, padx = c(15, 0))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix()
}
