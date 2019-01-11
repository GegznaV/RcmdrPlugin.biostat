# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Helper-functions
#' @export
#' @keywords internal
set_biostat_mode <- function() {
    # Change buttons
    tkconfigure(getRcmdr("dataSetLabel"),
                # foreground = "darkred",
                image = "::image::bs_dataset",
                compound = "left",
                command = window_dataset_select)


    tkconfigure(getRcmdr("modelLabel"),
                # foreground = "darkred",
                image = "::image::bs_model",
                compound = "left",
                command = window_model_select)


    # Add tooltips
    tk2tip(getRcmdr("dataSetLabel"), "Active dataset")
    tk2tip(getRcmdr("modelLabel"),   "Active model")


    # Change title and main icon
    .rcmdr <- CommanderWindow()
    tkwm.title(.rcmdr, str_c(gettextRcmdr("R Commander"), " (BioStat mode)"))
    tcl("wm", "iconphoto", .rcmdr, "-default", "::image::bs_green_r_72")

    # Change icon nad button layout
    pare <- tcl_get_parent(getRcmdr("dataSetLabel"))

    # New buttons
    b_dir <- tk2button(
        pare,
        tip = "Open working directory",
        image = "::image::bs_open_dir",
        command = command_openwd)

    b_in <- tk2button(
        pare,
        tip = "Import dataset",
        image = "::image::bs_import",
        command = command_dataset_refresh)

    b_out <- tk2button(
        pare,
        tip = "Export dataset",
        image = "::image::bs_export",
        command = command_dataset_refresh)

    b_print <- tk2button(
        pare,
        tip = "Print dataset to console",
        image = "::image::bs_to_console",
        command = command_dataset_refresh)

    b_refresh <- tk2button(
        pare,
        tip = "Refresh",
        image = "::image::bs_refresh",
        command = command_dataset_refresh)

    # b_plots <- tk2button(
    #     pare,
    #     tip = "Plots",
    #     image = "::image::bs_plot",
    #     command = command_dataset_refresh)
    #
    # b_analysis <- tk2button(
    #     pare,
    #     tip = "Analysis",
    #     image = "::image::bs_analyse",
    #     command = command_dataset_refresh)
    #
    # b_summary <- tk2button(
    #     pare,
    #     tip = "Summary",
    #     image = "::image::bs_summary",
    #     command = command_dataset_refresh)


    sibl <- tcl_get_siblings(getRcmdr("dataSetLabel"))
    img  <- map_chr(sibl, ~tcltk::tclvalue(tkcget(.x, "-image")))
    txt  <- map_chr(sibl, ~tcltk::tclvalue(tkcget(.x, "-text")))

    logo      <- sibl[img %in% c("::image::RlogoIcon", "::image::bs_green_r_24")]
    but_data  <- sibl[img %in% c("::image::dataIcon",  "::image::bs_dataset")]
    but_model <- sibl[img %in% c("::image::modelIcon", "::image::bs_model")]
    lab_data  <- sibl[txt == gettextRcmdr("   Data set:")]
    lab_model <- sibl[txt == gettextRcmdr("Model:")]
    but_edit  <- sibl[img == "::image::editIcon"]
    but_view  <- sibl[img == "::image::viewIcon"]



    # Remove old properties
    tkgrid.forget(logo, lab_data, but_data, lab_model, but_model)
    tkconfigure(but_edit, compound = "none")
    tkconfigure(but_view, compound = "none")


    if (length(logo) > 0) {
        tkconfigure(logo, image = "::image::bs_green_r_24")
    }

    if (length(but_edit) > 0) {
        tkgrid.forget(but_edit)
        # Add tooltip
        .Tcl(str_glue('tooltip::tooltip {but_edit} "Edit dataset"'))
    }

    if (length(but_view) > 0) {
        tkgrid.forget(but_view)
        # Add tooltip
        .Tcl(str_glue('tooltip::tooltip {but_view} "Preview dataset"'))
    }

    # tkcget(but_edit, "-command")


    # New layout
    tkgrid(logo,
           lab_data,
           but_data,
           b_dir, b_in, b_out, b_print,
           but_view, but_edit,
           # b_summary, b_plots, b_analysis,
           b_refresh,
           lab_model, but_model)

    tkgrid.configure(pare, pady = c(4, 3))

    tkgrid.configure(but_data,  padx = c(2, 10))
    tkgrid.configure(lab_model, padx = c(10, 2))
    tkgrid.configure(but_model, padx = c(0,  2))

}
