#' @name Menu-window-functions
#' @title RcmdrPlugin.biostat functions for menus and windows.
#' @description Functions that open Rcmdr menus and windows windows.
#' @keywords internal
NULL


# "Data" menu related functions ==============================================

# Manage dataset -------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_refresh <- function() {
    command_dataset_refresh_0()
}

command_dataset_refresh_0 <- function(...) {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    .ds <- active_dataset_0()

    if (isTRUE(!.ds %in% ls(envir = .GlobalEnv))) {
        # If ds is deleted
        active_dataset_0(NULL)

    } else if (!is.null(.ds)) {
        # If ds is present
        active_dataset(.ds, ...)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    activate_menus()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
activate_menus <- function() {
    Rcmdr::activateMenus()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Activate buttons in BioStat mode
    if (is_biostat_mode()) {

        if (!is.null(active_dataset_0())) {
            tk_normalize(getRcmdr("button_export"))
            tk_normalize(getRcmdr("button_rows"))
            tk_normalize(getRcmdr("button_variables"))
            tk_normalize(getRcmdr("button_view"))
            tk_normalize(getRcmdr("button_summary"))
            tk_normalize(getRcmdr("button_analysis"))

        } else {
            tk_disable(getRcmdr("button_export"))
            tk_disable(getRcmdr("button_rows"))
            tk_disable(getRcmdr("button_variables"))
            tk_disable(getRcmdr("button_view"))
            tk_disable(getRcmdr("button_summary"))
            tk_disable(getRcmdr("button_analysis"))
        }
    }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_view <- function() {
    justDoIt(str_glue("View({active_dataset_0()})"))
    logger(str_glue("## View({active_dataset_0()})"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_print <- function() {
    doItAndPrint(str_glue("print({active_dataset_0()})"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_print_as_df <- function() {
    .ds <- active_dataset_0()
    doItAndPrint(str_glue("print(as.data.frame({.ds}))"))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_print_as_dt <- function() {
    .ds <- active_dataset_0()
    ds <- eval_text(.ds, envir = .GlobalEnv)

    Library("data.table")

    if (tibble::has_rownames(ds)) {
        rn <- unique_colnames("rownames")
        command <- str_glue('print(as.data.table({.ds}, keep.rownames = "{rn}"))')

    } else {
        command <- str_glue("print(as.data.table({.ds}))")
    }
    doItAndPrint(command)
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_print_as_tibble <- function() {
    .ds <- active_dataset_0()
    Library("tibble")
    doItAndPrint(str_glue('print(as_tibble({.ds}))'))
}


# Dataset's class ------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_class <- function() {
    doItAndPrint(str_glue("## The R class of the dataset\n",
                          "class({active_dataset_0()})"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_as_df <- function() {
    .ds <- active_dataset_0()
    doItAndPrint(str_glue(
        "## Change class of the dataset to `data.frame`\n",
        "{.ds} <- as.data.frame({.ds})"))
    command_dataset_refresh()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_as_tibble <- function() {
    .ds <- active_dataset_0()

    Library("tibble")
    doItAndPrint(str_glue(
        "## Change class of the dataset to `tibble`\n",
        "{.ds} <- as_tibble({.ds})"))
    command_dataset_refresh()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_as_dt <- function() {
    .ds <- active_dataset_0()

    Library("data.table")
    doItAndPrint(str_glue(
        "## Change class of the dataset to `data.table`\n",
        "{.ds} <- data.table({.ds})"))
    command_dataset_refresh()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
