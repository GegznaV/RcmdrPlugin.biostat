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
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    .ds <- Rcmdr::ActiveDataSet()

    if (isTRUE(!.ds %in% ls(envir = .GlobalEnv))) {
        # If ds is deleted
        Rcmdr::ActiveDataSet(NULL)

    } else if (!is.null(.ds)) {
        # If ds is present
        active_dataset(.ds)
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

        if (!is.null(ActiveDataSet())) {
            tk_normalize(getRcmdr("button_export"))
            tk_normalize(getRcmdr("button_view"))

        } else {
            tk_disable(getRcmdr("button_export"))
            tk_disable(getRcmdr("button_view"))
        }
    }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_view <- function() {
    justDoIt(str_glue("View({ActiveDataSet()})"))
    logger(str_glue("## View({ActiveDataSet()})"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_print <- function() {
    doItAndPrint(str_glue("print({ActiveDataSet()})"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_print_as_df <- function() {
    .ds <- ActiveDataSet()
    doItAndPrint(str_glue("print(as.data.frame({.ds}))"))
}

#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_print_as_dt <- function() {
    .ds <- ActiveDataSet()
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
    .ds <- ActiveDataSet()
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
                          "class({ActiveDataSet()})"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_as_df <- function() {
    doItAndPrint(str_glue(
        "## Change class of the dataset to `data.frame`\n",
        "{ActiveDataSet()} <- as.data.frame({ActiveDataSet()})"))
    command_dataset_refresh()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_as_tibble <- function() {
    Library("tibble")
    doItAndPrint(str_glue(
        "## Change class of the dataset to `tibble`\n",
        "{ActiveDataSet()} <- as_tibble({ActiveDataSet()})"))
    command_dataset_refresh()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_dataset_as_dt <- function() {
    Library("data.table")
    doItAndPrint(str_glue(
        "## Change class of the dataset to `data.table`\n",
        "{ActiveDataSet()} <- data.table({ActiveDataSet()})"))
    command_dataset_refresh()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~